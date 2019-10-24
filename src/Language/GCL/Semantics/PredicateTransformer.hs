module Language.GCL.Semantics.PredicateTransformer
where

import Control.Monad ((>=>))
import Control.Monad.Reader hiding ( guard )
import Control.Lens ( transform, (^?), over )
import Data.Text ( Text )
import qualified Data.Set as Set

import qualified SimpleSMT as SMT
import           Language.GCL.Environment
import qualified Language.GCL.SMTLib as GCL.SMT
import           Language.GCL.Syntax.Abstract

data BasicPath
  = Assume BExp
  | Substitute [Text] [IExp]

type ParameterizedInvariant = ReaderT BExp []
instance Semigroup (ParameterizedInvariant a) where
  (<>) (ReaderT l1) (ReaderT l2) = ReaderT $ \r -> l1 r <> l2 r

specifyInvariant :: BExp -> ParameterizedInvariant a -> [a]
specifyInvariant = flip runReaderT

-------------------
-- VC Validation --
data Validity = Valid | Invalid BExp
  deriving (Eq, Ord, Show)

checkValidVCs
  :: SMT.Solver -- Solver MVar for SMT
  -> Set.Set Text -- Program closure
  -> BExp -- Candidate invariant
  -> ParameterizedInvariant BExp -- Program invariants
  -> IO Validity
checkValidVCs solver closure inv pVCs =
  let vcs = specifyInvariant inv pVCs
      fullClosure = Set.union closure $ getClosureBool inv
  in SMT.inNewScope solver $ do
    GCL.SMT.declareHeader solver fullClosure
    SMT.push solver
    results <- forM vcs $ \vc -> do
      SMT.push solver
      GCL.SMT.boolToSMTAssertion solver $ Not vc
      res <- SMT.check solver
      SMT.pop solver
      return (vc, res)
    return $ smtResultsToValidity results
  where
    smtResultsToValidity :: [(BExp, SMT.Result)] -> Validity
    smtResultsToValidity []                     =   Valid
    smtResultsToValidity ((vc, SMT.Sat    ): _) = Invalid vc
    smtResultsToValidity ((vc, SMT.Unknown): _) = Invalid $ Var "unknown" :<=: Var "result on" :&: vc
    smtResultsToValidity (( _, SMT.Unsat  ):rs) = smtResultsToValidity rs

-------------------
-- VC Generation --
-- | Generate all VCs from all basic paths of a program
getBasicPathVCs :: GCLProgram -> ParameterizedInvariant BExp
getBasicPathVCs GCLProgram {req , program, ens} =
 let path = maybe [] ((:[]) . Assume) req
     post = maybe (BConst True) id ens
     stmts = case program of Seq stmts -> stmts ; stmt -> [stmt]
  in uncurry wpSeq <$> getPaths post stmts path

-- | Generate all basic paths and postcondition pairs, parameterized by invariant
getPaths :: BExp -> [Statement] -> [BasicPath] -> ParameterizedInvariant ([BasicPath],BExp)
getPaths post [] path = return (path,post)
getPaths post (stmt:stmts) path
  = case stmt of
      vars := exps -> getPaths post stmts $ path ++ [Substitute vars exps]
      If gcs ->
        getPaths post stmts (path ++ [Assume $ negateGuards gcs]) -- TODO: maybe change back to break on unsat if
        <> getPathsGCS post gcs path
      Do mInv gcs -> do
        inv <- case mInv of Nothing -> ask ; Just i -> return i
        return (path, inv) -- cut on previous path
          <> getPaths post stmts [Assume inv, Assume $ negateGuards gcs] -- post loop path
          <> getPathsGCS inv gcs [Assume inv] -- loop branch paths
      Seq stmts' ->
        -- not convinced this is possible
        getPaths post (stmts' ++ stmts) path
  where
    getPathsGCS :: BExp -> GuardedCommandSet -> [BasicPath] -> ParameterizedInvariant ([BasicPath],BExp)
    getPathsGCS post gcs path = do
      GC {guard, statement} <- lift $ getCommandList gcs
      let stmts' = case statement of Seq stmts' -> stmts' ; stmt' -> [stmt']
      getPaths post (stmts' ++ stmts) (path ++ [(Assume guard)])

-- | calculate the wp of a basic path
wpSeq :: [BasicPath] -> BExp -> BExp
wpSeq = foldl (\f ins -> f . wp ins) id

-- | calculate the weakest pre of a single basic instruction
wp :: BasicPath -> BExp -> BExp
wp = \case
  Assume p -> (p :=>:)
  Substitute vs es -> substitute $ zip vs es

-------------
-- helpers --
substitute :: [(Text,IExp)] -> BExp -> BExp
substitute sub = transform match
  where
    match = \case
      i1 :<=: i2 -> substituteIExp sub i1 :<=: substituteIExp sub i2
      e' -> e'

substituteIExp :: [(Text,IExp)] -> IExp ->IExp
substituteIExp sub = transform match
  where
    match = \case
      Var v' -> maybe (Var v') id $ lookup v' sub
      e' -> e'

negateGuards :: GuardedCommandSet -> BExp
negateGuards (GCS []) = BConst True
negateGuards (GCS xs)
  = foldl1 (:&:)
  $ map (Not . guard) xs
