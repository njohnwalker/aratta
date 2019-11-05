module Language.GCL.Semantics.PredicateTransformer
where

import Control.Monad ((>=>))
import Control.Monad.Reader hiding ( guard )
import Control.Monad.State hiding ( guard )
import Control.Lens ( transform, (^?), over )

import Data.List ( break )
import Data.Text ( Text )
import qualified Data.Set as Set

import qualified SimpleSMT as SMT
import           Language.GCL.Environment
import qualified Language.GCL.SMTLib as GCL.SMT
import           Language.GCL.Syntax.Abstract

-------------------
-- VC Validation --
data Validity = Valid | Invalid BExp
  deriving (Eq, Ord, Show)

checkValidVCs
  :: SMT.Solver -- Solver MVar for SMT
  -> Set.Set Variable -- Program closure
  -> BExp -- Candidate invariant
  -> ParameterizedInvariant [BExp] -- Program invariants
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
    smtResultsToValidity ((vc, SMT.Unknown): _) = Invalid $ Var_ "unknown"
                                                  :<=: Var_ "result on" :&: vc
    smtResultsToValidity (( _, SMT.Unsat  ):rs) = smtResultsToValidity rs

-------------------
-- VC Generation --
type ParameterizedInvariant = Reader BExp

specifyInvariant :: BExp -> ParameterizedInvariant a -> a
specifyInvariant = flip runReader

data BasicInstruction
  = Assume BExp
  | Substitute [Variable] [IExp]


data Path ins post
  = Path ins post ::: ins
  | Postcondition post

data PathTree ins post
  = PTree ins (Path (PathTree ins post) post)

type GCLBasicPath = Path BasicInstruction BExp
type GCLBasicPathTree = PathTree

getPathTree :: GCLProgram -> ParameterizedInvariant (PathTree BasicInstruction BExp)
getPathTree GCLProgram {req, program, ens} =
  let post = maybe (BConst True) id ens
      rootF = maybe id (\pre tree -> PTree (Assume pre) (Postcondition post ::: tree)) req 
  in rootF <$> getPathTree' post program
  where
    getPathTree'
      :: BExp
      -> [Statement]
      -> ParameterizedInvariant (PathTree BasicInstruction BExp)
    getPathTree' post = undefined -- \case
      -- vs := es : stmts -> PTree ()

-- | calculate the wp of a basic path
--   expects path in reverse (stack)
wpSeq :: [BasicInstruction] -> BExp -> BExp
wpSeq = foldl (\f ins -> wp ins . f) id

-- | calculate the weakest pre of a single basic instruction
wp :: BasicInstruction -> BExp -> BExp
wp path post = case path of
  Assume p -> (p :=>: post)
  Substitute vs es -> substitute (zip vs es) post
  -- BranchPath fPairs -> case fPairs post of
  --   [] -> post
  --   pairs -> foldl1 (:&:) $ map (uncurry wpSeq) pairs

-------------
-- helpers --
substitute :: [(Variable,IExp)] -> BExp -> BExp
substitute sub = transform match
  where
    match = \case
      i1 :<=: i2 -> substituteIExp sub i1 :<=: substituteIExp sub i2
      e' -> e'

substituteIExp :: [(Variable, IExp)] -> IExp ->IExp
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
