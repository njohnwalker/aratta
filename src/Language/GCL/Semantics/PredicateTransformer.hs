module Language.GCL.Semantics.PredicateTransformer
where

import Control.Monad ((>=>))
import Control.Monad.Reader hiding ( guard )
import Control.Monad.State hiding ( guard )
import Control.Lens ( transform, (^?), over )

import           Data.Data
import           Data.List ( break )
import qualified Data.Set as Set
import           Data.Text ( Text )
import           Data.Text.Prettyprint.Doc

import           GHC.Generics

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

type BasicPath = Path BExp BasicInstruction

data BasicInstruction
  = Assume BExp
  | Substitute [Variable] [IExp]
  | BranchInstruction [BasicPath]
  deriving (Eq, Ord, Show, Data, Generic)

data Path post ins
  = ins ::: Path post ins
  | Postcondition post
  | Hole
  deriving (Eq, Ord, Show, Data, Generic)
infixr 5 :::

fromPostAndList :: post -> [ins] -> Path post ins
fromPostAndList = foldr (:::) . Postcondition 

getBasicPathVCs :: GCLProgram -> ParameterizedInvariant [BExp]
getBasicPathVCs = (fmap.fmap) (either ($ BConst True) id . wpSeq) . getBasicPath

getBasicPath :: GCLProgram -> ParameterizedInvariant [BasicPath]
getBasicPath GCLProgram {req, program, ens} =
  let post = maybe (BConst True) id ens
      kstartingPath = maybe id ((:::) . Assume) req
  in clipPaths (Just post) kstartingPath program

oPath
  :: (Path post ins -> Path post ins)
  ->  ins
  ->  Path post ins -> Path post ins
oPath kpath ins path = kpath $ ins ::: path

getPaths
  :: BExp
  -> [Statement]
  -> ParameterizedInvariant [BasicPath]
getPaths post path = clipPaths (Just post) id path

clipPaths
  :: Maybe BExp
  -> (BasicPath -> BasicPath)
  -> [Statement]
  -> ParameterizedInvariant [BasicPath]
clipPaths post = clipPaths' 
  where
    clipPaths' :: (BasicPath -> BasicPath) -> [Statement] -> ParameterizedInvariant [BasicPath]
    clipPaths' kpath [] = return [kpath $ maybe Hole Postcondition post]
    clipPaths' kpath (stmt:stmts) = case stmt of
      vs := es -> clipPaths' (kpath `oPath` Substitute vs es) stmts
      If gcs -> do
        kBranchPaths <- ifBranchPaths kpath gcs
        remainingPaths <- clipPaths' id stmts
        let (postIfPath, rest)
              = case remainingPaths of
                  []     -> ([],  [])
                  (p:ps) -> ([p], ps)
        return $ (kBranchPaths <*> postIfPath) ++ rest
      Do mInv gcs -> do
        inv <- maybe ask return mInv
        nextPaths <- clipPaths' id stmts
        internalPaths <- doBranchPaths inv gcs 
        return $ kpath (Postcondition inv) -- cut previous path
          : nextPaths -- paths after loop
          ++ internalPaths -- loop interior paths

ifBranchPaths
  :: (BasicPath -> BasicPath)
  -> GuardedCommandSet
  -> ParameterizedInvariant [BasicPath -> BasicPath]
ifBranchPaths kpath (GCS gcs) = do
  branchPaths <- forM gcs
    $ \GC {guard, command} ->
        clipPaths Nothing (kpath `oPath` Assume guard) command
  return
    [ (BranchInstruction branchPath :::)
    | branchPath <-
        sequence branchPaths
    ]

doBranchPaths
  :: BExp -- ^ invariant
  -> GuardedCommandSet
  -> ParameterizedInvariant [BasicPath]
doBranchPaths inv (GCS gcs) = do
  branchPaths <- forM gcs
    $ \GC {guard, command} ->
        clipPaths (Just inv) (Assume guard :::) command
  return [ Assume inv
           ::: BranchInstruction branchPath
           ::: Postcondition (BConst True) -- is this a hack? or okay
         | branchPath <- sequence branchPaths
         ]

-- | calculate the wp of a basic path
wpSeq :: BasicPath -> Either (BExp -> BExp) BExp
wpSeq = foldPath wp id

foldPath
  :: (ins -> post -> post)
  -> (post -> post')
  -> Path post ins
  -> Either (post -> post') post'
foldPath pt kpost = \case
  Hole -> Left kpost
  Postcondition post -> Right $ kpost post
  ins ::: path -> foldPath pt (kpost . pt ins) path

-- | calculate the weakest pre of a single basic instruction
wp :: BasicInstruction -> BExp -> BExp
wp path post = case path of
  Assume p -> (p :=>: post)
  Substitute vs es -> substitute (zip vs es) post
  BranchInstruction [] -> post
  BranchInstruction branchPaths ->
    foldl1 (:&:)
    $ map (either ($ post) id . wpSeq)
    branchPaths

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

--------------------------
-- Printing basic paths --
instance Pretty BasicInstruction where
  pretty = \case
    Assume p -> "Assume" <+> pretty p
    Substitute vs es ->
      "Substitute" <> line
      <> indent 2 (vsep $ map pretty $ zip vs es)
    BranchInstruction branches ->
      "Branch" <> line
      <> indent 2 (vsep $ map pretty branches)

instance Pretty BasicPath where
  pretty = \case
    Hole -> "{ HOLE }"
    Postcondition p -> "{ Satisfies" <+> pretty p <+> "}"
    ins ::: path -> ";" <+> pretty ins <> line
                    <> pretty path

  prettyList paths =
     vsep $ zipWith
     (\i path -> "Path" <+> pretty i <> line
                 <> indent 2 (pretty path)) 
     [1::Int ..] paths
