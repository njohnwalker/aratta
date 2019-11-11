module Language.GCL.Semantics.PredicateTransformer
where

import Control.Monad ((>=>))
import Control.Monad.Reader hiding ( guard )
import Control.Monad.State hiding ( guard )
import Control.Lens ( transform, (^?), over )

import           Data.Data
import           Data.Data.Lens ( biplate )
import           Data.List ( find, break )
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
data Validity = Valid | Invalid BExp Env
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
    results <- forM vcs \vc -> do
      SMT.push solver
      GCL.SMT.boolToSMTAssertion solver $ Not vc
      smtRes <- SMT.check solver
      validityRes <- case smtRes of
        SMT.Sat ->
          Invalid vc <$> GCL.SMT.getModel solver
                   (Set.map getVariableText fullClosure)         
        SMT.Unsat -> return Valid
        SMT.Unknown -> return $ Invalid
          ( Var "unknown" :==: Var "result on" :=>: vc)
          mempty
      SMT.pop solver
      return validityRes
    return $ maybe Valid id
      $ find \case Valid -> False ; _ -> True
      results
    
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
        kBranchPaths <- branchPaths kpath gcs
        remainingPaths <- clipPaths' id stmts
        let (postIfPath, rest)
              = case remainingPaths of
                  []     -> ([],  [])
                  (p:ps) -> ([p], ps)
        return $ (kBranchPaths <*> postIfPath) ++ rest

      Do mInv gcs -> do
        inv <- maybe ask return mInv
        nextPaths <- clipPaths'
          ((Assume inv :::)
           `oPath` (Assume (negateGuards gcs)))
          stmts
        kInternalPaths <- branchPaths id gcs
        return $ kpath (Postcondition inv) -- cut previous path
          : [ (Assume inv :::)
               $ kInternalPath
               $ Postcondition inv
             | kInternalPath <- kInternalPaths
             ] -- loop interior paths
          ++ nextPaths -- paths after loop


branchPaths
  :: (BasicPath -> BasicPath)
  -> GuardedCommandSet
  -> ParameterizedInvariant [BasicPath -> BasicPath]
branchPaths kpath (GCS gcs) = do
  branchPaths <- forM gcs
    $ \GC {guard, command} ->
        clipPaths Nothing (kpath `oPath` Assume guard) command
  return
    [ (BranchInstruction branchPath :::)
    | branchPath <-
        sequence branchPaths
    ]

-- | calculate the wp of a basic path
wpSeq :: BasicPath -> Either (BExp -> BExp) BExp
wpSeq = foldPath wp id

foldPath
  :: (ins -> post -> post)
  -> (post -> post)
  -> Path post ins
  -> Either (post -> post) post
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
substitute sub = over biplate $ substituteIExp sub

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
      align
      $ "Substitute" <> line
      <> indent 2 (vsep $ zipWith prettySub vs es)
      where
        prettySub v e = "@" <> pretty v <+> "->"
                        <+> pretty e

    BranchInstruction branches ->
      vsep $ zipWith
      (\i path -> "Branch" <+> pretty i <> line
                  <> indent 2 (pretty path)) 
      [1::Int ..] branches

instance Pretty BasicPath where
  pretty = \case
    Hole -> "{ HOLE }"
    Postcondition p -> "{ Satisfies" <+> pretty p <+> "}"

    b@(BranchInstruction _) ::: path ->
      "[" <+> hang 0 (pretty b) <> line
      <> "]" <> line
      <> pretty path

    ins ::: path -> ";" <+> pretty ins <> line
                    <> pretty path

  prettyList paths =
     vsep $ zipWith
     (\i path -> "Path" <+> pretty i <> line
                 <> indent 2 (pretty path)) 
     [1::Int ..] paths
