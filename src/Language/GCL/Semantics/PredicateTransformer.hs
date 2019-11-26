module Language.GCL.Semantics.PredicateTransformer
where

import Control.Monad ((>=>))
import Control.Monad.Reader hiding ( guard )
import Control.Monad.State hiding ( guard )
import Control.Lens ( transform, (^?), over, set, _1, (<&>) )
import Control.Concurrent
import Control.Concurrent.STM

import           Data.Data
import           Data.Data.Lens ( biplate )
import           Data.Foldable 
import           Data.List ( find, break )
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Stream.Infinite ( Stream(..) )
import           Data.Text ( Text, unpack )
import           Data.Text.Prettyprint.Doc

import           GHC.Generics

import qualified SimpleSMT as SMT
import qualified SMTLib.Environment as SMT
import           Language.GCL.Environment
import qualified Language.GCL.SMTLib as GCL.SMT
import           Language.GCL.Syntax.Abstract

import SemanticModel.PredicateTransformer
import SemanticModel.PredicateTransformer.Validity

type GCLValidity = Validity BExp

instance PredicateTransformer BExp where
  invariantTop = False_

  invariantJoin = (:&:)

  checkValidVCs = checkValidVCs'

-------------------
-- VC Validation --
type ParameterizedInvariant = Reader BExp

specifyInvariant :: BExp -> ParameterizedInvariant a -> a
specifyInvariant = flip runReader

checkValidVCs'
  :: Traversable t
  => t BExp -- Candidate invariant
  -> IO SMT.Solver -- Solver action for SMT
  -> IO GCLValidity
checkValidVCs' vcs newSolver = checkSolverVC $ toList vcs
  where
    checkSolverVC :: [BExp] -> IO GCLValidity
    checkSolverVC [] = return Valid
    checkSolverVC (vc:vcs) = do
      let closure = getClosure vc
      solver <- newSolver
      GCL.SMT.declareHeader solver closure
      GCL.SMT.boolToSMTAssertion solver $ Not vc
      SMT.check solver
        >>= \case
        SMT.Sat -> do
          model <- GCL.SMT.getModel solver closure
          SMT.stop solver
          return $ Invalid vc model
        SMT.Unsat -> do
          SMT.stop solver
          checkSolverVC vcs
        SMT.Unknown -> do
          SMT.stop solver
          return (UnknownValidity vc)

-------------------
-- VC Generation --
data BasicInstruction
  = Assume BExp
  | Substitute [Variable] [IExp]
  deriving (Eq, Ord, Show, Data, Generic)

data BasicPath = BasicPath
  { precondition :: BExp
  , path :: [BasicInstruction]
  , postcondition :: BExp
  } deriving (Eq, Ord, Show, Data, Generic)

getProgramPaths :: GCLProgram -> ParameterizedInvariant [BasicPath]
getProgramPaths gclpgm = getPaths gclpgm []

getPaths
  :: GCLProgram
  -> [BasicInstruction]
  -> ParameterizedInvariant [BasicPath]
getPaths (GCLProgram pre [] post) path = return [BasicPath pre path post]
getPaths (GCLProgram pre (stmt:stmts) post) path =
  case stmt of
    vs := es -> getPaths program' $ path ++ [Substitute vs es]
    If gcs -> getPathsIf gcs program' path
    Do mInv gcs -> do
      inv <- maybe ask return mInv
      loopPaths <- getPathsDo inv gcs
      postLoopPaths <- getPaths (GCLProgram inv stmts post) [Assume $ negateGuards gcs]
      return $ (BasicPath pre path inv) : loopPaths ++ postLoopPaths
  where
    program' = GCLProgram pre stmts post


getPathsIf
  :: GuardedCommandSet
  -> GCLProgram
  -> [BasicInstruction]
  -> ParameterizedInvariant [BasicPath]
getPathsIf (GCS []) gclpgm path = getPaths gclpgm path
getPathsIf (GCS gcs) (GCLProgram pre stmts post) path =
  concat <$> forM branches
  \branch ->
    let (guards, stmtss) = unzip branch
        stmts' = concat stmtss ++ stmts
        assumptions = gclAnd guards 
    in getPaths (GCLProgram pre stmts' post) (path ++ [Assume assumptions])
  where
    branches = sequence
      [ [ (guard, command), (Not guard, []) ]
      | GC{guard, command} <- gcs
      ]

getPathsDo
  :: BExp
  -> GuardedCommandSet
  -> ParameterizedInvariant [BasicPath]
getPathsDo inv (GCS []) = return [] -- no loop paths
getPathsDo inv (GCS gcs) =
  concat <$> forM branches
  \branch ->
    let (guards, stmtss) = unzip branch
        stmts = concat stmtss
        assumptions = gclAnd guards
    in getPaths (GCLProgram inv stmts inv) [Assume assumptions]
  where
    branches = init $ -- remove all negative path (handled elsewhere)
               sequence -- cartesian product
               [ [ (guard, command), (Not guard, []) ]
               | GC{guard, command} <- gcs ]

-- | Calculate the weakest pre of a list of basicinstructions
wpPath :: BasicPath -> BExp
wpPath BasicPath { precondition, path, postcondition}
  = foldr wp postcondition path'
  where
    path' = case precondition of
      True_ -> path
      _ -> Assume precondition : path

-- | Calculate the weakest pre of a single basic instruction
wp :: BasicInstruction -> BExp -> BExp
wp ins post = case ins of
  Assume p -> (p :=>: post)
  Substitute vs es -> substitute (zip vs es) post

-- | Calculate the strongest post of a basic path
spPath :: BasicPath -> BExp
spPath bPath@BasicPath { precondition, path, postcondition }
  = (:=>: postcondition)
    $ flip evalState
    (initialFVEnv $ getClosure bPath)
    $ foldl (\mPre ins -> mPre >>= sp ins)
    (pure precondition)
    path 

-- | Calculate the strongest post of a single basic instruction
sp :: BasicInstruction -> BExp -> FVState BExp
sp ins pre = case ins of
  Assume p -> return $ p :&: pre
  Substitute vs es -> do
    freshVs <- mapM fresh vs
    let subs = zip vs (map Var freshVs)
        es' = substitute subs es
    return $ gclAnd
           $ zipWith (\v e' -> Var v :==: e') vs es'
           ++ [substitute subs pre]

-------------
-- helpers --
substitute :: Data a => [(Variable,IExp)] -> a -> a
substitute = over biplate . substituteIExp

substituteIExp :: [(Variable, IExp)] -> IExp ->IExp
substituteIExp sub = transform match
  where
    match = \case
      Var v' -> maybe (Var v') id $ lookup v' sub
      e' -> e'

negateGuards :: GuardedCommandSet -> BExp
negateGuards (GCS gcs) = gclAnd $ map (Not . guard) gcs

gclAnd :: [BExp] -> BExp
gclAnd [] = True_
gclAnd bs = foldl1 (:&:) bs

---------------------------------------------
-- Fresh Variable Monad for Strongest Post --
type FVState = State FVEnv

fresh :: Variable -> FVState Variable
fresh var@(Variable varT) = do
  (freshVar :> rest) <- maybe (error $ "Variable: "
                               ++ unpack varT
                               ++ " not in program closure"
                              ) id <$> gets (Map.lookup var)
  modify (Map.adjust (const rest) var)
  return freshVar 
  
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

  prettyList = vsep .  map prettyIns
    where
      prettyIns ins = ";" <+> pretty ins

instance Pretty BasicPath where
  pretty BasicPath {precondition, path, postcondition} =
    "{ PRE:" <+> pretty precondition <+> "}" <> line 
    <> prettyList path <> line
    <> "{ POST:" <+> pretty postcondition <+> "}"

  prettyList = vsep . zipWith prettyPath [1::Int ..]
    where
      prettyPath i path
        = "Path" <+> pretty i <> ":" <> line
          <> indent 2 (pretty path)

-- wip threaded implementtion of checkvalidvcs
-- caused "exhausted resource" exception
checkValidVCsThreaded vcs newSolver = do
  counterVar <- newTVarIO $ toList vcs
  resultVar <- newTVarIO Valid
  solverThreadIds <- mapM forkIO
    $ fmap (checkSolverVC counterVar resultVar) vcs
  
  validityResult <- atomically $ readTVar counterVar >>= \case
    _:_ -> retry
    [] -> readTVar resultVar

  mapM_ killThread solverThreadIds

  return validityResult

  where
    fullClosure = foldMap getClosure vcs

    checkSolverVC counterVar resultVar vc = do
      solver <- newSolver
      GCL.SMT.declareHeader solver fullClosure
      GCL.SMT.boolToSMTAssertion solver $ Not vc
      smtResult <- SMT.check solver
      validityResult <- case smtResult of
        SMT.Unsat -> SMT.stop solver >> return Valid
        SMT.Unknown -> SMT.stop solver >> return (UnknownValidity vc)
        SMT.Sat -> do
          model <- GCL.SMT.getModel solver fullClosure
          SMT.stop solver
          return $ Invalid vc model

      if isValid validityResult
      then atomically
           $ modifyTVar counterVar
           $ \case [] -> [] ; xs -> tail xs
      else atomically
           $ writeTVar counterVar []
           >> modifyTVar resultVar
           \case Valid -> validityResult ; res -> res

