module SemanticModel.PredicateTransformer.ConcurrentSolving
where

import Data.Hashable

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State

import SemanticModel.PredicateTransformer.InvariantLattice
import SemanticModel.PredicateTransformer.Validity

import Language.GCL

import Control.Exception
import Say

type ValidInvariantTMVar inv = TMVar inv
type InvariantLatticeTVar = TVar LazyLattice

type MonadSolver inv
  = MonadReader (SolverEnv inv)

data SolverEnv inv = SolverEnv
  { invariantList :: [inv]
  , invariantMap :: InvariantMap inv
  , invariantLatticeTVar :: InvariantLatticeTVar
  , invariantResultTMVar :: ValidInvariantTMVar inv
  , invariantParameterizedVCs :: ParameterizedInvariant [inv]
  }

runSolver :: SolverEnv inv -> ReaderT (SolverEnv inv) m a -> m a
runSolver env = flip runReaderT env 

-- | Initialize invariant lattice and transaction variables
--   and spawn solver processes on each singleton candidate
--   invariant 
beginSolverFactory
  :: [BExp]
  -> ParameterizedInvariant [BExp]
  -> ValidInvariantTMVar BExp
  -> IO ()
beginSolverFactory invs pVCs mResultVar =
  let (invMap, initialState, initialInvariants) = initialLattice invs
  in  do
    invariantLatticeTVar <- newTVarIO initialState

    let solverEnv = SolverEnv
          { invariantList = invs
          , invariantMap = invMap
          , invariantLatticeTVar = invariantLatticeTVar
          , invariantResultTMVar = mResultVar
          , invariantParameterizedVCs = pVCs
          }
    
    mapM_ forkIO [ runSolver solverEnv $ solverThread invKey
                 | invKey <- initialInvariants ]

solverThread
  :: (MonadSolver BExp m, MonadIO m)
  => Invariant
  -> m ()
solverThread invKey = do
  SolverEnv{..} <- ask

  let inv = getInvariant invKey invariantMap  
  
  resultIsEmpty <- liftIO $ atomically $ isEmptyTMVar invariantResultTMVar 

  if not resultIsEmpty then return ()
    else do
    show invKey `seq` return ()
    show (pretty inv) `seq` return ()
    sayString $ unlines
      [ "Trying Candidate Invariant:"
      , "    Index: "++ show invKey
      , "    Invariant: "++ show (pretty inv) 
      ]

    liftIO ( checkValidVCs
             (newCVC4Solver 10)
             inv
             invariantParameterizedVCs
           )
      >>= \case
      -- Invariant is valid, report it to the TVar
      Valid -> do
        sayString $ unlines
          [ "Success! Found valid VC:"
          , "    Index: "++ show invKey
          , "    Invariant: "++ show (pretty inv) 
          ]
        liftIO $ atomically $ putTMVar invariantResultTMVar inv

      -- Invariant is invalid, spawn new solvers
      Invalid vc env -> do
        sayString $ unlines
          [ "Invalid VC:"
          , "    Index: "++ show invKey
          , "    Invariant: "++ show (pretty inv)
          , "    VC: "++ show (pretty vc)
          , "With counterexample: "++ show env
          ]
        solverFactory invKey

      -- Invariant has unknown validity;
      -- pessimistically, spawn new solvers
      UnknownValidity vc -> do
        sayString $ unlines
          [ "Unknown result from solver,"
          , "treating as Invalid:"
          , "     Index: "++ show invKey
          , "     Invariant: "++ show (pretty inv)
          , "     VC: "++ show (pretty vc)
          ]
        solverFactory invKey
    
    where
      -- update invariant lattice and spawn new solvers
      solverFactory
        :: (MonadSolver BExp m, MonadIO m)
        => Invariant -> m ()
      solverFactory invalidInvariant = do
        solverEnv@SolverEnv{..} <- ask
        if invalidInvariant == maxInvariant invariantList 
          then liftIO $ atomically $ putTMVar invariantResultTMVar False_
          else do
          
          nextInvariants <- liftIO
            $ atomically
            $ stateTVar invariantLatticeTVar
            $ updateLattice invariantList invalidInvariant 
                  
          liftIO $ mapM_ forkIO
            [ runSolver solverEnv
                  $ solverThread invKey
            | invKey <- nextInvariants
            ]
