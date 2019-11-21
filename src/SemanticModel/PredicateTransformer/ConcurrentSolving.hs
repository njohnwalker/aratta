module SemanticModel.PredicateTransformer.ConcurrentSolving
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.HashSet as Set
import Data.Text.Prettyprint.Doc

import Say

import SMTLib.IO

import SemanticModel.PredicateTransformer
import SemanticModel.PredicateTransformer.InvariantLattice
import SemanticModel.PredicateTransformer.Validity

type ValidInvariantTMVar inv = TMVar inv
type InvariantLatticeTVar = TVar LazyLattice

type MonadSolver inv m
  = ( Pretty inv
    , PredicateTransformer inv
    , MonadReader (SolverEnv inv) m
    )

data SolverEnv inv = SolverEnv
  { invariantList :: [inv]
  , invariantMap :: InvariantMap inv
  , invariantLatticeTVar :: InvariantLatticeTVar
  , invariantResultTMVar :: ValidInvariantTMVar inv
  , invariantParameterizedVCs :: Reader inv [inv]
  , isVerbose :: Bool
  }

-- | unpack the Reader Monad transformer from a Solver Action
runSolver :: SolverEnv inv -> ReaderT (SolverEnv inv) m a -> m a
runSolver env = flip runReaderT env 

-- | Initialize invariant lattice and transaction variables
--   and spawn solver processes on each singleton candidate
--   invariant 
beginSolverFactory
  :: ( Pretty inv
     , PredicateTransformer inv
     )
  => Bool
  -> [inv]
  -> Reader inv [inv]
  -> ValidInvariantTMVar inv
  -> IO ()
beginSolverFactory isVerbose invs pVCs mResultVar =
  let (invMap, initialState, initialInvariants) = initialLattice invs
  in  do
    invariantLatticeTVar <- newTVarIO initialState

    let solverEnv = SolverEnv
          { invariantList = invs
          , invariantMap = invMap
          , invariantLatticeTVar = invariantLatticeTVar
          , invariantResultTMVar = mResultVar
          , invariantParameterizedVCs = pVCs
          , isVerbose = isVerbose
          }

    mapM_ forkIO [ runSolver solverEnv $ solverThread invKey
                 | invKey <- initialInvariants ]

-- | Main solver thread for a single invariant validation
--   Spawns subsequent threads based on the state of the
--   invariant lattice
solverThread
  :: (MonadSolver inv m, MonadIO m)
  => Invariant
  -> m ()
solverThread invKey = do
  SolverEnv{..} <- ask

  let inv = getInvariant invKey invariantMap  
  
  resultIsEmpty <- liftIO $ atomically $ isEmptyTMVar invariantResultTMVar 

  if not resultIsEmpty then return ()
    else do
    show invKey `seq` return ()       -- race on stdout...
    show (pretty inv) `seq` return () -- many threads created at once
    when isVerbose $ sayString $ unlines
      [ "Trying Candidate Invariant:"
      , "    Index: "++ show (Set.toList invKey)
      , "    Invariant: "++ show (pretty inv) 
      ]

    let vcs = runReader invariantParameterizedVCs inv

    validity <- liftIO $ checkValidVCs vcs $ newCVC4Solver 10

    case validity of
      -- Invariant is valid, report it to the TVar
      Valid -> do
        sayString $ unlines
          [ "Success! Found Valid Invariant:"
          , "    Index: "++ show (Set.toList invKey)
          , "    Invariant: "++ show (pretty inv) 
          ]
        liftIO $ atomically $ putTMVar invariantResultTMVar inv

      -- Invariant is invalid, spawn new solvers
      Invalid vc env -> do
        when isVerbose $ sayString $ unlines
          [ "Invalid VC:"
          , "    Index: "++ show (Set.toList invKey)
          , "    Invariant: "++ show (pretty inv)
          , "    VC: "++ show (pretty vc)
          , "With counterexample: "++ show env
          ]
        solverFactory invKey

      -- Invariant has unknown validity;
      -- pessimistically, spawn new solvers
      UnknownValidity vc -> do
        when isVerbose $ sayString $ unlines
          [ "Unknown result from solver,"
          , "treating as Invalid:"
          , "     Index: "++ show (Set.toList invKey)
          , "     Invariant: "++ show (pretty inv)
          , "     VC: "++ show (pretty vc)
          ]
        solverFactory invKey
    
    where
      -- update invariant lattice and spawn new solvers
      solverFactory
        :: ( MonadSolver int m, MonadIO m )
        => Invariant -> m ()
      solverFactory invalidInvariant = do
        solverEnv@SolverEnv{..} <- ask
        -- if this is the maximal invariant (not top) we're done here
        if invalidInvariant == maxInvariant invariantList 
          then liftIO $ atomically $ putTMVar invariantResultTMVar invariantTop
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
