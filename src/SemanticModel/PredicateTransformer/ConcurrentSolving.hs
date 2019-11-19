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

type InvalidInvariantBuffer = TQueue Invariant
type ValidInvariantTMVar inv = TMVar inv
type InvariantLatticeTVar = TVar LazyLattice

beginSolverFactory
  :: [BExp]
  -> ParameterizedInvariant [BExp]
  -> ValidInvariantTMVar BExp
  -> IO ()
beginSolverFactory invs pVCs mResultVar =
  let (factoryEnv, initialState, initialInvPairs) = initialLattice invs
  in  do
    invalidBuffer <- newTQueueIO
    invariantLatticeTVar <- newTVarIO initialState
    
    mapM_ forkIO [ solverThread invKey inv pVCs mResultVar invalidBuffer
                 | (invKey, inv) <- initialInvPairs ]

    runFactory factoryEnv initialState
      $ solverFactory' factoryEnv mResultVar invalidBuffer invariantLatticeTVar 
  where
    runFactory env state = flip runReaderT env 
    
    solverFactory'
      :: (MonadFactory BExp m, MonadIO m)
      => FactoryEnv BExp
      -> ValidInvariantTMVar BExp
      -> InvalidInvariantBuffer
      -> InvariantLatticeTVar
      -> m ()
    solverFactory' factoryEnv mResultVar invalidBuffer invariantLatticeTVar = do
      resultIsEmpty <- liftIO $ atomically $ isEmptyTMVar mResultVar
      if not resultIsEmpty then
        -- halt factory if result has been found
        say "Factory Halting" >> return ()
  
        -- otherwise,
        --    - read the buffer
        --    - update the lattice
        --    - spawn next solver threads
        else do
        invalidInvariant <- liftIO $ atomically $ readTQueue invalidBuffer
        if invalidInvariant == maxInvariant invs
          then liftIO $ atomically $ putTMVar mResultVar False_
          else do

          nextInvariants <- liftIO
            $ atomically
            $ stateTVar invariantLatticeTVar
            $ updateLattice [1.. length invs] invalidInvariant 

          sayShow nextInvariants

          let invMap = invariantMap factoryEnv
              solverThreads = 
                [ solverThread invKey (getInvariant invKey invMap)
                  pVCs mResultVar invalidBuffer
                | invKey <- nextInvariants
                ]

          liftIO $ mapM_ forkIO solverThreads

          solverFactory' factoryEnv mResultVar invalidBuffer invariantLatticeTVar

solverThread
  :: Invariant
  -> BExp
  -> ParameterizedInvariant [BExp]
  -> ValidInvariantTMVar BExp
  -> InvalidInvariantBuffer
  -> IO ()
solverThread invKey inv pVCs mResultVar invalidBuffer = do
  resultIsEmpty <- atomically $ isEmptyTMVar mResultVar 
  if not resultIsEmpty then return ()
    else do
    show invKey `seq` return ()
    show (pretty inv) `seq` return ()
    sayString $ unlines
      [ "Trying Candidate Invariant:"
      , "    Index: "++ show invKey
      , "    Invariant: "++ show (pretty inv) 
      ]
  
    validity <-
      checkValidVCs (newCVC4Solver 10) inv pVCs
  
    case validity of
      Valid -> do
        sayString $ unlines
          [ "Success! Found valid VC:"
          , "    Index: "++ show invKey
          , "    Invariant: "++ show (pretty inv) 
          ]
        atomically $ putTMVar mResultVar inv
      Invalid vc env -> do
        sayString $ unlines
          [ "Invalid VC:"
          , "    Index: "++ show invKey
          , "    Invariant: "++ show (pretty inv)
          , "    VC: "++ show (pretty vc)
          , "With counterexample: "++ show env
          ]
        atomically $ writeTQueue invalidBuffer invKey
      UnknownValidity vc -> do
        sayString $ unlines
          [ "Unknown result from solver,"
          , "treating as Invalid:"
          , "     Index: "++ show invKey
          , "     Invariant: "++ show (pretty inv)
          , "     VC: "++ show (pretty vc)
          ]
        atomically $ writeTQueue invalidBuffer invKey
