module SemanticModel.PredicateTransformer.ConcurrentSolving
where

import Data.Hashable

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Reader

import SemanticModel.PredicateTransformer.InvariantLattice
import SemanticModel.PredicateTransformer.Validity

type InvalidInvariantBuffer = TQueue Invariant
type ValidInvariantTMVar inv = TMVar inv 

beginSolverFactory :: [inv] -> Reader inv [inv] -> IO ()
beginSolverFactory invs pVCs = undefined
  where
    solverFactory'
      :: (MonadFactory inv m, MonadIO m)
      => ValidInvariantTMVar inv
      -> InvalidInvariantBuffer
      -> m ()
    solverFactory'  mResultVar invalidBuffer = do
      mResult <- liftIO $ atomically $ tryReadTMVar mResultVar
      case mResult of
        -- halt factory if result has been found
        Just _ -> return ()
  
        -- otherwise,
        --    - read the buffer
        --    - update the lattice
        --    - spawn next solver threads
        Nothing -> do
          invalidInvariant <- liftIO $ atomically $ readTQueue invalidBuffer
          nextInvariants <- updateLattice invalidInvariant
          invMap <- asks invariantMap
          let solverThreads =
                []
          solverFactory' mResultVar invalidBuffer

solverThread
  :: Invariant
  -> inv
  -> Reader inv [inv]
  -> InvalidInvariantBuffer
  -> ValidInvariantTMVar inv
  -> IO ()
solverThread invKey inv pVCs invalidBuffer mResultVar = do
  undefined
