module SemanticModel.PredicateTransformer.ConcurrentSolving
where

import Data.Hashable

import Control.Concurrent.STM

import SemanticModel.PredicateTransformer.InvariantLattice

type     ValidInvariantTMVar inv = TMVar inv
type   InvalidInvariantsTVar inv = TVar  [Candidate inv]
type PossibleInvariantsTChan inv = TChan (Candidate inv)


initializeInvariantTVars
  :: Hashable inv
  => [inv]
  -> STM ( TMVar inv             -- a valid invariant
         , TVar  [Candidate inv] -- proven invalid invariants 
         , TChan (Candidate inv) -- possible invariants to validate
         )
initializeInvariantTVars invs = do
  validInvariant <- newEmptyTMVar
  invalidInvariants <- newTVar []
  possibleInvariants <- newTChan
  mapM_ (writeTChan possibleInvariants)
    $ initialCandidates invs
  return (validInvariant, invalidInvariants, possibleInvariants)


-- | Read and update the list of proven invalid invariants and
--   send (non-blocking) next possible invariants to the
--   to-solve buffer
updatePossibleInvariants
  :: (Hashable inv, Eq inv)
  => Candidate inv
  -> InvalidInvariantsTVar inv
  -> PossibleInvariantsTChan inv
  -> IO ()
updatePossibleInvariants inv invalidInvTVar invTChan = do
  invalidInvs <- atomically $ do
    invalidInvs <- readTVar invalidInvTVar
    writeTVar invalidInvTVar $ inv : invalidInvs
    return invalidInvs
  mapM_ (atomically . writeTChan invTChan)
    $ iterateCandidates invalidInvs inv
