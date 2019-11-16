module SemanticModel.PredicateTransformer.InvariantLattice
where

import           Data.Hashable
import qualified Data.HashSet as HSet

type Candidate invariant = HSet.HashSet invariant

initialCandidates :: (Hashable inv) => [inv] -> [Candidate inv]
initialCandidates = map HSet.singleton

iterateCandidates
  :: (Hashable inv, Eq inv)
  => [Candidate inv]
  -> Candidate inv
  -> [Candidate inv]
iterateCandidates is i' =
  [i' `HSet.union` i | i <- is, HSet.null( i `HSet.intersection` i')  ]
