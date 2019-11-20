module SemanticModel.PredicateTransformer
where

import Control.Monad.Reader

import SimpleSMT as SMT

import SemanticModel.PredicateTransformer.Validity

class PredicateTransformer predicate where

  checkValidVCs 
    :: Traversable t
    => t predicate
    -- ^ Container of vcs to verify
    -> IO SMT.Solver
    -- ^ IO action to generate a new SMT solver
    -> IO (Validity predicate)

  -- | top element in invariant lattice
  invariantTop :: predicate

  -- | Join operation for invariant lattice
  invariantJoin :: predicate -> predicate -> predicate
  
