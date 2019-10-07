module SemanticModel.PredicateTransformer
where

import Control.Monad.Reader ( Reader )
import SimpleSMT ( SExpr )

-- | Class of languages with predicate transformer semantics
class PredicateTransformer lang where

  -- | Predicate type
  data Predicate lang

  -- | Variable type
  data Variable lang

  -- | Expression (of variable assignment) type
  data Expression lang

  -- | Basic block type
  data BasicBlock lang

  -- | Generate all basic path verification conditions from a program
  --   parameterised on predicate transformer
  getBasicPathVC
    :: (BasicBlock lang -> Predicate lang -> Predicate lang)
    -> lang
    -> Reader (Predicate lang) [Predicate lang]

  -- | Generate SExpr for smt solver from predicate
  predicateToSExpr :: Predicate lang -> SExpr

  ----------------------------
  -- Predicate Transformers --

  -- | Calculate the Weakest Precondition of a block, given a postcondition
  wp :: BasicBlock lang -> Predicate lang -> Predicate lang

  -- | Calculate the Strongest Postcondition of a block, given a precondition
  sp :: BasicBlock lang -> Predicate lang -> Predicate lang
