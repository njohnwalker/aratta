module SemanticModel.PredicateTransformer.Validity
where

import qualified SMTLib.Environment as SMT

data Validity inv
  = Valid
  | Invalid inv SMT.Env
  | UnknownValidity inv
  deriving (Eq, Show)

isValid :: Validity inv -> Bool
isValid = \case Valid -> True ; _ -> False 
