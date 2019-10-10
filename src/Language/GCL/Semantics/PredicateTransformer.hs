module Language.GCL.Semantics.PredicateTransformer
where

import Control.Monad ((>=>))
import Control.Lens (transform, (^?), over )
import Data.Text ( Text )

import Language.GCL.SMTLib ( boolToSExpr )
import Language.GCL.Syntax.Abstract

data BasicPath
  = Sequence [BasicPath]
  | Assume (BExp)
  | Substitute Text IExp
  
getBasicPathVC pt GCLProgram {req, program, ens} = undefined
  
wp = \case
  Assume p -> (p :=>:)
  Substitute v e -> (substitute v e) 
  -- Sequence stmts -> foldl  
     
sp = undefined

-------------
-- helpers --
substitute :: Text -> IExp -> BExp -> BExp
substitute v e = transform match
  where
    match = \case
      i1 :<=: i2 -> substituteIExp v e i1 :<=: substituteIExp v e i2
      e' -> e'

substituteIExp :: Text -> IExp -> IExp ->IExp
substituteIExp v e = transform match 
  where
    match = \case
      Var v' | v == v' -> e
      e' -> e'  
