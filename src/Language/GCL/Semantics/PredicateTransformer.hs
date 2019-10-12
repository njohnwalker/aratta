module Language.GCL.Semantics.PredicateTransformer
where

import Control.Monad ((>=>))
import Control.Monad.Reader
import Control.Lens (transform, (^?), over )
import Data.Text ( Text )

import Language.GCL.SMTLib ( boolToSExpr )
import Language.GCL.Syntax.Abstract

data BasicPath
  =  Assume BExp
  | Substitute Text IExp

type ParameterizedInvariant = ReaderT BExp []

getBasicPathVCs :: GCLProgram -> ParameterizedInvariant BExp
getBasicPathVCs GCLProgram {req , program, ens} =
 let path = maybe [] ((:[]) . Assume) req
     post = maybe (BConst True) id ens 
  in uncurry wpSeq <$> getPaths post program path 

getPaths :: BExp -> Statement -> [BasicPath] -> ParameterizedInvariant ([BasicPath],BExp)
getPaths post (vars := exps) path =
  return $ (,post) $ path ++ zipWith Substitute vars exps

getPaths post (Seq []) path = return (path,post)
getPaths post (Seq (stmt:stmts)) path
  = case stmt of
      vars := exps -> getPaths post (Seq stmts) $ path ++ zipWith Substitute vars exps
      _ -> error "not implemented yet"
getPaths _ _ _ = error "not implemented yet"

specifyInvariant :: BExp -> ParameterizedInvariant a -> [a]
specifyInvariant = flip runReaderT 

wp :: BasicPath -> BExp -> BExp
wp = \case
  Assume p -> (p :=>:)
  Substitute v e -> (substitute v e) 
  
wpSeq :: [BasicPath] -> BExp -> BExp
wpSeq = foldl (\f ins -> f . wp ins) id

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
