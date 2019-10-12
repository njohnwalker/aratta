module Language.GCL.Semantics.PredicateTransformer
where

import Control.Monad ((>=>))
import Control.Monad.Reader hiding ( guard )
import Control.Lens ( transform, (^?), over )
import Data.Text ( Text )

import Language.GCL.SMTLib ( boolToSExpr )
import Language.GCL.Syntax.Abstract

data BasicPath
  =  Assume BExp
  | Substitute Text IExp

type ParameterizedInvariant = ReaderT BExp []
instance Semigroup (ParameterizedInvariant a) where
  (<>) (ReaderT l1) (ReaderT l2) = ReaderT $ \r -> l1 r <> l2 r
                                                  
getBasicPathVCs :: GCLProgram -> ParameterizedInvariant BExp
getBasicPathVCs GCLProgram {req , program, ens} =
 let path = maybe [] ((:[]) . Assume) req
     post = maybe (BConst True) id ens 
  in uncurry wpSeq <$> getPaths post program path 

-- | Generate all basic paths and postcondition pairs, parameterized by invariant
getPaths :: BExp -> Statement -> [BasicPath] -> ParameterizedInvariant ([BasicPath],BExp)

-- Assign Statements
getPaths post (vars := exps) path =
  return $ (,post) $ path ++ zipWith Substitute vars exps

-- If Statements
getPaths post (If (GCS gcs)) path =
  return (path ++ [Assume $ negateGuards gcs], post) <>
  do
    GC {guard, statement} <- lift gcs
    getPaths post statement (path ++ [(Assume guard)])


-- Empty Sequence Statements
getPaths post (Seq []) path = return (path,post)

-- Inductive sequence statements
getPaths post (Seq (stmt:stmts)) path
  = case stmt of
      vars := exps -> getPaths post (Seq stmts) $ path ++ zipWith Substitute vars exps
      If (GCS gcs) ->
        return (path ++ [Assume $ negateGuards gcs], post) <>
        do
          GC {guard, statement} <- lift gcs
          let stmts' = case statement of Seq stmts' -> stmts' ; stmt' -> [stmt'] 
          getPaths post (Seq (stmts'++stmts)) (path ++ [(Assume guard)])

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

negateGuards :: [GuardedCommand] -> BExp
negateGuards [] = BConst True
negateGuards xs
  = foldl1 (:&:)
  $ map (Not . guard) xs

