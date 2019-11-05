module Language.GCL.Semantics.SmallStep
  ( evalBool
  , evalBoolAny
  )
where

import Control.Monad.Reader ( Reader, ReaderT, runReader, runReaderT, reader, lift )
import Control.Monad.Except ( ExceptT, runExceptT, lift, throwError)

import Data.Map ( Map )
import qualified Data.Set as Set 
import Data.Text ( Text )

import Language.GCL.Syntax.Abstract
import Language.GCL.Environment

data DivZero = DivZero

type MaybeDivZero = ExceptT DivZero

evalBoolInternal :: BExp -> Reader Env Bool
evalBoolInternal = \case
  BConst b -> return b
  Not e -> not <$> evalBoolInternal e
  e1 :&: e2  -> (&&) <$> evalBoolInternal e1 <*> evalBoolInternal e2
  e1 :|: e2  -> (||) <$> evalBoolInternal e1 <*> evalBoolInternal e2
  e1 :=>: e2 -> (\l r -> not l || r) <$> evalBoolInternal e1 <*> evalBoolInternal e2
  i1 :==: i2 -> evalRelation (==) i1 i2
  i1 :<=: i2 -> evalRelation (<=) i1 i2
  i1 :>=: i2 -> evalRelation (>=) i1 i2
  i1 :<: i2  -> evalRelation (<) i1 i2
  i1 :>: i2  -> evalRelation (>) i1 i2
  where
    evalRelation
      :: (Integer -> Integer -> Bool)
      -> IExp -> IExp
      -> Reader Env Bool
    evalRelation op mInt1 mInt2 = do
      eI1 <- runExceptT $ evalIntInternal mInt1
      eI2 <- runExceptT $ evalIntInternal mInt2
      return case (eI1, eI2) of
        (Left _, _) -> True 
        (_, Left _) -> True
        (Right i1, Right i2) -> i1 `op` i2


evalIntInternal :: IExp -> MaybeDivZero (Reader Env) Integer
evalIntInternal = \case
  Var x -> lift $ reader $ lookupEnv x
  IConst i -> return i
  e1 :+: e2 ->   (+) <$> evalIntInternal e1 <*> evalIntInternal e2 
  e1 :-: e2 ->   (-) <$> evalIntInternal e1 <*> evalIntInternal e2 
  e1 :*: e2 ->   (*) <$> evalIntInternal e1 <*> evalIntInternal e2 
  e1 :/: e2 -> do
    denominator <- evalIntInternal e2
    case denominator of
      0 -> throwError DivZero
      nonZero -> (`div` nonZero) <$> evalIntInternal e1
  e1 :%: e2 -> do
    denominator <- evalIntInternal e2
    case denominator of
      0 ->  throwError DivZero
      nonZero -> (`mod` nonZero) <$> evalIntInternal e1

{- | Guards of GCL Commands are atomic
-}
evalBool :: Env -> BExp -> Bool
evalBool env bexp = runReader (evalBoolInternal bexp) env

{- | For unsatisfiable boolean expressions
-}
evalBoolAny :: BExp -> Bool
evalBoolAny bexp = runReader (evalBoolInternal bexp) env
  where
    env = makeAnyEnvironment $ Set.map getVariableText $ getClosureBool bexp
