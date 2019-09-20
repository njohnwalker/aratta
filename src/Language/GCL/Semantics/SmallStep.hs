module Language.GCL.Semantics.SmallStep
  ( evalBool
  , evalBoolAny
  )
where

import Control.Monad.Reader ( Reader, runReader, reader )
import Language.GCL.Syntax.Abstract
import Language.GCL.Environment


evalBoolInternal :: BExp -> Reader Env Bool
evalBoolInternal = \case
  BConst b -> return b
  Not e -> not <$> evalBoolInternal e
  e1 :&: e2 -> (&&) <$> evalBoolInternal e1 <*> evalBoolInternal e2
  i1 :<=: i2 -> (<=) <$> evalIntInternal i1 <*> evalIntInternal i2

evalIntInternal :: IExp -> Reader Env Integer
evalIntInternal = \case
  Var x -> reader $ lookupEnv x
  IConst i -> return i
  e1 :-: e2 -> (-) <$> evalIntInternal e1 <*> evalIntInternal e2 


{- | Guards of GCL Commands are atomic
-}
evalBool :: Env -> BExp -> Bool
evalBool env bexp = runReader (evalBoolInternal bexp) env

{- | For unsatisfiable boolean expressions
-}
evalBoolAny :: BExp -> Bool
evalBoolAny bexp = runReader (evalBoolInternal bexp) env
  where
    env = makeAnyEnvironment $ getClosureBool bexp
