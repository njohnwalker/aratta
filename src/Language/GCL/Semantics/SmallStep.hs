module Language.GCL.Semantics.SmallStep
  ( DivZero(..)
  , evalBool
  , evalBoolAny
  , eDiv
  , eMod
  )
where

import Control.Monad.Reader ( Reader, ReaderT, runReader, runReaderT, reader, lift )
import Control.Monad.Except ( ExceptT, runExceptT, lift, throwError, mplus)

import Data.Map ( Map )
import qualified Data.Set as Set 
import Data.Text ( Text )

import Language.GCL.Syntax.Abstract
import Language.GCL.Environment

data DivZero = DivZero
  deriving (Eq, Ord, Show)

instance Semigroup DivZero where
  (<>) = const

instance Monoid DivZero where
  mempty = DivZero

type MaybeDivZero = ExceptT DivZero

evalBoolInternal :: BExp -> MaybeDivZero (Reader Env) Bool
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
      -> MaybeDivZero (Reader Env) Bool
    evalRelation op mInt1 mInt2 = do
      i1 <- evalIntInternal mInt1
      i2 <- evalIntInternal mInt2
      return $ i1 `op` i2


evalIntInternal :: IExp -> MaybeDivZero (Reader Env) Integer
evalIntInternal = \case
  Var x -> lift $ reader $ lookupEnv x
  IConst i -> return i
  e1 :+: e2 ->   (+) <$> evalIntInternal e1 <*> evalIntInternal e2 
  e1 :-: e2 ->   (-) <$> evalIntInternal e1 <*> evalIntInternal e2 
  e1 :*: e2 ->
    evalIntInternal e1 `mplus` evalIntInternal e2
    >>= \case
      0 -> return 0
      _ -> (*) <$> evalIntInternal e1 <*> evalIntInternal e2
  e1 :/: e2 ->
    do i1 <- evalIntInternal e1
       i2 <- evalIntInternal e2
       i1 `eDivM` i2
  e1 :%: e2 ->
    do i1 <- evalIntInternal e1
       i2 <- evalIntInternal e2
       i1 `eModM` i2

{- | Guards of GCL Commands are atomic
-}
evalBool :: Env -> BExp -> Either DivZero Bool
evalBool env bexp
  = flip runReader env
  $ runExceptT (evalBoolInternal bexp)

{- | For unsatisfiable boolean expressions
-}
evalBoolAny :: BExp -> Either DivZero Bool
evalBoolAny bexp
  = flip runReader env
  $ runExceptT (evalBoolInternal bexp)
  where
    env = makeAnyEnvironment
        $ Set.map getVariableText
        $ getClosureBool bexp

--eDiv :: Integral a => a -> a -> a
eDivM m 0 = throwError DivZero
eDivM m n = return $ m `eDiv` n

--eDiv :: Integral a => a -> a -> a
eDiv m n = if n < 0 then ceiling x  else floor x
    where x = fromIntegral m / fromIntegral n

--eMod :: Integral a => a -> a -> Maybe a
eModM m 0 = throwError DivZero
eModM m n = return $ m `eMod` n

eMod :: Integral a => a -> a -> a
eMod m n = m - abs n * floor q'
  where q = m `eDiv` n
        q' = fromIntegral m / fromIntegral (abs n)
-- when n is positive, (div m n) is the floor of the rational number m/n;
-- when n is negative, (div m n) is the ceiling of m/n.
-- div truncate -inf
-- quot truncate 0
