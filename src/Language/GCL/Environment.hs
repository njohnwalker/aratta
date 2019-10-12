module Language.GCL.Environment
  ( Env
  , initialEnv
  , lookupEnv
  , makeAnyEnvironment
  , getClosureBool
  , Map.fromList
  )
where

import Data.Text ( Text )
import Data.Set
import Data.Map ( Map, fromList )
import qualified Data.Map as Map

import           Data.Stream.Infinite  ( Stream )
import qualified Data.Stream.Infinite as Stream

import Language.GCL.Syntax.Abstract

type Env = Map Text Integer

initialEnv :: Env
initialEnv = Map.empty

lookupEnv :: Text -> Env ->  Integer
lookupEnv = Map.findWithDefault 0


arbitraryEnv :: Env
arbitraryEnv = undefined


integerStream :: (Num a, Ord a) => Stream a
integerStream = Stream.iterate (\x-> if x < 0 then x * (-1) + 1 else -(x+1)) 0

tails = Stream.iterate Stream.tail

makeAnyEnvironment :: Set Text -> Env
makeAnyEnvironment = Map.fromList . flip zip [0,0..] . toList

-- | Find the free variables of a boolean expression
getClosureBool :: BExp -> Set Text
getClosureBool (Not e)     = getClosureBool e
getClosureBool (e1 :&: e2) = getClosureBool e1 <> getClosureBool e2
getClosureBool (e1 :<=:e2) = getClosureInt e1 <> getClosureInt e2
getClosureBool _ = mempty

-- | Find the free variables of an integer expression
getClosureInt :: IExp -> Set Text
getClosureInt (Var name)  = singleton name
getClosureInt (i1 :-: i2) = getClosureInt i1 <> getClosureInt i2
getClosureInt _           = mempty
    
