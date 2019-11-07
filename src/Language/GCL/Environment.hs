module Language.GCL.Environment
  ( Env
  , initialEnv
  , lookupEnv
  , makeAnyEnvironment
  , getClosureBool
  , getClosureStmt
  , getClosure
  , Map.fromList
  )
where

import Control.Lens ( toListOf )

import Data.Data.Lens ( biplate )
import           Data.Set ( Set )
import qualified Data.Set as Set
import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Text ( Text )

import           Data.Stream.Infinite  ( Stream )
import qualified Data.Stream.Infinite as Stream

import Language.GCL.Syntax.Abstract

type Env = Map Text Integer

initialEnv :: Env
initialEnv = Map.empty

lookupEnv :: Variable -> Env ->  Integer
lookupEnv = Map.findWithDefault 0 . getVariableText

integerStream :: (Num a, Ord a) => Stream a
integerStream = Stream.iterate (\x-> if x < 0 then x * (-1) + 1 else -(x+1)) 0

tails = Stream.iterate Stream.tail

makeAnyEnvironment :: Set Text -> Env 
makeAnyEnvironment = Map.fromList . flip zip [1,1..] . Set.toList


-- TODO: convert to `foldmapof sets` instead of lists
-- | Find the variable closure of a program
getClosure :: GCLProgram -> Set Variable
getClosure GCLProgram {req, program, ens}
  =  requiresClo <> programClo <> ensuresClo
  where
    requiresClo = maybe mempty getClosureBool req
    ensuresClo = maybe mempty getClosureBool ens
    programClo = Set.fromList $ toListOf biplate program

-- | Find the free variables of a statement
getClosureStmt :: Statement -> Set Variable
getClosureStmt = Set.fromList . toListOf biplate

-- | Find the free variables of a boolean expression
getClosureBool :: BExp -> Set Variable
getClosureBool = Set.fromList . toListOf biplate

-- | Find the free variables of an integer expression
getClosureInt :: IExp -> Set Variable
getClosureInt = Set.fromList . toListOf biplate
