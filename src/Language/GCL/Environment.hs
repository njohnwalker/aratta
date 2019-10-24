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

import Control.Lens -- ( toListOf, both, _2 )
import Control.Lens.Plated ( cosmos )

import           Data.Text ( Text )
import           Data.Set ( Set )
import qualified Data.Set as Set
import           Data.Map ( Map )
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
makeAnyEnvironment = Map.fromList . flip zip [0,0..] . Set.toList

-- | get the variable closure of a program
getClosure :: GCLProgram -> Set Text
getClosure GCLProgram {req, program, ens}
  =  reqClo <> getClosureStmt program <> ensClo
  where
    reqClo = maybe mempty getClosureBool req
    ensClo = maybe mempty getClosureBool ens

-- | Find the free variables of a statement
getClosureStmt :: Statement -> Set Text
getClosureStmt = Set.fromList . toListOf (statementFold . _Var)
  where
    statementFold = cosmos . (assignFold <> doFold <> ifFold)
    assignFold = (.:=) . (_1 . traverse . to Var <> _2 . traverse . cosmos)
    doFold = _Do . (_1 . invFold <> _2 . commandListFold)
    invFold = traverse . bExpFold
    bExpFold = cosmos . (.:<=:) . both . cosmos
    ifFold = _If . commandListFold
    commandListFold = _getCommandList . traverse . (_guard . bExpFold <> (_statement . statementFold))

-- | Find the free variables of a boolean expression
getClosureBool :: BExp -> Set Text
getClosureBool = Set.fromList . toListOf (cosmos . (.:<=:) . both . cosmos . _Var)

-- getClosureBool (Not e)     = getClosureBool e
-- getClosureBool (e1 :&: e2) = getClosureBool e1 <> getClosureBool e2
-- getClosureBool (e1 :<=:e2) = getClosureInt e1 <> getClosureInt e2
-- getClosureBool _ = mempty

-- | Find the free variables of an integer expression
getClosureInt :: IExp -> Set Text
getClosureInt = Set.fromList . toListOf (cosmos . _Var)

-- getClosureInt (Var name)  = Set.singleton name
-- getClosureInt (i1 :-: i2) = getClosureInt i1 <> getClosureInt i2
-- getClosureInt _           = mempty
