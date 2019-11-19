module Language.GCL.Environment
where

import Control.Lens ( toListOf )

import           Data.Data
import           Data.Data.Lens ( biplate )
import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Set ( Set )
import qualified Data.Set as Set
import           Data.Stream.Infinite  ( Stream )
import qualified Data.Stream.Infinite as Stream
import           Data.Text ( Text )
import           Data.Text.Prettyprint.Doc


import Language.GCL.Syntax.Abstract

type Env = Map Variable Integer

initialEnv :: Env
initialEnv = Map.empty

lookupEnv :: Variable -> Env ->  Integer
lookupEnv = Map.findWithDefault 0

fromListEnv :: [(Variable, Integer)] -> Env
fromListEnv = Map.fromList

makeAnyEnvironment :: Set Variable -> Env 
makeAnyEnvironment = Map.fromList . flip zip [1,1..] . Set.toList

-- | Find the variable closure of a *thing*
getClosure :: Data a => a -> Set Variable
getClosure = Set.fromList . toListOf biplate

instance Pretty Env where
  pretty env = encloseSep "{ " " }" (", ")
              (map prettySub list)
    where list = Map.toList env
          prettySub (v,e) = "@" <> pretty v <+> "->" <+> pretty e 

--------------------
-- Free Variables --
type FVEnv = Map Variable (Stream Variable)

initialFVEnv :: Set Variable -> FVEnv
initialFVEnv = Map.fromSet buildFVStream
  where
    buildFVStream v = Stream.iterate (<>"'") (v <> "'")
