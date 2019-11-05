module SMTIO
  ( newZ3Solver
  , getModel
  )
where

import Control.Lens

import qualified Data.Map as Map
import Data.Set as Set ( Set, toList, null)
import Data.Text (Text, pack, unpack )

import SimpleSMT

newZ3Solver :: Int -> IO Solver
newZ3Solver timeout = do
  solver <- newSolver "/usr/bin/z3" ["-smt2", "-in", "-T:" ++ show timeout] Nothing
  setOption solver ":produce-models" "true"
  return solver
              
getModel :: Solver -> Set Text ->  IO (Map.Map Text Integer)
getModel _ set | Set.null set = return Map.empty
getModel solver closure
  =   Map.fromList
  .   map (\(name, val) -> (pack name, intFromVal val))
  <$> getConsts solver (map unpack $ toList closure)
  where
    intFromVal :: Value -> Integer
    intFromVal (Int i) = i
    intFromVal _ = error "Model contains non-integer constants"
