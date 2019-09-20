module SMTIO
  ( newZ3Solver
  , getModel
  )
where

import Data.Set as Set ( Set, toList, null)
import Data.Text (Text, pack, unpack )

import SimpleSMT

import Language.GCL.Environment ( Env, fromList, initialEnv )

newZ3Solver :: IO Solver
newZ3Solver = do
  solver <- newSolver "/usr/bin/z3" ["-smt2", "-in"] Nothing
  setOption solver ":produce-models" "true"
  return solver
              

getModel :: Solver -> Set Text ->  IO Env
getModel _ set | Set.null set = return initialEnv
getModel solver closure
  =   fromList
  .   map (\(name, val) -> (pack name, intFromVal val))
  <$> getConsts solver (map unpack $ toList closure)
  where
    intFromVal :: Value -> Integer
    intFromVal (Int i) = i
    intFromVal _ = error "Model contains non-integer constants"
