module SMTIO
where

import qualified Data.Map as Map
import Data.Set as Set ( Set, toList, null)
import Data.Text (Text, pack, unpack )

import SimpleSMT

newZ3Solver :: Int -> IO Solver
newZ3Solver timeout = do
  logger <- newLogger 0
  solver <- newSolver
            "/usr/bin/z3"
            ["-smt2", "-in", "-T:" ++ show timeout]
            $ Just logger
  setLogic solver "QF_NIA"
  return solver

newCVC4Solver :: Int -> IO Solver
newCVC4Solver timeout = do
  solver <- newSolver "/usr/bin/cvc4"
            [ "--lang=smt2"
            , "-i"
            , "--tlimit-per=" ++ show (timeout * 1000)
            ] Nothing
  setLogic solver "QF_NIA"
  return solver

newCVC4SolverWithLogger :: Int -> IO Solver
newCVC4SolverWithLogger timeout = do
  logger <- newLogger 0
  solver <- newSolver "/usr/bin/cvc4"
            [ "--lang=smt2"
            , "-i"
            , "--tlimit-per=" ++ show (timeout * 1000)
            ] $ Just logger
  setLogic solver "QF_NIA"
  return solver


getModel :: Solver -> Set Text ->  IO (Map.Map Text Integer)
getModel _ (Set.null -> True) = return Map.empty
getModel solver closure
  =   Map.fromList
  .   map (\(name, val) -> (pack name, intFromVal val))
  <$> getConsts solver (map unpack $ toList closure)
  where
    intFromVal :: Value -> Integer
    intFromVal (Int i) = i
    intFromVal _ = error "Model contains non-integer constants"
