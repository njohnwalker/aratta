module Language.GCL.SMTLib
where

import Control.Monad ( forM )

import Data.Set
import Data.Text ( Text,  unpack )
import qualified SimpleSMT as SMT
import qualified SMTIO

import Language.GCL.Syntax.Abstract
import Language.GCL.Environment ( getClosureBool )
import Language.GCL.Semantics.SmallStep

boolToSExpr :: BExp -> SMT.SExpr
boolToSExpr = \case
  -- Logical connectives
  BConst b -> SMT.bool b
  Not bexp -> SMT.not $ boolToSExpr bexp
  b1 :&: b2 -> boolToSExpr b1 `SMT.and`  boolToSExpr b2
  b1 :|: b2 -> boolToSExpr b1 `SMT.or` boolToSExpr b2
  b1 :=>: b2 -> SMT.not (boolToSExpr b1) `SMT.or` boolToSExpr b2
  -- Relations
  i1 :<=: i2 -> intToSExpr i1 `SMT.leq` intToSExpr i2
  i1 :>=: i2 -> intToSExpr i1 `SMT.geq` intToSExpr i2
  i1 :<: i2  -> intToSExpr i1 `SMT.lt`  intToSExpr i2
  i1 :>: i2  -> intToSExpr i1 `SMT.gt`  intToSExpr i2
  i1 :==: i2 -> intToSExpr i1 `SMT.eq`  intToSExpr i2

intToSExpr :: IExp -> SMT.SExpr
intToSExpr = \case
  Var v -> SMT.const $ unpack $ getVariableText v
  IConst i -> SMT.int i
  i1 :+: i2 -> intToSExpr i1 `SMT.add` intToSExpr i2
  i1 :-: i2 -> intToSExpr i1 `SMT.sub` intToSExpr i2
  i1 :%: i2 -> intToSExpr i1 `SMT.mod` intToSExpr i2
  i1 :/: i2 -> intToSExpr i1 `SMT.div` intToSExpr i2
  i1 :*: i2 -> intToSExpr i1 `SMT.mul` intToSExpr i2

boolToSMTAssertion :: SMT.Solver -> BExp -> IO ()
boolToSMTAssertion solver bexp
  = SMT.assert solver $ boolToSExpr bexp

boolToSMTAssertionWithHeader :: SMT.Solver -> BExp -> IO ()
boolToSMTAssertionWithHeader solver bexp
  =  declareHeader solver names
  >> SMT.assert solver (boolToSExpr bexp)
  where names = getClosureBool bexp

declareHeader :: SMT.Solver -> Set Variable -> IO ()
declareHeader solver = mapM_ \name ->
  SMT.declare solver (unpack $ getVariableText name) SMT.tInt

-------------------------
-- interactive helpers --
debugSolverSatCheck :: SMT.Solver -> BExp -> IO ()
debugSolverSatCheck solver bexp = SMT.inNewScope solver $ do
  boolToSMTAssertionWithHeader solver bexp
  result <- SMT.check solver
  case result of
    SMT.Sat -> do
      env <- SMTIO.getModel solver
             $ Data.Set.map getVariableText
             $ getClosureBool bexp
      putStrLn $ unlines
        [ "SMT: SAT with model:"
        , show env
        , "Eval: " ++ show (evalBool env bexp)
        ]
    SMT.Unsat ->
      putStrLn $ unlines
      [ "SMT: Unsat"
      , "Eval (with arbitrary environment)"
        ++ show (Prelude.not $ evalBoolAny bexp)
      ]
    SMT.Unknown -> putStrLn "Unkown result from solver"
