module Language.GCL.SMTLib
  ( module Language.GCL.SMTLib
  , SMT.IO.newCVC4Solver
  , SMT.IO.newCVC4SolverWithLogger
  , SMT.IO.newZ3Solver
  , SMT.IO.debugAssertions
  )
where

import Control.Lens
import Control.Lens.Iso
import Control.Monad ( forM )

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text ( Text, pack, unpack )
import qualified SimpleSMT as SMT
import qualified SMTLib.Environment as SMT
import qualified SMTLib.IO as SMT.IO

import Language.GCL.Syntax.Abstract
import Language.GCL.Environment ( Env, getClosure )
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
  Var v -> SMT.const $ v ^. varEncoded
  IConst i -> SMT.int i
  i1 :+: i2 -> intToSExpr i1 `SMT.add` intToSExpr i2
  i1 :-: i2 -> intToSExpr i1 `SMT.sub` intToSExpr i2
  i1 :%: i2 -> intToSExpr i1 `SMT.mod` intToSExpr i2
  i1 :/: i2 -> intToSExpr i1 `SMT.div` intToSExpr i2
  i1 :*: i2 -> intToSExpr i1 `SMT.mul` intToSExpr i2

varEncoded :: Iso Variable Variable String String
varEncoded = iso variableToSMTSymbol smtSymbolToVariable
  where
    variableToSMTSymbol :: Variable -> String
    variableToSMTSymbol (Variable v) = unpack $ "|" <> v <> "|"

    smtSymbolToVariable :: String -> Variable
    smtSymbolToVariable varString
      = Variable $ pack $ case varString of
          '|':rest -> takeWhile (/='|') rest
          _   -> varString
                                        
boolToSMTAssertion :: SMT.Solver -> BExp -> IO ()
boolToSMTAssertion solver bexp
  = SMT.assert solver $ boolToSExpr bexp

boolToSMTAssertionWithHeader :: SMT.Solver -> BExp -> IO ()
boolToSMTAssertionWithHeader solver bexp
  =  declareHeader solver names
  >> SMT.assert solver (boolToSExpr bexp)
  where names = getClosure bexp

declareHeader :: SMT.Solver -> Set.Set Variable -> IO ()
declareHeader solver = mapM_ \name ->
  SMT.declare solver (name^.varEncoded) SMT.tInt

-- | extract a satisfying smt environment from th solver
getModel :: SMT.Solver -> Set.Set Variable ->  IO SMT.Env
getModel _ (Set.null -> True) = return []
getModel solver closure =
  SMT.getConsts solver
  $ map (view varEncoded)
  $ Set.toList closure

-- | extract a satisfying gcl environment from the solver
smtToGCLModel :: SMT.Env -> Env
smtToGCLModel smtModel = Map.fromList
    [ ( name ^. from varEncoded
      , intFromVal val
      ) 
    | (name, val) <- smtModel
    ]
  where
    intFromVal :: SMT.Value -> Integer
    intFromVal (SMT.Int i) = i
    intFromVal _ = error "Model contains non-integer constants"

-------------------------
-- interactive helpers --
debugSolverSatCheck :: SMT.Solver -> BExp -> IO ()
debugSolverSatCheck solver bexp = SMT.inNewScope solver $ do
  boolToSMTAssertionWithHeader solver bexp
  result <- SMT.check solver
  putStrLn $ unlines
    [ "Debugging satisfiability of Predicate:"
    , "\t" ++ show (pretty bexp)
    ]
  case result of
    SMT.Sat -> do
      env <- fmap smtToGCLModel
             $ getModel solver
             $ getClosure bexp
      putStrLn $ unlines
        [ "SMT: SAT with model:"
        , show env
        , "Eval: " ++ show (evalBool env bexp)
        ]
    SMT.Unsat ->
      putStrLn $ unlines
      [ "SMT: Unsat"
      , "(with arbitrary environment)"
      , "Eval: " ++ show (evalBoolAny $ Not bexp)
      ]
    SMT.Unknown -> putStrLn "Unkown result from solver"
