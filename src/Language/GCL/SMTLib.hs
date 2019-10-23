module Language.GCL.SMTLib
where

import Control.Monad ( forM )

import Data.Set
import Data.Text ( Text,  unpack )
import qualified SimpleSMT as SMT

import Language.GCL.Syntax.Abstract
import Language.GCL.Environment ( getClosureBool )

boolToSExpr :: BExp -> SMT.SExpr
boolToSExpr = \case
  BConst b -> SMT.bool b
  Not bexp -> SMT.not $ boolToSExpr bexp
  b1 :&: b2 -> SMT.and (boolToSExpr b1) $ boolToSExpr b2
  i1 :<=: i2 -> SMT.leq (intToSExpr i1) $ intToSExpr i2

intToSExpr :: IExp -> SMT.SExpr
intToSExpr = \case
  Var v -> SMT.const $ unpack  v
  IConst i -> SMT.int i
  i1 :-: i2 -> SMT.sub (intToSExpr i1) $ intToSExpr i2
  i1 :%: i2 -> SMT.mod (intToSExpr i1) $ intToSExpr i2
  i1 :/: i2 -> SMT.div (intToSExpr i1) $ intToSExpr i2
  i1 :*: i2 -> SMT.mul (intToSExpr i1) $ intToSExpr i2

boolToSMTAssertion :: SMT.Solver -> BExp -> IO ()
boolToSMTAssertion solver bexp
  =  declareHeader solver names
  >> SMT.assert solver (boolToSExpr bexp)
  where names = getClosureBool bexp

declareHeader :: SMT.Solver -> Set Text -> IO ()
declareHeader solver = mapM_ \name ->
  SMT.declare solver (unpack name) SMT.tInt

data Validity = Valid | Invalid BExp
  deriving (Eq, Ord, Show)

checkValidVCs :: SMT.Solver -> [BExp] -> IO Validity
checkValidVCs solver vcs = do
  results <- forM vcs $ \vc -> SMT.inNewScope solver $ do
    boolToSMTAssertion solver $ Not vc
    res <- SMT.check solver
    return (vc, res)
  return $ smtResultsToValidity results
  where
    smtResultsToValidity :: [(BExp, SMT.Result)] -> Validity
    smtResultsToValidity []                     =   Valid
    smtResultsToValidity ((vc, SMT.Sat    ): _) = Invalid vc
    smtResultsToValidity ((vc, SMT.Unknown): _) = Invalid vc
    smtResultsToValidity (( _, SMT.Unsat  ):rs) = smtResultsToValidity rs
