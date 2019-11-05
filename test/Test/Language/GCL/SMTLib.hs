module Test.Language.GCL.SMTLib
where


import Control.Monad.IO.Class ( MonadIO )
import Control.Exception (IOException,  try)
import Data.GenValidity ( genValid )

import Test.Hspec

import Test.Tasty
import Test.Tasty.Golden ( goldenVsFile )

import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC.M

import qualified Data.Map as Map
import qualified Data.Set as Set

import SimpleSMT

import SMTIO
import                Language.GCL as GCL

spec_simpleSMT :: Spec
spec_simpleSMT = before (newZ3Solver 30) $ do
  it "runs a trivial smt solver instance" \_ -> True

spec_simpleBoolToSExpr :: Spec
spec_simpleBoolToSExpr = let bexp = Var "a" :<=: Var "b"
  in before (newZ3Solver 30) $ do
  it "runs a simple smt query and \
     \asserts the boolean is satisfied by the model"
     \solver -> do
       boolToSMTAssertionWithHeader solver bexp
       check solver
       env <- getModel solver $ Set.fromList ["a","b"]
       evalBool env bexp `shouldBe` True


spec_boolToSExpr :: Spec
spec_boolToSExpr = before (newZ3Solver 30) $ do
  it "generates arbitrary Boolean expressions and checks satisfiability"
    \solver -> QC.property $ boolToSExprProperty solver  
  where
    boolToSExprProperty :: Solver -> BExp -> QC.Property
    boolToSExprProperty solver bExp = QC.M.monadicIO $ do
      QC.M.run $ push solver
      mSMTMatchesEval <- QC.M.run . try $ inNewScope solver $ do
        boolToSMTAssertionWithHeader solver bExp
        result <- check solver
        case result of
          Sat -> do
            env <- getModel solver
              $ Set.map getVariableText
              $ getClosureBool bExp
            return $ Just (GCL.evalBool env bExp, Sat)
          Unsat ->
            return $ Just (Prelude.not $ GCL.evalBoolAny bExp, Unsat)
          Unknown -> putStrLn "Unkown result from solver"
            >> return Nothing
      case mSMTMatchesEval of
        Right Nothing -> QC.discard
        Right (Just (smtMatchesEval, result))  ->
          return $ QC.label (show result) smtMatchesEval
        Left (e :: IOException) ->
          QC.M.run $ putStrLn (show e)
          >> QC.discard

