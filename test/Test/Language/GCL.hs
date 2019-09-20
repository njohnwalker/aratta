module Test.Language.GCL
where

import Test.Hspec

import Test.Tasty
import Test.Tasty.Golden ( goldenVsFile )

import qualified Hedgehog as Hog

import Data.Set as Set (fromList)

import SimpleSMT

import SMTIO
import                Language.GCL as GCL
import qualified Test.Language.GCL.Gen as Gen

spec_simpleSMT :: Spec
spec_simpleSMT = before newZ3Solver $ do
  it "runs a trivial smt solver instance" \_ -> True

spec_simpleBoolToSExpr :: Spec
spec_simpleBoolToSExpr = let bexp = Var "a" :<=: Var "b"
  in before newZ3Solver $ do
  it "runs a simple smt query and \
     \asserts the boolean is satisfied by the model"
     \solver -> do
       boolToSMTAssertion solver bexp
       check solver
       env <- getModel solver $ Set.fromList ["a","b"]
       evalBool env bexp `shouldBe` True

hprop_boolToSExpr :: Hog.Property
hprop_boolToSExpr =  Hog.property do
  solver <- Hog.evalIO newZ3Solver
  bExp <- Hog.forAll Gen.bool
  result <- Hog.evalIO $ do
      boolToSMTAssertion solver bExp
      check solver
  case result of
    Sat -> do
      env <- Hog.evalIO $ getModel solver $ getClosureBool bExp
      Hog.assert $ GCL.evalBool env bExp
    Unsat -> Hog.assert $ Prelude.not $ GCL.evalBoolAny bExp
    Unknown -> Hog.discard
