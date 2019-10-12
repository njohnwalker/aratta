module Test.Language.GCL.SMTLib
where


import Control.Monad.IO.Class ( MonadIO )
import Control.Exception (IOException,  try)

import Test.Hspec

import Test.Tasty
import Test.Tasty.Golden ( goldenVsFile )

import qualified Hedgehog as Hog

import Data.Set as Set ( fromList )

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
hprop_boolToSExpr =  Hog.property $ do
  solver <- Hog.evalIO newZ3Solver
  bExp <- Hog.forAll Gen.bool
  eResult <- hogEvalIOEither $ do
      boolToSMTAssertion solver bExp
      check solver
  case eResult of
    Right Sat -> do
      eEnv <- hogEvalIOEither $ getModel solver $ getClosureBool bExp
      case eEnv of
        Right env -> Hog.assert $ GCL.evalBool env bExp
        _ -> Hog.discard
    Right Unsat -> Hog.assert $ Prelude.not $ GCL.evalBoolAny bExp
    _ -> Hog.discard

-- hmmm...
hogEvalIOEither :: (Hog.MonadTest m, MonadIO m, HasCallStack) => IO a -> m (Either IOException a)
hogEvalIOEither io = Hog.evalIO $ try io

replayFDException :: IO ()
replayFDException =
  Hog.recheck (Hog.Size 88) (Hog.Seed 2852360753211302202 8158979062945036807) hprop_boolToSExpr
