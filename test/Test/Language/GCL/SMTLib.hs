module Test.Language.GCL.SMTLib
where

import Control.Monad.IO.Class ( MonadIO )
import Control.Exception (IOException,  try, catch)
import Data.GenValidity ( genValid )

import Test.Hspec

import Test.Tasty hiding ( after )
import Test.Tasty.Golden ( goldenVsFile )

import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC.M

import qualified Data.Map as Map
import qualified Data.Set as Set

import SimpleSMT

import SMTIO
import                Language.GCL as GCL

spec_simpleSMT :: Spec
spec_simpleSMT
  = before (newCVC4Solver 30)
  $ after (\solver -> stop solver >> return ())
  $ it "runs a trivial smt solver instance"
  \_ -> True

spec_simpleBoolToSExpr :: Spec
spec_simpleBoolToSExpr = let bexp = Var "a" :<=: Var "b"
  in before (newCVC4Solver 30)
  $ after (\solver -> stop solver >> return ())
  $ it "runs a simple smt query and \
     \asserts the boolean is satisfied by the model"
     \solver -> do
       boolToSMTAssertionWithHeader solver bexp
       check solver
       env <- getModel solver $ Set.fromList ["a","b"]
       evalBool env bexp `shouldBe` Right True

spec_pathologicalBoolToSExpr :: Spec
spec_pathologicalBoolToSExpr = let bexp = 1 :==: 0 :*: (0 :/: 0)
  in before (newCVC4Solver 30)
  $ after (\solver -> stop solver >> return ())
  $ it "runs a pathological division-by-zero smt query and \
     \asserts the boolean is satisfied by the model"
     \solver -> do
       boolToSMTAssertionWithHeader solver bexp
       check solver
       evalBoolAny bexp `shouldBe` Right False


spec_boolToSExpr :: Spec
spec_boolToSExpr = do
  solver <- runIO $ newCVC4Solver 30
  it "generates arbitrary Boolean expressions and checks satisfiability"
    $ QC.property $ boolToSExprProperty solver
  where
    boolToSExprProperty :: Solver -> BExp -> QC.Property
    boolToSExprProperty solver bExp = QC.M.monadicIO $ do
      QC.M.run $ push solver
      mSMTResult <- QC.M.run $ inNewScope solver $ do
            boolToSMTAssertionWithHeader solver bExp
            result <- check solver
            case result of
              Sat -> do
                env <- getModel solver $ getClosure bExp
                return $ Just $ Just env
              Unsat -> return $ Just Nothing
              Unknown -> return Nothing
      return case mSMTResult of
        Nothing -> QC.discard
        Just Nothing ->
          QC.counterexample
          "Solver says: Unsat"
          $ case evalBoolAny $ Not bExp of
              Left DivZero -> QC.discard
              Right sat -> sat
        Just (Just env) ->
          QC.counterexample
          ("Solver says: Sat\n\
           \with model:  "++ show (Map.toList env)
          ) $ case evalBool env bExp of
                Left DivZero -> QC.discard
                Right sat -> sat
