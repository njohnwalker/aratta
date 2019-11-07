module Test.Language.GCL.Semantics.SmallStep
where

import Control.Monad.IO.Class ( MonadIO )
import Control.Exception (IOException,  try)
import Data.GenValidity ( genValid )

import Test.Hspec

import Test.Tasty
import Test.Tasty.Golden ( goldenVsFile )

import                Language.GCL as GCL

spec_eMod_edge_cases :: Spec
spec_eMod_edge_cases =
  it "eMod follows Euclid's construction (and SMTLib)"
  $ sequence_
  $ zipWith3 (\m n res -> m `eMod` n `shouldBe` res)
  [ -1,  1, -1]
  [  2, -3,  3]
  [  1,  1,  2]

spec_eDiv_edge_cases :: Spec
spec_eDiv_edge_cases =
  it "eDiv follows Euclid's construction (and SMTLib)"
  $ sequence_
  [  (-2) `eDiv` (-2439) `shouldBe` 1
  ]
