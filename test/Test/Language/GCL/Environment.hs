module Test.Language.GCL.Environment
where

import qualified Data.Set as Set

import Test.Hspec

import Language.GCL

spec_getClosure :: Spec
spec_getClosure =
  it "gets all the free variables of the 'every-lexical' gcl program" do
  ePgm <- readAndParseGCL "res/gcl/every-lexical.gcl"
  case ePgm of
    Left err -> err `shouldBe` "a correct parse"
    Right pgm ->
      Set.toList (getClosure pgm)
      `shouldMatchList`
      ["x", "y", "v", "foo", "baz", "z", "catdog", "quux", "c", "ni"]
