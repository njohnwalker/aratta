module Test.Language.GCL.Semantics.PredicateTransformer
where

import Test.Hspec

import Language.GCL

spec_wpAssume :: Spec
spec_wpAssume =
  it "wp of assume statement is implication" do
  wp (Assume $ BConst True) (BConst False) `shouldBe` (BConst True :=>: BConst False)


spec_substituteIExp :: Spec
spec_substituteIExp =
  it "substituteIExp replaces variable occurences with expression" do
  let exp = IConst 10 :-: Var "x"
      exp' = IConst 10 :-: IConst 10
  substituteIExp "x" (IConst 10) exp `shouldBe` exp'

spec_substituteIExpIdentity :: Spec
spec_substituteIExpIdentity =
  it "substituteIExp terminates on identity substitution" do
  let exp = IConst 10 :-: Var "x"
      exp' = IConst 10 :-: Var "x"
  substituteIExp "x" (Var "x") exp `shouldBe` exp'

spec_substitute :: Spec
spec_substitute =
  it "substitute replaces variable occurences with expression" do
  let exp = (IConst 10 :<=: Var "x") :&: BConst True
      exp' = (IConst 10 :<=: IConst 10) :&: BConst True
  substitute "x" (IConst 10) exp `shouldBe` exp'

spec_substituteIdentity :: Spec
spec_substituteIdentity =
  it "substitute terminates on identity substitution" do
  let exp = (IConst 10 :<=: Var "x") :&: BConst True
      exp' = (IConst 10 :<=: Var "x") :&: BConst True
  substitute "x" (Var "x") exp `shouldBe` exp'

