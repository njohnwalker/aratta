module Test.Language.GCL.Semantics.PredicateTransformer
where

import qualified Data.Text.IO as T.IO

import Test.Hspec

import Language.GCL

----------------------
-- Basic Path Tests --
spec_trivial_basicpath :: Spec
spec_trivial_basicpath = 
  it "VC of program with no branches or loops" do
  ePgm  <- readAndParseGCL "res/gcl/trivial-basicpath.gcl"
  case ePgm of
    Left err -> err `shouldBe` "a Correct Parse"
    Right pgm ->
      specifyInvariant (BConst False) (getBasicPathVCs pgm) `shouldBe`
      let [x,y,z] = map Var ["x","y","z"]
      in [x :<=: y :=>: z :<=: z :-: 7
           :&: 30 :+: 14 :-: x :<=: z :-: 7
           :&: 30 :+: 14 :-: x :<=: z
           :&: z :<=: 10]
         
spec_max_basicpaths :: Spec
spec_max_basicpaths =
  it "VC of max program (only branches, no loops)" do
  ePgm  <- readAndParseGCL "res/gcl/max.gcl"
  case ePgm of
    Left err -> err `shouldBe` "a Correct Parse"
    Right pgm ->
      specifyInvariant (BConst False) (getBasicPathVCs pgm) `shouldMatchList`
      let [x,y,m] = map Var ["x","y","m"]
      in [ x :>=: y :=>: x :>=: x :&: x :>=: y
         , y :>=: x :=>: y :>=: x :&: y :>=: y
         , Not (x :>=: y) :&: Not (y :>=: x) :=>: m :>=: x :&: m :>=: y
         ]
  

-----------------------
-- Weakest Pre tests --
spec_wpAssume :: Spec
spec_wpAssume =
  it "wp of assume statement is implication" do
  wp (Assume $ BConst True) (BConst False) `shouldBe` (BConst True :=>: BConst False)

spec_wpSubstitute :: Spec
spec_wpSubstitute =
  it "wp of substitute statement is the substituted expression" do
  let exp = (10  :<=: Var "x") :&: BConst True
      exp' = (10 :<=: 10) :&: BConst True
  wp (Substitute "x" $ 10) exp `shouldBe` exp'

spec_wpSequence :: Spec
spec_wpSequence =
  it "wp of a sequence of statements is the composition of the statements" do
  let x = Var "x"
      y = Var "y"
      z = Var "z"
      path =
        [ Assume $ x :<=: y
        , Substitute "y" $ 30 :+: 14 :-: x
        , Substitute "x" 17
        , Assume $ y :<=: x
        , Substitute "x" $ z :-: 7
        ]
      ensures = z :<=: x :&: y :<=: x :&: y :<=: z :&: z :<=: 10
      weakestPrecondition = x :<=: y
        :=>: 30 :+: 14 :-: x :<=: 17
        :=>: z :<=: z :-: 7
          :&: 30 :+: 14 :-: x :<=: z :-: 7
          :&: 30 :+: 14 :-: x :<=: z
          :&: z :<=: 10
  wpSeq path ensures `shouldBe` weakestPrecondition

------------------------
-- Substitution Tests --
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
  substitute "x" (Var "x") exp `shouldBe` exp

spec_substitute_contrived :: Spec
spec_substitute_contrived =
  it "substitute performs a nested, multiple, non-idempotent substitution" do
  let exp = (Var "x" :<=: IConst 22)
            :&: Not (Not (Var "z" :<=: (Var "x" :-: (IConst 0 :-: IConst 12)))
                     :&: Not (Not (Var "x" :<=: Var "x")))
      exp' = ((IConst 42 :-: Var "x") :<=: IConst 22)
             :&: Not (Not (Var "z" :<=: ((IConst 42 :-: Var "x") :-: (IConst 0 :-: IConst 12)))
                      :&: Not (Not ((IConst 42 :-: Var "x") :<=: (IConst 42 :-: Var "x"))))
  substitute "x" (IConst 42 :-: Var "x") exp `shouldBe` exp'
