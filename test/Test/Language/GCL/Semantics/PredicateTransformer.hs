module Test.Language.GCL.Semantics.PredicateTransformer
where

import Data.List ( zipWith5 )
import qualified Data.Text.IO as T.IO

import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec


import Language.GCL

----------------------
-- Basic Path Tests --

basicpathSpec :: String -> FilePath -> BExp -> [BExp] -> Spec
basicpathSpec description pgmPath invariant vcs =
  it description do
  ePgm <- readAndParseGCL pgmPath
  case ePgm of
    Left err -> err `shouldBe` "a correct parse"
    Right pgm ->
      specifyInvariant invariant (getBasicPathVCs pgm)
      `shouldMatchList` vcs

spec_trivial_basicpath :: Spec
spec_trivial_basicpath = basicpathSpec
  "VC of program with no branches or loops"
  "res/gcl/trivial-basicpath.gcl"
  (BConst False)
  let [x,y,z] = map Var ["x","y","z"]
  in [ x :<=: y :=>: z :<=: z :-: 7
       :&: 30 :+: 14 :-: x :<=: z :-: 7
       :&: 30 :+: 14 :-: x :<=: z
       :&: z :<=: 10
     ]

spec_simultaneous_assignment_basicpath :: Spec
spec_simultaneous_assignment_basicpath =
  it "Simultaneous assignment is not serialized"
  do  getVCs (Var "a" :<=: Var "b") [["a","b"] := [Var "b", Var "a"]]
       `shouldBe` [Var "b" :<=: Var "a"]
  where
    getVCs p stmt =
      map (uncurry wpSeq)
      $ specifyInvariant (BConst True)
      $ getPaths p stmt []


spec_max_basicpaths :: Spec
spec_max_basicpaths = basicpathSpec
  "VC of max program (only branches, no loops)"
  "res/gcl/max.gcl"
  (BConst False)
  let [x,y,m] = map Var ["x","y","m"]
  in [ x :>=: y :=>: x :>=: x :&: x :>=: y
     , y :>=: x :=>: y :>=: x :&: y :>=: y
     , Not (x :>=: y) :&: Not (y :>=: x) :=>: m :>=: x :&: m :>=: y
     ]

spec_peasants_multiplication_basicpath :: Spec
spec_peasants_multiplication_basicpath = basicpathSpec
  "VC of peasant's multiplication program (single loop)"
  "res/gcl/peasants-multiplication.gcl"
  (a :+: b:+: x :+: y :+: res :<=: 0) -- garbage invariant mentioning all FVs
  [ x :>=: 0 :&: y :>=: 0 :=>:
    x :+: y :+: x :+: y :+: 0 :<=: 0
  , a :+: b :+: x :+: y :+: res :<=: 0 :=>:
    a :>: 0 :&: a :%: 2 :==: 1 :=>:
    a :+: b :+: x :+: y :+: (res :+: b) :<=: 0
  , a :+: b :+: x :+: y :+: res :<=: 0 :=>:
    a :>: 0 :&: Not (a :%: 2 :==: 1) :=>:
    a :/: 2 :+: b :*: 2 :+: x :+: y :+: res :<=: 0
  , a :+: b :+: x :+: y :+: res :<=: 0 :=>:
    Not (a :>: 0 :&: a :%: 2 :==: 1) :&:
    Not (a :>: 0 :&: Not (a :%: 2 :==: 1)) :=>:
    res :==: x :*: y
  ]
  where [a,b,x,y,res] = map Var ["a","b","x","y","res"]

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
  wp (Substitute ["x"] [10]) exp `shouldBe` exp'

spec_wpSequence :: Spec
spec_wpSequence =
  it "wp of a sequence of statements is the composition of the statements" do
  let x = Var "x"
      y = Var "y"
      z = Var "z"
      path =
        [ Assume $ x :<=: y
        , Substitute ["y"] [30 :+: 14 :-: x]
        , Substitute ["x"] [17]
        , Assume $ y :<=: x
        , Substitute ["x"] [z :-: 7]
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
  let exp = 10 :-: Var "x"
      exp' = 10 :-: 10
  substituteIExp [("x", 10)] exp `shouldBe` exp'

spec_substituteIExpIdentity :: Spec
spec_substituteIExpIdentity =
  it "substituteIExp terminates on identity substitution" do
  let exp = 10 :-: Var "x"
      exp' = 10 :-: Var "x"
  substituteIExp [("x", Var "x")] exp `shouldBe` exp'

spec_substitute :: Spec
spec_substitute =
  it "substitute replaces variable occurences with expression" do
  let exp = (10 :<=: Var "x") :&: BConst True
      exp' = (10 :<=: 10) :&: BConst True
  substitute [("x", 10)] exp `shouldBe` exp'

spec_substituteIdentity :: Spec
spec_substituteIdentity =
  it "substitute terminates on identity substitution" do
  let exp = (10 :<=: Var "x") :&: BConst True
  substitute [("x", Var "x")] exp `shouldBe` exp

spec_substitute_contrived :: Spec
spec_substitute_contrived =
  it "substitute performs a nested, multiple, non-idempotent substitution" do
  let exp = (Var "x" :<=: 22)
            :&: Not (Not (Var "z" :<=: (Var "x" :-: (0 :-: 12)))
                     :&: Not (Not (Var "x" :<=: Var "x")))
      exp' = ((42 :-: Var "x") :<=: 22)
             :&: Not (Not (Var "z" :<=: ((42 :-: Var "x") :-: (0 :-: 12)))
                      :&: Not (Not ((42 :-: Var "x") :<=: (42 :-: Var "x"))))
  substitute [("x", 42 :-: Var "x")] exp `shouldBe` exp'
