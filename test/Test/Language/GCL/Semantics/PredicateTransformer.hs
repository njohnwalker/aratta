module Test.Language.GCL.Semantics.PredicateTransformer
where

import System.FilePath ( takeBaseName, replaceExtension)

import qualified Data.Text as Text 
import qualified Data.Text.IO as T.IO
import qualified Data.Text.Lazy as TL ( fromStrict )
import qualified Data.Text.Lazy.Encoding as TL (encodeUtf8)

import Test.Hspec
import Test.Tasty
import Test.Tasty.Golden ( goldenVsStringDiff, findByExtension )

import Language.GCL

------------------------
-- VC validity checks --
validitySpec
  :: String -- ^ test description
  -> FilePath -- ^ gcl source file
  -> BExp -- ^ candidate invariant
  -> Validity -- ^ expected result
  -> Spec
validitySpec description pgmPath invariant expected =
  it description do
  readAndParseGCL pgmPath >>= \case
    Left err -> err `shouldBe` "a correct parse"
    Right pgm ->
      let closure = getClosure pgm
          pVCs = getBasicPathVCs pgm
      in do
        solver <- newZ3Solver 30
        checkValidVCs solver closure invariant pVCs
          `shouldReturn` expected

spec_max_validity :: Spec
spec_max_validity = validitySpec
  "asserts the validity of the max.gcl program's VCs"
  "res/gcl/max.gcl"
  (BConst undefined) -- not evaluated
  Valid

spec_peasants_multiplication_validity :: Spec
spec_peasants_multiplication_validity = validitySpec
  "asserts the validity of the (redundant code) \
  \peasants-multiplication.gcl program's VCs"
  "res/gcl/peasants-multiplication.gcl"
-- invariant a * b + res == x * y
  (a :*: b :+: res :==: x :*: y :&: a :>=: 0)
  Valid
  where [a,b,res,x,y] = map Var ["a","b","res","x","y"]

spec_peasants_multiplication_serial_validity :: Spec
spec_peasants_multiplication_serial_validity = validitySpec
  "asserts the validity of the serial version of \
  \peasants-multiplication.gcl program's VCs"
  "res/gcl/peasants-multiplication.gcl"
-- invariant a * b + res == x * y
  (a :*: b :+: res :==: x :*: y :&: a :>=: 0)
  Valid
  where [a,b,res,x,y] = map Var ["a","b","res","x","y"]

spec_peasants_multiplication_concurrent_validity :: Spec
spec_peasants_multiplication_concurrent_validity = validitySpec
  "asserts the validity of the concurrent version of \
  \peasants-multiplication-concurrent.gcl program's VCs"
  "res/gcl/peasants-multiplication-concurrent.gcl"
-- invariant a * b + res == x * y
  (a :*: b :+: res :==: x :*: y :&: a :>=: 0)
  Valid
  where [a,b,res,x,y] = map Var ["a","b","res","x","y"]


----------------------
-- Basic Path Tests --
test_basicpath_golden :: IO TestTree
test_basicpath_golden = do
  srcFilePaths <- findByExtension [".gcl"] "res/gcl"
  return $ testGroup "Basicpath Golden Tests"
    [ goldenVsStringDiff
          (takeBaseName filePath)
          diffCmd
          (replaceExtension filePath "basicpath.golden")
          ( TL.encodeUtf8
            . TL.fromStrict
            . (either
                Text.pack
                ( renderPretty
                . specifyInvariant
                  (Var "INVARIANT" :>: Var "PLACEHOLDER")
                . getBasicPath )
              ) <$> readAndParseGCL filePath
          )
    | filePath <- srcFilePaths
    ]
  where diffCmd ref new = ["diff", "-u", ref, new]

-- non-golden basicpath tests
spec_simultaneous_assignment_basicpath :: Spec
spec_simultaneous_assignment_basicpath =
  it "Simultaneous assignment is not serialized" $ do
  let [Right actual] = getVCs (Var "a" :<=: Var "b") [["a","b"] := [Var "b", Var "a"]]
  actual `shouldBe` Var "b" :<=: Var "a"
  where
    getVCs p stmt =
      map wpSeq
      $ specifyInvariant undefined
      $ getPaths p stmt

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
  let [x,y,z] = map Var ["x","y","z"]
      ensures = z :<=: x :&: y :<=: x :&: y :<=: z :&: z :<=: 10
      path = fromPostAndList
        ensures
        [ Assume $ x :<=: y
        , Substitute ["y"] [30 :+: 14 :-: x]
        , Substitute ["x"] [17]
        , Assume $ y :<=: x
        , Substitute ["x"] [z :-: 7]
        ]
      weakestPrecondition = x :<=: y
        :=>: 30 :+: 14 :-: x :<=: 17
        :=>: z :<=: z :-: 7
          :&: 30 :+: 14 :-: x :<=: z :-: 7
          :&: 30 :+: 14 :-: x :<=: z
          :&: z :<=: 10
  let Right actual = wpSeq path
  actual `shouldBe` weakestPrecondition

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
