module Test.Language.GCL.Syntax.Parser
where

import Control.Monad ( forM )

import System.FilePath ( takeBaseName, replaceExtension)

import qualified Data.Text as Text ( pack )
import qualified Data.Text.IO as Text ( readFile )

import qualified Data.Text.Lazy as TL ( fromStrict )
import qualified Data.Text.Lazy.Encoding as TL ( encodeUtf8 )

import Test.Hspec
import Test.Tasty
import Test.Tasty.Golden ( goldenVsStringDiff, findByExtension )

import Language.GCL


test_prettyGCL :: IO TestTree
test_prettyGCL = do
  srcFilePaths <- findByExtension [".gcl"] "res/gcl"
  return $ testGroup "Parse/UnParse Golden Tests"
    [ goldenVsStringDiff
          (takeBaseName filePath)
          diffCmd
          (replaceExtension filePath ".golden")
          ( TL.encodeUtf8
            . TL.fromStrict
            . (either Text.pack (renderPretty . pretty))
            <$> readAndParseGCL filePath
          )
    | filePath <- srcFilePaths
    ]
  where diffCmd ref new = ["diff", "-u", ref, new]

spec_parseIdentityGCL :: Spec
spec_parseIdentityGCL =
  before parseAllGCLSources $
  it "'parse . pretty . parse = parse' for gcl source-files"
     \pathASTPairs ->
       [parseGCL path $ renderPretty $ pretty ast | (path, ast) <- pathASTPairs]
       `shouldMatchList` map (Right . snd) pathASTPairs
  where
    parseAllGCLSources = do
      srcFilePaths <- findByExtension [".gcl"] "res/gcl"
      srcTexts <- forM srcFilePaths Text.readFile
      return [ (path, pgm)
             | (path, Right pgm) <-
                 zipWith (\path text -> (path, parseGCL path text))
                 srcFilePaths srcTexts
             ]

spec_parseIdentityInvariantList :: Spec
spec_parseIdentityInvariantList =
  before parseAllInvariantListSources $
  it "'parse . pretty . parse = parse' for invariant lists"
  \pathInvariantListPairs ->
    [ parseGCLInvariantList path
      $ renderPretty
      $ prettyList ast
    | (path, ast) <- pathInvariantListPairs
    ] `shouldMatchList` map (Right . snd) pathInvariantListPairs
  where
    parseAllInvariantListSources :: IO [(FilePath, [BExp])]
    parseAllInvariantListSources = do
      srcFilePaths <- findByExtension [".gcl.inv"] "res/gcl"
      srcTexts <- forM srcFilePaths Text.readFile
      return [ (path, pgm)
             | (path, Right pgm) <-
                 zipWith (\path text -> (path, parseGCLInvariantList path text))
                 srcFilePaths srcTexts
             ]
