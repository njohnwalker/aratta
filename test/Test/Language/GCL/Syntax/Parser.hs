module Test.Language.GCL.Syntax.Parser
where

import Control.Monad ( forM )

import System.FilePath ( takeBaseName, replaceExtension)

import qualified Data.Text as Text ( pack )
import qualified Data.Text.IO as Text ( readFile )

import qualified Data.Text.Lazy as TL ( fromStrict )
import qualified Data.Text.Lazy.Encoding as TL (encodeUtf8)


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
            . (either Text.pack renderGCLPretty)
            <$> readAndParseGCL filePath
          )
    | filePath <- srcFilePaths
    ]
  where diffCmd ref new = ["diff", "-u", ref, new]

spec_parseIdentityGCL :: Spec
spec_parseIdentityGCL =
  before parseAllGCLSources $ do
  it "parse . pretty . parse = parse"
     \pathASTPairs -> do
       [parseGCL path $ renderGCLPretty ast | (path, ast) <- pathASTPairs]
       `shouldMatchList` map (Right . snd) pathASTPairs
  where
    parseAllGCLSources = do
      srcFilePaths <- findByExtension [".gcl"] "res/gcl"
      srcTexts <- forM srcFilePaths Text.readFile
      return [ (path, pgm)
             | (path, Right pgm) <-
                 map (\(path, text) -> (path, parseGCL path text))
                 $ zip srcFilePaths srcTexts
             ]
