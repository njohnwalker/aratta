module Test.Language.GCL.Syntax.Parser
where

import Control.Monad ( forM )

import System.FilePath ( takeBaseName, replaceExtension)

import qualified Data.Text as Text ( pack )
import qualified Data.Text.IO as Text ( readFile )

import qualified Data.Text.Lazy as TL ( fromStrict )
import qualified Data.Text.Lazy.Encoding as TL (encodeUtf8)

import Test.Tasty
import Test.Tasty.Golden ( goldenVsString, findByExtension )

import Language.GCL


test_parseUnparseGCL :: IO TestTree
test_parseUnparseGCL = do
  srcFilePaths <- findByExtension [".gcl"] "res"
  return $ testGroup "Parse/UnParse Golden Tests"
    [ goldenVsString (takeBaseName filePath) (replaceExtension filePath ".golden")
      ( TL.encodeUtf8
      . TL.fromStrict
      . (either Text.pack renderGCLPretty)
      . parseGCL filePath
      <$> Text.readFile filePath
      )
    | filePath <- srcFilePaths
    ]

