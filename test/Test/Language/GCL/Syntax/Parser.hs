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
  srcFilePaths <- findByExtension [".gcl"] "res"
  return $ testGroup "Parse/UnParse Golden Tests"
    [ goldenVsStringDiff (takeBaseName filePath) diffCmd (replaceExtension filePath ".golden")
      ( TL.encodeUtf8
      . TL.fromStrict
      . (either Text.pack renderGCLPretty)
      . parseGCL filePath
      <$> Text.readFile filePath
      )
    | filePath <- srcFilePaths
    ]
  where diffCmd ref new = ["diff", "-u", ref, new]
