module Main where

import Options.Generic

import Data.Char
import qualified Data.Text.IO as Text

import Language.GCL
import RunGCL

-- | Generic Options Type for houdini-verify cli
data HoudiniOptions w = HoudiniOptions
  { srcFlag :: (w ::: FilePath       <?> "Source file to verify")
  , invFlag :: (w ::: Maybe FilePath <?> "File containing candidate invariant\
                                       \list to use in verification")
  ,  wpFlag :: (w ::: Bool <?> "use weakest precondition predicate transformation")
  ,  spFlag :: (w ::: Bool <?> "use strongest postcondition predicate transformation\
                               \[default]")
  , verboseFlag :: (w ::: Bool <?> "print detailed solving info")
  } deriving(Generic)
instance ParseRecord (HoudiniOptions Wrapped) where
  parseRecord = parseRecordWithModifiers
                defaultModifiers
                { fieldNameModifier = takeWhile isLower
                , shortNameModifier = \case 'v':cs -> Just 'v' ; _ -> Nothing
                }

deriving instance Show (HoudiniOptions Unwrapped)


main :: IO ()
main =  do
  opts@(HoudiniOptions srcPath mInvListPath isWP isSP isVerbose)
      <- unwrapRecord "houdini-verify"

  srcText <- Text.readFile srcPath

  runGCL
    srcText
    srcPath
    mInvListPath
    (if not isWP || isSP then spPath else wpPath)
    isVerbose
