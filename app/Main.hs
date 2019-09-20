module Main where

import Prelude hiding ( readFile )
import Data.Text.IO ( readFile )
import Text.Megaparsec

import Options.Applicative
import Options.Applicative.Builder.Extra
import Options.Applicative.MainOptions

import RunImp   ( runImp )
import RunImpPP ( runImpPP )
import RunGCL   ( runGCL )

main :: IO ()
main = do
  { options <- commandLineParse
  ; putStrLn $ "Reading file \'" ++ sourceFile options ++ "\'"
  ; srcFile <- readFile $ sourceFile options
  ; (case language options of
      Imp   -> runImp
      ImpPP -> runImpPP 
      GCL   -> runGCL
      _     -> \_ _ -> putStrLn "Language not implemented yet"
    ) srcFile options

  }
