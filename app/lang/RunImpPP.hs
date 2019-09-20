module RunImpPP
  ( runImpPP )
where

import Prelude hiding (readFile, readIO)
import qualified Prelude as P ( readFile, readIO )
import Data.Text ( Text )
import qualified Data.Text.IO as T ( putStrLn )

import Data.Text.Prettyprint.Doc.Render.Text ( renderStrict )
import Data.Text.Prettyprint.Doc ( pretty, defaultLayoutOptions, layoutSmart )

import Options.Applicative.MainOptions 
import Language.ImpPP.Syntax.Parser ( parseImpPP )
import Language.ImpPP.Semantics.SmallStep ( stepRunImpPP, execPgm )

import Language.Imp.Programs

runImpPP :: Text -> MainOptions -> IO ()
runImpPP srcText options = do
  { let file = sourceFile options
        semantics = semanticModel options
        mInFile = inputFile options
  ; input <- case mInFile of
      Nothing -> return []
      Just inFile -> do 
        putStrLn $ "Parsing input file \'" ++ inFile ++ "\'"
        fList <- P.readFile inFile
        P.readIO fList
  ; putStrLn $ "Parsing file \'" ++ file ++ "\'"
  ; case parseImpPP file srcText of
      Left errorOut -> putStr errorOut
      Right pgm -> do
        T.putStrLn $ renderStrict $ layoutSmart defaultLayoutOptions $ pretty pgm
        putStrLn $ "Executing file \'" ++ file ++ "\'"
        mapM_ (putStrLn . show . map (\(l,r) -> (show l, show r ))) $ stepRunImpPP input pgm
  }
