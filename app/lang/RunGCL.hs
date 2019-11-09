module RunGCL
  ( runGCL )
where

import Prelude hiding (readFile, readIO)
import qualified Prelude as P ( readFile, readIO )
import Data.Text ( Text )
import qualified Data.Text.IO as T ( putStrLn )

import Data.Text.Prettyprint.Doc.Render.Text ( renderStrict )
import Data.Text.Prettyprint.Doc (PageWidth(..), LayoutOptions(..),  pretty, defaultLayoutOptions, layoutSmart )

import Options.Applicative.MainOptions 
--import Language.GCL.Syntax.Parser ( parseGCL )
import Language.GCL ( parseGCL, renderPretty )
import Language.GCL.Semantics.PredicateTransformer ()


runGCL :: Text -> MainOptions -> IO ()
runGCL srcText options = do
  { let file = sourceFile options
        semantics = semanticModel options
        mInFile = inputFile options
  ; putStrLn $ "Parsing file \'" ++ file ++ "\'"
  ; case parseGCL file srcText of
      Left errorOut -> putStr errorOut
      Right pgm -> do
        T.putStrLn $ renderPretty pgm
  }
