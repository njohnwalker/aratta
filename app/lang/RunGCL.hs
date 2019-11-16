module RunGCL
  ( runGCL )
where

import           Prelude hiding (readFile, readIO)
import qualified Prelude as P
import qualified Data.Set as Set
import           Data.Text ( Text )
import qualified Data.Text.IO as T ( putStrLn, readFile )
import qualified System.Exit      as System
import qualified System.Directory as System

import Options.Applicative.MainOptions 
import Language.GCL

runGCL :: Text -> MainOptions -> IO ()
runGCL srcText options = do
  { let file = sourceFile options
        semantics = semanticModel options
        mInFile = inputFile options
        mInvariantFile = invariantFile options
  ; putStrLn $ "Parsing file \'" ++ file ++ "\'..."
  ; case parseGCL file srcText of
      Left errorOut -> putStrLn "Parsing Failed!"
                       >> putStrLn errorOut
                       >> System.exitFailure
      Right pgm -> do
        T.putStrLn $ renderPretty $ pretty $ pgm

        putStrLn "Calculating Program's Basicpaths..."

        let pPgmPaths = getProgramPaths pgm
        
        T.putStrLn
          $ renderPretty
          $ pretty
          $ specifyInvariant (Var "INVARIANT" :==: Var "PLACEHOLDER")
          $ pPgmPaths

        let invariantFilePath = maybe (file <> ".inv") id mInvariantFile

        invariantList <- retrieveInvariantList invariantFilePath
        
        validityWP <-
          checkValidVCs
              (newCVC4SolverWithLogger 10)
              (foldl1 (:&:) invariantList)
              (map wpPath <$> pPgmPaths)

        reportValidity validityWP

        validitySP <-
          checkValidVCs
              (newCVC4SolverWithLogger 10)
              (foldl1 (:&:) invariantList)
              (map spPath <$> pPgmPaths)
        
        reportValidity validitySP
        
        System.exitSuccess
  }


retrieveInvariantList :: FilePath -> IO [BExp]
retrieveInvariantList invariantFilePath = do
  putStrLn $ "Reading '"++invariantFilePath
    ++"' for candidate invariants for houdini verification..." 

  fileExists <- System.doesFileExist invariantFilePath
  if not fileExists
    then putStrLn "No invariant file exists! Trying verification without invariant..."
         >> return [BConst (error "no invariant specified")]
    else do
      invText <- T.readFile invariantFilePath
      putStrLn $ "Parsing invariants from '"++invariantFilePath++"'"
  
      case parseGCLInvariantList invariantFilePath invText of
        Left err -> putStrLn "Parsing Invariants Failed!" >> putStrLn err
                    >> System.exitFailure
        Right invariants -> return invariants
      

reportValidity :: Validity -> IO ()
reportValidity = \case
  Valid -> putStrLn "Program Validated"
  Invalid vc counterexample -> do
    putStrLn "Verification failed, VC invalid:"
    T.putStrLn $ "    " <> (renderPretty $ pretty $ vc)
    putStrLn "With counterexample:"
    T.putStrLn $ "    " <> (renderPretty $ pretty $ counterexample)
    putStrLn $ "Evaluates to: " ++ show (evalBool counterexample vc)
    System.exitFailure
  UnknownValidity vc -> do
    putStrLn "Verification failed,\n smt solver returned Unknown result:"
    T.putStrLn $ "\t" <> (renderPretty $ pretty $ Not vc)
    System.exitFailure
