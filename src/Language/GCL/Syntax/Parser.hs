module Language.GCL.Syntax.Parser
  ( readAndParseGCL
  , parseGCL
  , parseGCLInvariantList
  , programParser
  , ifGcs
  , doGcs
  , gc
  , stmt
  , iExp
  , bExp )
where

import           Data.Void
import           Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO ( readFile )

import Prelude hiding (and, not, or)

import Text.Megaparsec
       ( Parsec, try, notFollowedBy
       , many, some, between, eof, (<|>)
       , sepBy, sepBy1, sepEndBy, parse, errorBundlePretty
       , optional
       )

import Control.Monad.Combinators.Expr

import Language.GCL.Syntax.Abstract
import Language.GCL.Syntax.Lexer as Lex hiding ( Parser )

type Parser = Parsec Void Text

------------------------
-- Source File Parser --

readAndParseGCL :: FilePath -> IO (Either String GCLProgram)
readAndParseGCL filepath
  = parseGCL filepath <$> T.IO.readFile filepath

parseGCL :: String -> Text -> Either String GCLProgram
parseGCL file input = case parse programParser file input of
  Left  err -> Left $ errorBundlePretty err
  Right cSet -> Right cSet

programParser :: Parser GCLProgram
programParser = between space eof block

---------------------------
-- Invariant File Parser --
parseGCLInvariantList :: String -> Text -> Either String [BExp]
parseGCLInvariantList file input =
  either (Left . errorBundlePretty) Right
  $ parse invariantListParser file input

invariantListParser :: Parser [BExp]
invariantListParser = between space eof (sepBy1 bExp space)

-----------------
-- GCL Program --
block :: Parser GCLProgram
block = GCLProgram <$> requires <*> many stmt <*> ensures
  where
    requires, ensures :: Parser BExp
    requires = reqTag *> trueOrAnnotation
    ensures = ensTag *> trueOrAnnotation 
    trueOrAnnotation =
      (maybe (BConst True) id <$> annotation)
----------------
-- Statements --
stmt :: Parser Statement
stmt
  =  assignStmt
 <|> doStmt
 <|> ifStmt

assignStmt, doStmt, ifStmt :: Parser Statement
assignStmt = do
  varList <- sepBy identifier comma
  _       <- assign
  expList <- sepBy iExp comma
  _       <- semicolon
  if length varList /= length expList
    then fail "Concurrent variable assignment list lengths do not match"
    else return $ varList := expList

doStmt = do
  _    <- symbol "do"
  inv  <- invTag *> annotation
  body <- doGcs
  _    <- symbol "od"
  return $ Do inv body

ifStmt = If <$> ifBlock ifGcs

---------------------
-- Guarded Commands and Command Sets --

ifGcs :: Parser GuardedCommandSet
ifGcs = GCS <$> sepBy gc square

doGcs :: Parser GuardedCommandSet
doGcs = GCS <$> many (square *> gc)  

gc :: Parser GuardedCommand
gc = GC <$> bExp <* arrow <*> some stmt

-----------------
-- Expressions --

-- Arithmetic
iExp :: Parser IExp
iExp = makeExprParser iTerm iOpTable

iOpTable :: [[Operator Parser IExp]]
iOpTable
  = [ [ InfixL $ (:/:) <$ Lex.div
      , InfixL $ (:*:) <$ mult ]
    , [ InfixL $ (:%:) <$ Lex.mod ]
    , [ InfixL $ (:+:) <$ plus
      , InfixL $ (:-:) <$ minus
      ]
    ]

iTerm :: Parser IExp
iTerm
  =  parentheses iExp
 <|> Var <$> identifier
 <|> IConst <$> integer


-- Logical
bExp :: Parser BExp
bExp = makeExprParser bTerm bOpTable

bOpTable :: [[Operator Parser BExp]]
bOpTable
  = [ [ Prefix $ Not <$ not ]
    , [ InfixR $ (:&:) <$ and ]
    , [ InfixR $ (:|:) <$ or ]
    , [ InfixR $ (:=>:) <$ implies ]
    ]
    
bTerm :: Parser BExp
bTerm
  =  try (parentheses bExp)
 <|> BConst <$> boolean
 <|> try gteRExp
 <|> gtRExp
 <|> try lteRExp
 <|> ltRExp
 <|> eRExp
 
-- Relations

lteRExp = (:<=:) <$> try (iExp <* lequal) <*> iExp
gteRExp = (:>=:) <$> try (iExp <* gequal) <*> iExp
eRExp   = (:==:) <$> try (iExp <* equal) <*> iExp
ltRExp  = (:<:) <$> try (iExp <* lessthan) <*> iExp
gtRExp  = (:>:) <$> try (iExp <* greaterthan) <*> iExp

annotation :: Parser (Maybe BExp)
annotation = (parentheses $ optional bExp)
