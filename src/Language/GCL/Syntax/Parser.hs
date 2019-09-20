module Language.GCL.Syntax.Parser
  ( parseGCL
  , programParser
  , gcs
  , gc
  , stmt
  , iExp
  , bExp )
where

import           Data.Void
import           Data.Text ( Text )
import qualified Data.Text as T

import Prelude hiding (and, not, or)


import Text.Megaparsec
       ( Parsec, try, notFollowedBy
       , many, between, eof, (<|>)
       , sepBy, parse, errorBundlePretty
       )

import Control.Monad.Combinators.Expr

import Language.GCL.Syntax.Abstract
import Language.GCL.Syntax.Lexer

type Parser = Parsec Void Text

------------------------
-- Source File Parser --
parseGCL :: String -> Text -> Either String Statement
parseGCL file input = case parse programParser file input of
  Left  err -> Left $ errorBundlePretty err
  Right cSet -> Right cSet

---------------------
-- Guarded Commands and Command Sets --

programParser :: Parser Statement
programParser = between space eof stmt

gcs :: Parser GuardedCommandSet
gcs =  GCS <$> sepBy gc square

gc :: Parser GuardedCommand
gc = GC <$> bExp <* arrow <*> stmt

----------------
-- Statements --

stmt :: Parser Statement
stmt = Seq <$> sepBy stmt' semicolon

stmt' :: Parser Statement
stmt'
  =  assignStmt
 <|> doStmt
 <|> ifStmt
 
assignStmt, doStmt, ifStmt :: Parser Statement
assignStmt = do
  varList <- sepBy identifier comma
  _       <- assign
  expList <- sepBy iExp comma
  if length varList /= length expList
    then fail "Concurrent variable assignment list lengths do not match"
    else return $ varList := expList
  
doStmt = Do <$> doBlock gcs
ifStmt = If <$> ifBlock gcs

-----------------
-- Expressions --

-- Arithmetic
iExp :: Parser IExp
iExp = makeExprParser iTerm iOpTable

iOpTable :: [[Operator Parser IExp]]
iOpTable
  = [ [ InfixR $ (:-:) <$ minus ]
    , [ InfixR $ (:+:) <$ plus ]
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
    , [ InfixR $ (:|:) <$ or]
    , [ InfixR $ (:&:) <$ and ]
    ]
    
bTerm :: Parser BExp
bTerm
  =  parentheses bExp
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
