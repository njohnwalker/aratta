module Language.GCL.Syntax.Lexer
  ( lexeme
  , symbol
  , space
  , integer
  , boolean
  , parentheses
  , ifBlock
  , doBlock
  , identifier
  , arrow
  , semicolon
  , square
  , assign
  , equal
  , minus
  , plus
  , not
  , and
  , or
  , lequal
  , gequal
  , greaterthan
  , lessthan
  , comma
  )
where

import Prelude hiding ( not, and, or )

import Data.Text hiding (map)
import Data.Void

import Text.Megaparsec ( Parsec, Token, many, between, try, notFollowedBy, (<|>) )
import qualified Text.Megaparsec.Char as PChar
import qualified Text.Megaparsec.Char.Lexer as Lex

type Parser = Parsec Void Text

space :: Parser ()
space = Lex.space PChar.space1 lineComment blockComment
  where
    lineComment  = Lex.skipLineComment "//"
    blockComment = Lex.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme space

symbol :: Text -> Parser Text
symbol = Lex.symbol space

integer :: Parser Integer
integer = Lex.signed space $ lexeme Lex.decimal

boolean :: Parser Bool
boolean = rword "true" *> pure True <|> rword "false" *> pure False

parentheses :: Parser a -> Parser a
parentheses = between (symbol "(") $ symbol ")"

ifBlock :: Parser a -> Parser a
ifBlock = between (symbol "if") $ symbol "fi"

doBlock :: Parser a -> Parser a
doBlock = between (symbol "do") $ symbol "od"


identifier :: Parser Text
identifier = lexeme $ try $ p >>= check
  where
    p       = Data.Text.pack <$> ( (:) <$> PChar.letterChar <*>  many PChar.alphaNumChar )
    check x = if x `elem` rwords
                then fail $ "keyword " ++ show x
                     ++ " cannot be an identifier"
                else return x

---------------
-- Operators --
semicolon = symbol ";"
comma     = symbol ","
square    = symbol "[]"
assign    = symbol ":="
not       = symbol "~"
and       = symbol "&"
or        = symbol "|"
lequal    = symbol "<="
gequal    = symbol ">="
plus      = symbol "+"
minus     = try $ PChar.string "-" >> notFollowedBy ">" >> space >> return "-"
arrow     = symbol "->"
equal     = symbol "=="
greaterthan = symbol ">"
lessthan    = symbol "<"
-------------
-- Helpers --
rword :: Text -> Parser ()
rword w
  = lexeme
  $ try $ PChar.string w
  *> notFollowedBy PChar.alphaNumChar

rwords :: [Text]
rwords = [ "if", "fi", "do", "od", "true", "false"]
