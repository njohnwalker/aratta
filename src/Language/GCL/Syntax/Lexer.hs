{- * GCL Lexicals

import qualified Language.GCL.Syntax.Lexer as Lex -- or GCL.Lex

-}
module Language.GCL.Syntax.Lexer where

import Prelude hiding ( not, and, or )

import Data.Text hiding (map)
import Data.Void

import Text.Megaparsec ( Parsec, Token, many, between, try, notFollowedBy, (<|>) )
import qualified Text.Megaparsec.Char as PChar
import qualified Text.Megaparsec.Char.Lexer as Lex

import Language.GCL.Syntax.Abstract

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
parentheses = between lparen rparen

ifBlock :: Parser a -> Parser a
ifBlock = symbol "if" `between` symbol "fi"

identifier :: Parser Variable
identifier = lexeme $ try $ p >>= check
  where
    p = Variable . Data.Text.pack
        <$> ((:) <$> PChar.letterChar <*> many PChar.alphaNumChar)
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
mult      = symbol "*"
div       = symbol "/"
mod       = symbol "%"
minus     = try $ PChar.string "-" >> notFollowedBy ">" >> space >> return "-"
arrow     = symbol "->"
equal     = symbol "=="
greaterthan = symbol ">"
lessthan    = symbol "<"

lparen = symbol "("
rparen = symbol ")"
reqTag = symbol "@req"
ensTag = symbol "@ens"
invTag = symbol "@inv"


-------------
-- Helpers --
rword :: Text -> Parser ()
rword w
  = lexeme
  $ try $ PChar.string w
  *> notFollowedBy PChar.alphaNumChar
