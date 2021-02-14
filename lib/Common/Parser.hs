-- | Common parser types and functions
module Common.Parser where

import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Text.Megaparsec.Char
import Text.Megaparsec

type Parser = Parsec Void String

-- Lexer wrappers
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- Lexing
parens :: Parser a -> Parser a
parens = lexeme . between (symbol "(") (symbol ")")

number :: Parser Int
number = L.signed sc (lexeme L.decimal)

identifier :: Parser String
identifier = lexeme $ (:) <$> allowedNonDigit <*> many allowed
    where allowedNonDigit = letterChar <|> oneOf "_.$:"
          allowed = allowedNonDigit <|> digitChar

