module Assembly.Parser (parse) where

import Assembly.Types
import qualified Text.Megaparsec as P
import Text.Megaparsec hiding (Label, parse, label)
import Text.Megaparsec.Char
import Data.Bifunctor
import Common.Parser
import Common.Types

parse :: String -> Either String [Source 'AST 'Unresolved Command]
parse i = first P.errorBundlePretty $ P.parse parseAssembly "" i

parseAssembly :: Parser [Source 'AST 'Unresolved Command]
parseAssembly = between sc eof $ many line

-- Parsing
line :: Parser (Source 'AST 'Unresolved Command)
line = aInstruction <|> cInstruction <|> location

location :: Parser (Source 'AST 'Unresolved Command)
location = Source . Location <$> parens identifier

aInstruction :: Parser (Source 'AST 'Unresolved Command)
aInstruction = P.label "A-Instruction" $ lexeme $ do
  _ <- char '@'
  addr <- address <|> label
  return $ Source . AInstruction $ addr

address :: Parser Addressable
address = Address . fromIntegral <$> number

label :: Parser Addressable
label = Label <$> identifier

cInstruction :: Parser (Source 'AST 'Unresolved Command)
cInstruction = P.label "C-instruction" $ lexeme $ do
  dest <- destination
  comp <- computation
  jmp  <- optional jump
  pure $ Source $ CInstruction dest comp jmp

destination :: Parser [Register]
destination = P.label "C-instruction destination" $ option [] $ try $ count' 1 3 register <* char '='

jump :: Parser Jump
jump = P.label "C-instruction jump part" $
    char ';' *> (read <$> choice [ try $ symbol "JLT"
                                 , try $ symbol "JLE"
                                 , try $ symbol "JEQ"
                                 , try $ symbol "JNE"
                                 , try $ symbol "JGT"
                                 , try $ symbol "JGE"
                                 , try $ symbol "JMP" ])

computation :: Parser Computation
computation = P.label "C-instruction computation part" $
  try binary <|> unary
    where unary = unOp <*> operand
          binary = do lhs <- register
                      op  <- binOp
                      op lhs <$> operand


binOp :: Parser (Register -> Operand -> Computation)
binOp = do op <- oneOf "-+&|"
           case op of
             '-' -> return Minus
             '+' -> return Plus
             '&' -> return And
             '|' -> return Or
             _   -> error "impossible"

unOp :: Parser (Operand -> Computation)
unOp = do op <- optional $ oneOf "-!"
          case op of
            Just '-' -> return Negate
            Just '!' -> return Not
            Nothing  -> return Identity
            _        -> error "impossible"

operand :: Parser Operand
operand = (Val . fromIntegral <$> number) <|> (Reg <$> register)

register :: Parser Register
register = do
    val <- char 'A' <|> char 'M' <|> char 'D'
    return $ read [val]
