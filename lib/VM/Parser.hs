module VM.Parser (parse) where

import Prelude hiding (and, or, not)
import VM.Types
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as P
import Common.Parser
import Common.Types
import Data.Bifunctor (first)
import Data.Functor (($>))

parse :: String -> Either String [Source 'AST 'Unresolved Command]
parse i = first P.errorBundlePretty $ P.parse parseVM "" i

parseVM :: Parser [Source 'AST 'Unresolved Command]
parseVM = between sc eof $ some $ Source <$> vmCommand

vmCommand :: Parser Command
vmCommand = choice [eq, lt, gt, add, sub, neg, and, or, not, push]

push :: Parser Command
push = do
  _ <- symbol "push"
  seg <- segment
  Push seg . fromIntegral <$> number

segment :: Parser Segment
segment = choice [ symbol "argument" $> Argument
                 , symbol "local"    $> Local
                 , symbol "static"   $> Static
                 , symbol "constant" $> Constant
                 , symbol "this"     $> This
                 , symbol "that"     $> That
                 , symbol "pointer"  $> Pointer
                 , symbol "temp"     $> Temp]

eq, lt, gt, add, sub, neg, and, or, not :: Parser Command
eq = symbol "eq" $> Eq
lt = symbol "lt" $> Lt
gt = symbol "gt" $> Gt
add = symbol "add" $> Add
sub = symbol "sub" $> Sub
neg = symbol "neg" $> Neg
and = symbol "and" $> And
or = symbol "or" $> Or
not = symbol "not" $> Not

