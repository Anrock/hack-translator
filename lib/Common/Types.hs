module Common.Types where

import Data.Word

type Address = Word16
type Value   = Word16

data Presentation = AST | Binary
data Symbols = Resolved | Unresolved

newtype Source (p :: Presentation) (s :: Symbols) a = Source a
  deriving (Show, Eq)

unSource :: Source p s a -> a
unSource (Source a) = a

