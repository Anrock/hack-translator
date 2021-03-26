module VM.Types where

import Common.Types

type Index = Address

data Segment = Argument
             | Local
             | Static
             | Constant
             | This
             | That
             | Pointer
             | Temp
             deriving (Show, Eq)

data Command = Eq
             | Lt
             | Gt
             | Add
             | Sub
             | Neg
             | And
             | Or
             | Not
             | Push Segment Index
             | Pop Segment Index
             deriving (Show, Eq)

