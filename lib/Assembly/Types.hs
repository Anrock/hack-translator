module Assembly.Types where

import Common.Types

data Addressable = Address Address | Label String
    deriving (Show, Eq)

data Command =
       AInstruction Addressable
     | CInstruction [Register] Computation (Maybe Jump)
     | Location String
     deriving (Show, Eq)
data Register = A | M | D deriving (Read, Show, Eq, Bounded, Enum)

data Computation = Negate Operand
                 | Not Operand
                 | Identity Operand
                 | Minus Register Operand
                 | Plus Register Operand
                 | And Register Operand
                 | Or Register Operand
                 deriving (Show, Eq)

data Operand = Reg Register | Val Value deriving (Show, Eq)
data Jump = JLT | JLE | JEQ | JNE | JGT | JGE | JMP
    deriving (Read, Show, Eq, Bounded, Enum)

