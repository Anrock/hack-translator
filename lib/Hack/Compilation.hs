{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hack.Compilation
  ( binary
  , stringify
  , Bin(bin)
  )
where

import           Hack.Common             hiding ( Binary )
import           Assembly.Types
import           Data.Word                      ( Word16 )
import           Data.Bits
import           Common.Types

-- TODO: use fold
binary :: [Source 'AST 'Resolved Command] -> [Value]
binary []               = []
binary (Source c : ls) = bin c : binary ls

class Bin a where
  bin :: a -> Word16

instance Bin Command where
  bin (CInstruction d c j) =
    cType .|. binDestination .|. bin c .|. maybe 0b0 bin j
    where binDestination = foldr (.|.) 0b0 (bin <$> d)
  bin (AInstruction (Address addr)) = fromIntegral addr
  bin invalid                       = error $ "bin " <> show invalid

instance Bin Jump where
  bin JLT = 0x4
  bin JLE = 0x6
  bin JEQ = 0x2
  bin JNE = 0x5
  bin JGT = 0x1
  bin JGE = 0x3
  bin JMP = 0x7

instance Bin Register where
  bin A = 0x20
  bin M = 0x8
  bin D = 0x10

-- TODO: make total
instance Bin Computation where
  bin (Identity (Val 0   )) = 0b0101010000000
  bin (Identity (Val 1   )) = 0b0111111000000
  bin (Identity (Val (-1))) = 0b0111010000000

  bin (Identity (Reg D   )) = 0b0001100000000
  bin (Identity (Reg A   )) = 0b0110000000000
  bin (Identity (Reg M   )) = 0b1110000000000

  bin (Not      (Reg D   )) = 0b0001101000000
  bin (Not      (Reg A   )) = 0b0110001000000
  bin (Not      (Reg M   )) = 0b1110001000000

  bin (Negate   (Reg D   )) = 0b0001111000000
  bin (Negate   (Reg A   )) = 0b0110011000000
  bin (Negate   (Reg M   )) = 0b1110011000000

  bin (Plus  D (Val 1)    ) = 0b0011111000000
  bin (Plus  A (Val 1)    ) = 0b0110111000000
  bin (Plus  M (Val 1)    ) = 0b1110111000000

  bin (Minus D (Val 1)    ) = 0b0001110000000
  bin (Minus A (Val 1)    ) = 0b0110010000000
  bin (Minus M (Val 1)    ) = 0b1110010000000

  bin (Plus  D (Reg A)    ) = 0b0000010000000
  bin (Plus  D (Reg M)    ) = 0b1000010000000

  bin (Minus D (Reg A)    ) = 0b0010011000000
  bin (Minus D (Reg M)    ) = 0b1010011000000

  bin (Minus A (Reg D)    ) = 0b0000111000000
  bin (Minus M (Reg D)    ) = 0b1000111000000

  bin (And   D (Reg A)    ) = 0b0000000000000
  bin (And   D (Reg M)    ) = 0b1000000000000

  bin (Or    D (Reg A)    ) = 0b0010101000000
  bin (Or    D (Reg M)    ) = 0b1010101000000
  bin invalidOP             = error $ "Invalid OP: " <> show invalidOP
