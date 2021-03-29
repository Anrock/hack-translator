{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE RecordWildCards #-}

-- | Low level CPU emul
module Emul.CPU where

import Common.Types
import Data.Bits hiding (bit)
import Hack.Common

type Instruction = Value

data CPU = CPU
  { a, pc :: Address
  , d :: Value
  , outM :: Maybe Value
  }

mkCPU :: CPU
mkCPU = CPU{a = 0, pc = 0, d = 0, outM = Nothing}

data ALUFlags = ALUFlags
  { nx, ny, zx, zy :: Bool
  , f :: Bool
  , no :: Bool
  }
  deriving (Show, Eq)

data ALUOutputs = ALUOutputs
  { zr, ng :: Bool
  , out :: Value
  }
  deriving (Show, Eq)

-- y  zx nx zy ny f no
-- 12 11 10 9  8  7 6
decodeALUFlags :: Instruction -> (ALUFlags, Bool)
decodeALUFlags i =
  let zx = bit 11
      nx = bit 10
      zy = bit 9
      ny = bit 8
      f = bit 7
      no = bit 6
      bit = testBit i
   in (ALUFlags{..}, bit 12)

decodeDestination :: Instruction -> (Bool, Bool, Bool)
decodeDestination i = (bit 5, bit 4, bit 3)
  where
    bit = testBit i

decodeJump :: Instruction -> (Bool, Bool, Bool)
decodeJump i = (bit 2, bit 1, bit 0)
  where
    bit = testBit i

alu :: ALUFlags -> Value -> Value -> ALUOutputs
alu ALUFlags{..} x y =
  let proc n z v
        | z && n = 0xFFFF
        | z = 0x0
        | n = complement v
        | otherwise = v
      x' = proc nx zx x
      y' = proc ny zy y
      out
        | f = x' + y'
        | otherwise = x' .&. y'
      out'
        | no = complement out
        | otherwise = out
      zr = out' == 0
      ng = testBit out' 15
   in ALUOutputs{zr, ng, out = out'}

step :: CPU -> Instruction -> Value -> CPU
step cpu@CPU{a, pc, d} ins m
  | ins `maskedBy` cType =
    cpu
      { a = if writeA then out else a
      , d = if writeD then out else a
      , pc = if mustJump then a else pc + 1
      , outM = if writeM then Just out else Nothing
      }
  | otherwise = cpu{a = ins, pc = pc + 1}
  where
    (flags, useM) = decodeALUFlags ins
    ALUOutputs{zr, ng, out} = alu flags d (if useM then m else a)
    (writeA, writeD, writeM) = decodeDestination ins
    (jl, je, jg) = decodeJump ins
    mustJump = (jl && ng) || (je && zr) || (jg && not zr && not ng)
