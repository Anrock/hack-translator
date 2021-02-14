{-# LANGUAGE BinaryLiterals, NamedFieldPuns #-}

module CPUEmulSpec where

import Test.Hspec

import Control.Monad (forM_)

import Emul.CPU

inputsTable =
  [
  --     x        y       zx     nx     zy     ny     f      no     out                 zr     ng
    (0,  0,       0xFFFF, True,  False, True,  False, True,  False, 0b0,                True,  False)
  , (1,  0,       0xFFFF, True,  True,  True,  True,  True,  True,  0b0000000000000001, False, False)
  , (2,  0,       0xFFFF, True,  True,  True,  False, True,  False, 0b1111111111111111, False, True)
  , (3,  0,       0xFFFF, False, False, True,  True,  False, False, 0b0000000000000000, True,  False)
  , (4,  0,       0xFFFF, True,  True,  False, False, False, False, 0b1111111111111111, False, True)
  , (5,  0,       0xFFFF, False, False, True,  True,  False, True,  0b1111111111111111, False, True)
  , (6,  0,       0xFFFF, True,  True,  False, False, False, True,  0b0000000000000000, True,  False)
  , (7,  0,       0xFFFF, False, False, True,  True,  True,  True,  0b0000000000000000, True,  False)
  , (8,  0,       0xFFFF, True,  True,  False, False, True,  True,  0b0000000000000001, False, False)
  , (9,  0,       0xFFFF, False, True,  True,  True,  True,  True,  0b0000000000000001, False, False)
  , (10, 0,       0xFFFF, True,  True,  False, True,  True,  True,  0b0000000000000000, True,  False)
  , (11, 0,       0xFFFF, False, False, True,  True,  True,  False, 0b1111111111111111, False, True)
  , (12, 0,       0xFFFF, True,  True,  False, False, True,  False, 0b1111111111111110, False, True)
  , (13, 0,       0xFFFF, False, False, False, False, True,  False, 0b1111111111111111, False, True)
  , (14, 0,       0xFFFF, False, True,  False, False, True,  True,  0b0000000000000001, False, False)
  , (15, 0,       0xFFFF, False, False, False, True,  True,  True,  0b1111111111111111, False, True)
  , (16, 0,       0xFFFF, False, False, False, False, False, False, 0b0000000000000000, True,  False)
  , (17, 0,       0xFFFF, False, True,  False, True,  False, True,  0b1111111111111111, False, True)
  , (18, 0b10001, 0b11,   True,  False, True,  False, True,  False, 0b0000000000000000, True,  False)
  , (19, 0b10001, 0b11,   True,  True,  True,  True,  True,  True , 0b0000000000000001, False, False)
  , (20, 0b10001, 0b11,   True,  True,  True,  False, True,  False, 0b1111111111111111, False, True)
  , (21, 0b10001, 0b11,   False, False, True,  True,  False, False, 0b0000000000010001, False, False)
  , (22, 0b10001, 0b11,   True,  True,  False, False, False, False, 0b0000000000000011, False, False)
  , (23, 0b10001, 0b11,   False, False, True,  True,  False, True , 0b1111111111101110, False, True)
  , (24, 0b10001, 0b11,   True,  True,  False, False, False, True , 0b1111111111111100, False, True)
  , (25, 0b10001, 0b11,   False, False, True,  True,  True,  True , 0b1111111111101111, False, True)
  , (26, 0b10001, 0b11,   True,  True,  False, False, True,  True , 0b1111111111111101, False, True)
  , (27, 0b10001, 0b11,   False, True,  True,  True,  True,  True , 0b0000000000010010, False, False)
  , (28, 0b10001, 0b11,   True,  True,  False, True,  True,  True , 0b0000000000000100, False, False)
  , (29, 0b10001, 0b11,   False, False, True,  True,  True,  False, 0b0000000000010000, False, False)
  , (30, 0b10001, 0b11,   True,  True,  False, False, True,  False, 0b0000000000000010, False, False)
  , (31, 0b10001, 0b11,   False, False, False, False, True,  False, 0b0000000000010100, False, False)
  , (32, 0b10001, 0b11,   False, True,  False, False, True,  True , 0b0000000000001110, False, False)
  , (33, 0b10001, 0b11,   False, False, False, True,  True,  True , 0b1111111111110010, False, True)
  , (34, 0b10001, 0b11,   False, False, False, False, False, False, 0b0000000000000001, False, False)
  , (35, 0b10001, 0b11,   False, True,  False, True,  False, True , 0b0000000000010011, False, False)
  ]

inputToExpectation (en, x, y, zx, nx, zy, ny, f, no, out, zr, ng) =
  let inputs = ALUInputs{x, y, zx, nx, zy, ny, f, no}
      expectedOutputs = ALUOutputs{out, zr, ng}
      actualOutputs = alu inputs
  in it ("for inputs #" <> show en) $ actualOutputs `shouldBe` expectedOutputs

cpuSpec :: SpecWith ()
cpuSpec = describe "CPU emulator" $
            describe "ALU emulator" $
              forM_ inputsTable inputToExpectation

