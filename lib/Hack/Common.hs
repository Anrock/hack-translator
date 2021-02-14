{-# LANGUAGE BinaryLiterals #-}
module Hack.Common where

import           Data.List.NonEmpty
import           Data.Word
import           Data.Bits

type Binary = [Word16]

cType :: Word16
cType = 0b1110000000000000

aType :: Word16
aType = 0b0

stringify' :: NonEmpty Word16 -> String
stringify' (i :| is) = strBits i ++ "\n" ++ stringify is

stringify :: Binary -> String
stringify [] = []
stringify is = stringify' (fromList is)

strBits :: Word16 -> String
strBits w = (\b -> if b then '1' else '0') <$> boolBits w
 where boolBits bits = foldr (\bi acc -> acc ++ [testBit bits bi]) [] [0 .. 15]

maskedBy :: (FiniteBits a) => a -> a -> Bool
maskedBy a b = a .&. b == b

