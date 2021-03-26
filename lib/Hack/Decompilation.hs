{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Hack.Decompilation where

import           Hack.Common
import           Hack.Compilation
import           Assembly.Types
import           Data.Word
import           Text.Megaparsec
import           Data.Void

type Parser = Parsec Void Binary

-- instance Stream Binary where
--   type Tokens Binary = Binary
--   type Token Binary  = Word16
--   tokenToChunk Proxy = pure
--   tokensToChunk Proxy = id
--   chunkToTokens Proxy = id
--   chunkLength Proxy = length
--   chunkEmpty Proxy = null
--   take1_ [] = Nothing
--   take1_ (t:ts) = Just (t, ts)
--   takeN_ n s
--     | n <= 0    = Just ([], s)
--     | null s    = Nothing
--     | otherwise = Just (splitAt n s)
--   takeWhile_ = span
--   showTokens Proxy = stringify'
--   -- NOTE Do not eta-reduce these (breaks inlining)
--   reachOffset o pst =
--     reachOffset' splitAt foldl' id id ('\n','\t') o pst
--   reachOffsetNoLine o pst =
--     reachOffsetNoLine' splitAt foldl' ('\n', '\t') o pst

class (Bounded a, Enum a) => AllValues a where
  allValues :: [a]
  allValues = [minBound .. maxBound]

instance AllValues Jump
instance AllValues Register

decompile :: [Word16] -> [Command]
decompile = fmap decode

decode :: Word16 -> Command
decode c | c `maskedBy` cType = decodeC c
         | otherwise          = decodeA c

decodeA :: Word16 -> Command
decodeA c = AInstruction $ Address c

decodeC :: Word16 -> Command
decodeC c = CInstruction (dest c) (comp c) (jmp c)

dest :: Word16 -> [Register]
dest c = (D `setIf` dSet) . (M `setIf` mSet) . (A `setIf` aSet) $ []
 where
  aSet = c `maskedBy` bin A
  mSet = c `maskedBy` bin M
  dSet = c `maskedBy` bin D
  setIf r b = if b then (:) r else id

comp :: Word16 -> Computation
comp c | c `maskedBy` bin (Identity (Val 0))    = Identity (Val 0)
       | c `maskedBy` bin (Identity (Val 1))    = Identity (Val 1)
       | c `maskedBy` bin (Identity (Val (-1))) = Identity (Val (-1))
       | c `maskedBy` bin (Identity (Reg D))    = Identity (Reg D)
       | c `maskedBy` bin (Identity (Reg A))    = Identity (Reg A)
       | c `maskedBy` bin (Identity (Reg M))    = Identity (Reg M)
       | c `maskedBy` bin (Not (Reg D))         = Not (Reg D)
       | c `maskedBy` bin (Not (Reg A))         = Not (Reg A)
       | c `maskedBy` bin (Not (Reg M))         = Not (Reg M)
       | c `maskedBy` bin (Negate (Reg D))      = Negate (Reg D)
       | c `maskedBy` bin (Negate (Reg A))      = Negate (Reg A)
       | c `maskedBy` bin (Negate (Reg M))      = Negate (Reg M)
       | c `maskedBy` bin (Plus D (Val 1))      = Plus D (Val 1)
       | c `maskedBy` bin (Plus A (Val 1))      = Plus A (Val 1)
       | c `maskedBy` bin (Plus M (Val 1))      = Plus M (Val 1)
       | c `maskedBy` bin (Minus D (Val 1))     = Minus D (Val 1)
       | c `maskedBy` bin (Minus A (Val 1))     = Minus A (Val 1)
       | c `maskedBy` bin (Minus M (Val 1))     = Minus M (Val 1)
       | c `maskedBy` bin (Plus D (Reg A))      = Plus D (Reg A)
       | c `maskedBy` bin (Plus D (Reg M))      = Plus D (Reg M)
       | c `maskedBy` bin (Minus D (Reg A))     = Minus D (Reg A)
       | c `maskedBy` bin (Minus D (Reg M))     = Minus D (Reg M)
       | c `maskedBy` bin (Minus A (Reg D))     = Minus A (Reg D)
       | c `maskedBy` bin (Minus M (Reg D))     = Minus M (Reg D)
       | c `maskedBy` bin (And D (Reg A))       = And D (Reg A)
       | c `maskedBy` bin (And D (Reg M))       = And D (Reg M)
       | c `maskedBy` bin (Or D (Reg A))        = Or D (Reg A)
       | c `maskedBy` bin (Or D (Reg M))        = Or D (Reg M)
       | otherwise = error $ "Invalid OP: " <> show c

jmp :: Word16 -> Maybe Jump
jmp c | c `maskedBy` bin JLT = Just JLT
      | c `maskedBy` bin JLE = Just JLE
      | c `maskedBy` bin JEQ = Just JEQ
      | c `maskedBy` bin JNE = Just JNE
      | c `maskedBy` bin JGT = Just JGT
      | c `maskedBy` bin JGE = Just JGE
      | c `maskedBy` bin JMP = Just JMP
      | otherwise            = Nothing

