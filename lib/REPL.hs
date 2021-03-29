{-
  Add files:
    - VM
    - ASM
  Translate:
    - VM -> ASM + annotations
  Link:
    - ASM + annotations -> linked ASM + annotations
  Compile:
    - linked ASM + annotations -> Binary
  Load:
    Binary
  Run:
    Loaded binary
  Display:
    - Registers
    - RAM
    - ROM
    - Segments: with base pointers, display ram as segments
-}
module REPL where

import qualified Assembly.Linker as Asm
import qualified Assembly.Parser as Asm
import qualified Assembly.Render as Asm
import Common.Types
import Control.Exception
import Data.Array
import Data.Char
import Data.Foldable (toList)
import Data.List (intercalate)
import Emul.CPU (CPU (..), mkCPU)
import qualified Emul.CPU as CPU
import qualified Hack.Compilation as Hack
import qualified Hack.Decompilation as Hack
import Numeric
import Text.Read (readMaybe)
import Prelude hiding (max, min)

data REPL = REPL
  { rom :: Array Address Value
  , programLength :: Value
  , ram :: Array Address Value
  , cpu :: CPU
  }

data REPLCommand
  = ShowRamInterval Address Address
  | ShowRam
  | ShowListing
  | Step
  | LoadProgram String
  | LoadRam String
  | Halt

empty :: REPL
empty =
  REPL
    { rom = listArray (0, maxBound) (repeat 0)
    , programLength = 0
    , ram = listArray (0, maxBound) (repeat 0)
    , cpu = CPU.mkCPU
    }

withCommands :: [Value] -> REPL
withCommands cs =
  REPL
    { rom = listArray (0, maxBound) (cs ++ repeat 0)
    , programLength = fromIntegral . length $ cs
    , ram = listArray (0, maxBound) (repeat 0)
    , cpu = mkCPU
    }

repl :: IO ()
repl = repl' empty >> pure ()

repl' :: REPL -> IO REPL
repl' r = do
  printPrompt r
  input <- getLine
  let command = parse input
  case command of
    Nothing -> do
      putStrLn $ "Unknown command: " <> input
      repl' r
    Just Halt -> pure r
    Just c -> eval r c >>= repl'

parse :: String -> Maybe REPLCommand
parse i = case words i of
  ["ram", b, o] -> do
    base <- readMaybe b :: Maybe Address
    offset <- readMaybe o :: Maybe Address
    pure $ ShowRamInterval base offset
  ["list"] -> Just ShowListing
  ["ram"] -> Just ShowRam
  ["step"] -> Just Step
  ["halt"] -> Just Halt
  ["load-prg", f] -> Just (LoadProgram f)
  _ -> Nothing

printPrompt :: REPL -> IO ()
printPrompt r@REPL{ram, cpu} = do
  let CPU{a, d, pc} = cpu
      aVal = "A: " <> show a
      dVal = "D: " <> show d
      mVal = "M: " <> show (ram ! a)
      pcVal = "PC: " <> show pc
  putStrLn $ unwords [aVal, dVal, aVal, mVal, pcVal]
  -- printImmediateListing r
  putStr "HACK> "

clampRange :: (Int, Int) -> (Int, Int) -> (Int, Int)
clampRange (s', f') (s, f)
  | s' < s = clampRange (s, f') (s, f)
  | f' > f = clampRange (s', f) (s, f)
  | otherwise = (s', f')

printImmediateListing :: REPL -> IO ()
printImmediateListing r@REPL{rom, cpu = CPU{pc}} = undefined

showBin :: Value -> String
showBin w = "0b" <> replicate (16 - length significantStr) '0' <> significantStr
  where
    significantStr = showIntAtBase 2 intToDigit w ""

eval :: REPL -> REPLCommand -> IO REPL
eval r@REPL{ram} (ShowRamInterval base offset) = do
  let (start, finish) = bounds ram
  if (base < start || base > finish) || (base + offset > finish)
    then putStrLn "Error: Out of bounds" >> pure r
    else do
      let ramValues = [ram ! i | i <- indices ram, i >= base, i <= base + offset]
      putStrLn $ unlines (showBin <$> ramValues)
      pure r
eval r@REPL{cpu} ShowRam = eval r (ShowRamInterval (a cpu - 10) (a cpu + 10))
eval r Step = pure $ step r
eval r (LoadProgram f) = do
  contents <- try $ readFile f
  case contents of
    Left e -> do
      print (e :: IOException)
      pure r
    Right c -> case Asm.parse c of
      Left e -> do
        putStrLn $ "Error parsing " <> e
        pure r
      Right ast -> do
        let resolved = Asm.resolve ast
        let encoded = Hack.binary resolved
        pure $ withCommands encoded
eval r (LoadRam _) = putStrLn "Not implemented" >> pure r
eval r Halt = putStrLn "Impossible: Halt in eval" >> pure r
eval r@REPL{rom, programLength} ShowListing = do
  let programROM = take (fromIntegral programLength) . toList $ rom
      lineNumbers = show <$> [0 .. programLength]
      romInstructions = showBin <$> programROM
      decodedCommands = Asm.render . Hack.decode <$> programROM
      zipped = zip3 lineNumbers romInstructions decodedCommands
      outLines = (\(l, i, c) -> intercalate " | " [l, i, c]) <$> zipped
  putStrLn $ unlines outLines
  pure r

step :: REPL -> REPL
step r@REPL{rom, ram, cpu} = r{cpu = cpu', ram = ram'}
  where
    ram' = maybe ram (\v -> ram // [(fromIntegral a, v)]) outM
    cpu'@CPU{outM} = CPU.step cpu (rom ! pc) (ram ! a)
    CPU{a, pc} = cpu
