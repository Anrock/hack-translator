import System.Environment (getArgs)
import System.IO

import Common
import Common.Types

import qualified VM.Parser as VM
import qualified VM.Processing as VM

import qualified Assembly.Parser as Asm
import qualified Assembly.Linker as Asm
import qualified Assembly.Types as Asm
import qualified Assembly.Render as Asm

import REPL (repl)

import qualified Hack.Compilation as Hack
import Data.Monoid ((<>))

{-
TODO:
  hackc:
    - Use Text instead of String
    - eDSL for writing asm instructions
    - Pretty printing with color
    - Debug symbols via annotated source spans, like asm block annotated by vm command
  Compiler optimization:
    Ad-hoc:
      - Multiple consecutive A-Instructions with same address
      - For binary ops use --A for popping instead of jumping to SP and decrementing it and dereferencing again
    Future:
      - dead code elimination
      - constant propagation
-}

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      hSetBuffering stdout NoBuffering
      repl
    (filename:_) -> nonInteractive filename

nonInteractive :: String -> IO ()
nonInteractive fileName = do
  let (name, extension) = fileInfo fileName
  contents <- readFile fileName

  case extension of
    Nothing   -> putStrLn "Unknown file type. Supported extensions: .vm, .asm"
    Just Hack -> putStrLn "Can't do anything with hack binary"
    Just Vm   -> processVM contents fileName name
    Just Asm  -> processASM contents fileName name

processVM :: String -> String -> String -> IO ()
processVM contents fileName name = do
  putStrLn "Parsing VM"
  case VM.parse contents of
    Left e -> reportParseError e fileName
    Right ast -> do putStrLn "Compiling to assembly"
                    let asm = VM.compile name ast
                    putStrLn "Rendering assembly"
                    let rendered = Asm.render (unSource <$> asm)
                    let asmFile = name <> "asm"
                    writeFile asmFile rendered
                    putStrLn $ "Rendered assembly written to " <> asmFile
                    hack asm name

processASM :: String -> String -> String -> IO ()
processASM contents fileName name =
  case Asm.parse contents of
    Left e -> reportParseError e fileName
    Right ast -> hack ast name

reportParseError :: String -> String -> IO ()
reportParseError e f = putStrLn $ "Error when parsing " <> f <> ":\n" <> e

fileInfo :: String -> (String, Maybe SourceType)
fileInfo fileName = (name, ext)
  where ext = extensionToSourceType . reverse . takeWhile (/= '.') . reverse $ fileName
        name = reverse . dropWhile (/= '.') . reverse $ fileName

-- TODO: Move somewhere
hack :: [Source 'AST 'Unresolved Asm.Command] -> FilePath -> IO ()
hack ast file = do putStrLn "Resolving symbols"
                   let out = Hack.stringify . Hack.binary . Asm.resolve $ ast
                   putStrLn "Writing rendered binary"
                   writeFile (file <> "hack") out
