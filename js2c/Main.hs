{- Main.hs
 - Main entry point.
 -}

module Main where

import System.Environment
import Language.JavaScript.Parser.Parser (readJs)
import Clang
import CPrint

helpMessage =
 "Usage:\n\
 \    js2c [-o outfile] [infile] [--help] [--noprelude]"

data Action = PrintHelp
            | Translate Bool String String

processArgs :: [String] -> Action
processArgs ("--help":xs) = PrintHelp
processArgs ("--noprelude":xs) =
  case processArgs xs of
    PrintHelp           -> PrintHelp
    Translate _ inf ouf -> Translate False inf ouf
processArgs ("-o":outfile:xs) =
  case processArgs xs of
    PrintHelp             -> PrintHelp
    Translate p infile "" -> Translate p infile outfile
    action                -> action
processArgs (infile:xs) =
  case processArgs xs of
    PrintHelp              -> PrintHelp
    Translate p "" outfile -> Translate p infile outfile
    action                 -> action
processArgs [] = Translate True "" ""

main :: IO ()
main = do
  args <- getArgs
  case processArgs args of
    PrintHelp -> putStrLn helpMessage
    Translate p infile outfile -> do
      indata <- if infile == "" then getContents else readFile infile
      let output = if outfile == "" then putStrLn else writeFile outfile
      case cprog (readJs indata) of
        Just prog -> do
          prelude <- if p then readFile "prelude.c" else return ""
          output (prelude ++ (show prog))
        Nothing -> putStrLn "Error: Invalid program"
