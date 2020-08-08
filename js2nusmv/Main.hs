{- SMVMain.js
 - Converts Javascript code into NuSMV input code.
 -}

module Main where

import System.Environment
import Language.JavaScript.Parser.Parser (readJs)
import ParseJS (mprog)
import SMV (smvModule)
import SMVPrint

helpMessage =
 "Usage:\n\
 \    js2nusmv [-o outfile] [infile] [--help]"

data Action = PrintHelp
            | Translate String String

processArgs :: [String] -> Action
processArgs ("--help":xs) = PrintHelp
processArgs ("-o":outfile:xs) =
  case processArgs xs of
    PrintHelp           -> PrintHelp
    Translate infile "" -> Translate infile outfile
    action              -> action
processArgs (infile:xs) =
  case processArgs xs of
    PrintHelp            -> PrintHelp
    Translate "" outfile -> Translate infile outfile
    action               -> action
processArgs [] = Translate "" ""

main :: IO ()
main = do
  args <- getArgs
  case processArgs args of
    PrintHelp -> putStrLn helpMessage
    Translate infile outfile -> do
      indata <- if infile == "" then getContents else readFile infile
      let output = if outfile == "" then putStrLn else writeFile outfile
      case mprog (readJs indata) of
        Just prog -> output (show (smvModule prog))
        Nothing   -> putStrLn "Error: Invalid program"
