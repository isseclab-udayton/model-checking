{- SMVMain.js
 - Converts Javascript code into NuSMV input code.
 -}

module Main where

import System.Environment
import Language.JavaScript.Parser.Parser (readJs)
import ParseJS (mprog)
import SMV (smvModule)
import SMVPrint
import InterJSPrint

helpMessage =
 "Usage:\n\n\
 \    js2nusmv [FILE.js] [OPTIONS]\n\n\
 \By default, the input source is taken from STDIN and the SMV\n\
 \module is output to STDOUT. This behavior can be modified\n\
 \with the following options:\n\n\
 \    -help             Display this help message\n\
 \    -js <FILE.js>     Output the modified JS file to FILE.js\n\
 \    -smv <FILE.smv>   Output SMV module to FILE.smv\n"

-- Translate "input.js" "output.js" "output.smv"
data Action = PrintHelp
            | ErrorMesg String
            | Translate (IO String) (String -> IO ()) (String -> IO ())

-- NOTE: If duplicate cmd options are provided, only the first option
-- is considered, the rest are discarded.
-- Example: "js2nusmv hello.js world.js" is same as "js2nusmv hello.js"
processArgs :: [String] -> Action
processArgs ("-help":xs) = PrintHelp
processArgs ("-smv":outsmv:xs) =
  case processArgs xs of
    Translate injs outjs _ -> Translate injs outjs (writeFile outsmv)
    action                  -> action
processArgs ("-js":outjs:xs) =
  case processArgs xs of
    Translate injs _ outsmv -> Translate injs (writeFile outjs) outsmv
    action                   -> action
processArgs (('-':op):_) =
  ErrorMesg $ "Error: unrecognized option -" ++ op
processArgs (injs:xs) =
  case processArgs xs of
    Translate _ outjs outsmv -> Translate (readFile injs) outjs outsmv
    action                    -> action
processArgs [] = Translate getContents (const $ return ()) putStrLn

main :: IO ()
main = do
  args <- getArgs
  case processArgs args of
    PrintHelp -> putStrLn helpMessage
    ErrorMesg m -> putStrLn m
    Translate getinjs outputjs outputsmv -> do
      indata <- getinjs
      case mprog (readJs indata) of
        Just prog -> do
          outputjs (show prog)
          outputsmv (show (smvModule prog))
        Nothing   -> putStrLn "Error: Invalid program"
