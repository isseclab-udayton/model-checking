{- InterJSPrint.hs
 - Converts "Intermediate Javascript" back into normal JavaScript
 - with the custom semantics imposed.
 -}

module InterJSPrint where

import Data.List (intercalate)
import Text.Printf (printf)
import InterJS

instance Show MProg where show = jsprog
instance Show MVarDecl where show = jsvardecl
instance Show MStmt where show = jsstmt ""
instance Show MBoolExpr where show = jsboolexpr
instance Show MIntExpr where show = jsintexpr

jsprog :: MProg -> String
jsprog (MProg decls stmts) = (showls decls . ("\n"++) . showls stmts) ""
  where showls l s = foldr (\x r -> (show x) ++ "\n" ++ r) s l

jsvardecl :: MVarDecl -> String
jsvardecl (MIntVarDecl s e) = printf "var %s = %s;" s (show e)
jsvardecl (MBoolVarDecl s e) = printf "var %s = %s;" s (show e)

-- jsstmt indent stmt
jsstmt :: String -> MStmt -> String
jsstmt ind (MSetBoolVar s e) = printf "%s%s = %s;" ind s (show e)
jsstmt ind (MSetIntVar s e) = printf "%s%s = %s;" ind s (show e)
jsstmt ind (MIf e ss) = printf "%sif (%s) {\n%s\n%s}" ind (show e) block ind
  where block = intercalate "\n" (jsstmt (ind ++ "    ") <$> ss)
jsstmt ind (MIfElse e ss1 ss2) =
  printf "%sif (%s) {\n%s\n%s} else {\n%s\n%s}" ind (show e) block1 ind block2 ind
    where block1 = intercalate "\n" (jsstmt (ind ++ "    ") <$> ss1)
          block2 = intercalate "\n" (jsstmt (ind ++ "    ") <$> ss2)
jsstmt ind (MWhile e ss) = printf "%swhile (%s) {\n%s\n%s}" ind (show e) block ind
  where block = intercalate "\n" (jsstmt (ind ++ "    ") <$> ss)

jsboolexpr :: MBoolExpr -> String
jsboolexpr MTrue = "true"
jsboolexpr MFalse = "false"
jsboolexpr (MBoolVar s) = s
jsboolexpr (MEqu e1 e2) = printf "opEQU(%s, %s)" (show e1) (show e2)
jsboolexpr (MNeq e1 e2) = printf "opNEQ(%s, %s)" (show e1) (show e2)
jsboolexpr (MLT e1 e2) = printf "opLT(%s, %s)" (show e1) (show e2)
jsboolexpr (MLE e1 e2) = printf "opLE(%s, %s)" (show e1) (show e2)
jsboolexpr (MGT e1 e2) = printf "opGT(%s, %s)" (show e1) (show e2)
jsboolexpr (MGE e1 e2) = printf "opGE(%s, %s)" (show e1) (show e2)
jsboolexpr (MOr e1 e2) = printf "opOR(%s, %s)" (show e1) (show e2)
jsboolexpr (MAnd e1 e2) = printf "opAND(%s, %s)" (show e1) (show e2)
jsboolexpr (MNot e) = printf "opNOT(%s)" (show e)
jsboolexpr (MEqv e1 e2) = printf "opEQV(%s, %s)" (show e1) (show e2)
jsboolexpr (MXor e1 e2) = printf "opXOR(%s, %s)" (show e1) (show e2)

jsintexpr :: MIntExpr -> String
jsintexpr (MIntVal i) = show i
jsintexpr (MIntVar s) = s
jsintexpr (MAdd e1 e2) = printf "opADD(%s, %s)" (show e1) (show e2)
jsintexpr (MSub e1 e2) = printf "opSUB(%s, %s)" (show e1) (show e2)
jsintexpr (MMul e1 e2) = printf "opMUL(%s, %s)" (show e1) (show e2)
jsintexpr (MDiv e1 e2) = printf "opDIV(%s, %s)" (show e1) (show e2)
jsintexpr (MMod e1 e2) = printf "opMOD(%s, %s)" (show e1) (show e2)
jsintexpr (MNeg e) = printf "opNEG(%s)" (show e)
