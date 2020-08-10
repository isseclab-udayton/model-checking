{- CPrint.hs
 - Defines an instance of Show for C programs
 -}

module CPrint where

import Data.List (intercalate)
import Text.Printf
import Clang

-- showCStmt indent statement
showCStmt :: String -> CStmt -> String
showCStmt ind (CVarDecl v (Just e)) = printf "%sJSVar %s = %s;" ind v (show e)
showCStmt ind (CVarDecl v Nothing) = printf "%sJSVar %s;" ind v
showCStmt ind (CSetVar v e) = printf "%s%s = %s;" ind v (show e)
showCStmt ind (CFuncStmt f args) = printf "%s%s(%s);" ind f (intercalate ", " (show <$> args))
showCStmt ind (CLabelled l s) = printf "%s%s: %s" ind l (show s)
showCStmt ind (CIf e stmts) =
  printf "%sif (ToBoolean(%s).val.asBool) {\n%s\n%s}" ind (show e) block ind
    where block = intercalate "\n" (showCStmt (ind ++ "    ") <$> stmts)
showCStmt ind (CIfElse cond b1 b2) =
  printf "%sif (ToBoolean(%s).val.asBool) {\n%s\n%s} else {\n%s\n%s}" ind (show cond) ifBlock ind elseBlock ind
    where ifBlock = intercalate "\n" (showCStmt (ind ++ "    ") <$> b1)
          elseBlock = intercalate "\n" (showCStmt (ind ++ "    ") <$> b2)
showCStmt ind (CWhile e stmts) =
  printf "%swhile (ToBoolean(%s).val.asBool) {\n%s\n%s}" ind (show e) block ind
    where block = intercalate "\n" (showCStmt (ind ++ "    ") <$> stmts)
showCStmt ind (CReturn e) = printf "%sreturn %s;" ind (show e)

instance Show CProg where
  show (CProg decls stmts) =
    intercalate "\n" (show <$> decls)
      ++ "\nvoid main() {\n"
      ++ intercalate "\n" (showCStmt "    " <$> stmts)
      ++ "\n}\n"

instance Show CDecl where
  show (CGlobalVar v (Just e)) = printf "JSVar %s = %s;\n" v (show e)
  show (CGlobalVar v Nothing) = printf "JSVar %s = jsUndefined;\n" v
  show (CFuncDecl fname argList stmts) =
    printf "JSVar %s(%s) {\n%s\n}\n" fname args body
      where args = intercalate ", " (("JSVar "++) <$> argList)
            body = intercalate "\n" (showCStmt "    " <$> stmts)

instance Show CStmt where
  show cstmt = showCStmt "" cstmt

instance Show CExpr where
  show CTrue = "jsTrue"
  show CFalse = "jsFalse"
  show CUndefined = "jsUndefined"
  show (CNumber n) = printf "jsNumValue(%s)" n
  show (CVar v) = v
  show (CFuncCall f args) = printf "%s(%s)" f (intercalate ", " (show <$> args))
  show (CAdd e1 e2) = printf "jsAdd(%s, %s)" (show e1) (show e2)
  show (CSub e1 e2) = printf "jsSub(%s, %s)" (show e1) (show e2)
  show (CMul e1 e2) = printf "jsMul(%s, %s)" (show e1) (show e2)
  show (CDiv e1 e2) = printf "jsDiv(%s, %s)" (show e1) (show e2)
  show (CMod e1 e2) = printf "jsMod(%s, %s)" (show e1) (show e2)
  show (CNeg e) = printf "jsNeg(%s)" (show e)
  show (CEqu e1 e2) = printf "jsEqu(%s, %s)" (show e1) (show e2)
  show (CNeq e1 e2) = printf "jsNeq(%s, %s)" (show e1) (show e2)
  show (CLT e1 e2) = printf "jsLT(%s, %s)" (show e1) (show e2)
  show (CLE e1 e2) = printf "jsLE(%s, %s)" (show e1) (show e2)
  show (CGT e1 e2) = printf "jsGT(%s, %s)" (show e1) (show e2)
  show (CGE e1 e2) = printf "jsGE(%s, %s)" (show e1) (show e2)
  show (COr e1 e2) = printf "jsOr(%s, %s)" (show e1) (show e2)
  show (CAnd e1 e2) = printf "jsAnd(%s, %s)" (show e1) (show e2)
  show (CNot e) = printf "jsNot(%s)" (show e)
