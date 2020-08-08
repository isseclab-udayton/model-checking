module SMVPrint where

import Data.List (intercalate)
import Text.Printf
import SMV

instance Show SMVModule where
  show (SMVModule name argList compList) =
    let args = case intercalate ", " argList of
          "" -> ""
          x  -> printf "(%s)" x
        comps = foldr shows "" compList
    in "MODULE " ++ name ++ args ++ "\n" ++ comps

instance Show SMVModuleElement where
  show (SMVVAR l) = "  VAR\n" ++ concat (f <$> l)
    where f (v, t) = printf "    %s: %s;\n" v t
  show (SMVASSIGN l) = "  ASSIGN\n" ++ concat (f <$> l)
    where f (a, v, e) = printf "    %s(%s) := %s;\n" a v e

instance Show SMVBasicExpr where
  show e = printf "%s" e

instance PrintfArg SMVModule where
  formatArg (SMVModule name argList compList) _ =
    let args = case intercalate ", " argList of
          "" -> ""
          x  -> printf "(%s)" x
        header = printf "MODULE %s%s\n" name args
        comps = foldr shows "" compList
    in showString header . showString comps

instance PrintfArg SMVModuleElement where
  formatArg elem _ = shows elem

instance PrintfArg SMVBasicExpr where
  formatArg expr _ = case expr of
    SMVCon c -> showString c
    SMVVar v -> showString v
    SMVNot e -> printf "!(%s)%s" e
    SMVAbs e -> printf "abs(%s)%s" e
    SMVAnd e1 e2 -> printf "(%s & %s)%s" e1 e2
    SMVOr e1 e2 -> printf "(%s | %s)%s" e1 e2
    SMVImpl e1 e2 -> printf "(%s -> %s)%s" e1 e2
    SMVEqv e1 e2 -> printf "(%s <-> %s)%s" e1 e2
    SMVEqu e1 e2 -> printf "(%s = %s)%s" e1 e2
    SMVNeq e1 e2 -> printf "(%s != %s)%s" e1 e2
    SMVLT e1 e2 -> printf "(%s < %s)%s" e1 e2
    SMVGT e1 e2 -> printf "(%s > %s)%s" e1 e2
    SMVLE e1 e2 -> printf "(%s <= %s)%s" e1 e2
    SMVGE e1 e2 -> printf "(%s >= %s)%s" e1 e2
    SMVNeg e -> printf "-(%s)%s" e
    SMVAdd e1 e2 -> printf "(%s + %s)%s" e1 e2
    SMVSub e1 e2 -> printf "(%s - %s)%s" e1 e2
    SMVMul e1 e2 -> printf "(%s * %s)%s" e1 e2
    SMVDiv e1 e2 -> printf "(%s / %s)%s" e1 e2
    SMVMod e1 e2 -> printf "(%s mod %s)%s" e1 e2
    SMVCase arms -> printf "\n      case %s      esac %s" branches
      where f (c, r) = printf "%s: %s;\n" c r
            branches = intercalate "           " (fmap f arms) :: String
    _ -> showString ""
