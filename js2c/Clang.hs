{- Clang.hs
 - Contains function definitions for converting a Javascript abstract
 - syntax tree into an abstract C program.
 -}

module Clang
  ( CProg(..)
  , CDecl(..)
  , CStmt(..)
  , CExpr(..)
  , cprog
  ) where

import Control.Applicative (liftA2)
import Language.JavaScript.Parser.Parser as Parser
import Language.JavaScript.Parser.AST as AST

data CProg = CProg [CDecl]

data CDecl = CGlobalVar String (Maybe CExpr)
           | CFuncDecl String [String] [CStmt]

data CStmt = CVarDecl String (Maybe CExpr)
           | CSetVar String CExpr
           | CFuncStmt String [CExpr]
           | CIf CExpr [CStmt]
           | CIfElse CExpr [CStmt] [CStmt]
           | CWhile CExpr [CStmt]
           | CReturn CExpr

data CExpr = CTrue               -- {JSboolean, {.boolean = 1})
           | CFalse              -- {JSboolean, {.boolean = 0}}
           | CUndefined
           | CNumber String      -- {JSnumber, {.number = <_>}}
           | CVar String
           | CFuncCall String [CExpr]
           | CAdd CExpr CExpr
           | CSub CExpr CExpr
           | CMul CExpr CExpr
           | CDiv CExpr CExpr
           | CMod CExpr CExpr
           | CEqu CExpr CExpr
           | CNeq CExpr CExpr
           | CLT CExpr CExpr
           | CLE CExpr CExpr
           | CGT CExpr CExpr
           | CGE CExpr CExpr
           | COr CExpr CExpr
           | CAnd CExpr CExpr
           | CNot CExpr

-- Converts a JSCommaList to a regular list
fromCommaList :: JSCommaList a -> [a]
fromCommaList (JSLCons head _ tail) = (fromCommaList head) ++ [tail]
fromCommaList (JSLOne x) = [x]
fromCommaList JSLNil = []

-- Extracts the variable initializations from the comma list provided in
-- a JSVariable construct. Uninitialized variables are initialized to
-- undefined in the C context.
getVarInits :: JSCommaList JSExpression -> Maybe [(String, CExpr)]
getVarInits = combine1 . (f<$>) . fromCommaList
  where f (JSVarInitExpression (JSIdentifier _ s) (JSVarInit _ e)) =
          case cexpr e of
            Just e' -> Just (s, e')
            Nothing -> Nothing
        f (JSVarInitExpression (JSIdentifier _ s) JSVarInitNone) =
          Just (s, CUndefined)
        f _ = Nothing

cprog :: JSAST -> Maybe CProg
cprog (JSAstProgram stmts _) =
  do decls <- combine2 (cdecl <$> stmts)
     return (CProg decls)
cprog _ = Nothing

cdecl :: JSStatement -> Maybe [CDecl]
cdecl (JSFunction _ (JSIdentName _ ident) _ arglist _ (JSBlock _ stmts _) _) =
  do args <- combine1 (f <$> (fromCommaList arglist))
     body <- getBody stmts
     return [CFuncDecl ident args body]
    where f (JSIdentifier _ x) = Just x
          f _ = Nothing
          getBody [] = Just [CReturn CUndefined]
          getBody [JSReturn _ Nothing _] = Just [CReturn CUndefined]
          getBody [JSReturn _ (Just e) _] = cexpr e >>= (\x -> Just [CReturn x])
          getBody [s] = (liftA2 (++)) (cstmt s) (Just [CReturn CUndefined])
          getBody (s:ss) = (liftA2 (++)) (cstmt s) (getBody ss)
cdecl (JSVariable _ clist _) =
  do vars <- getVarInits clist
     return (fmap (\(s,e) -> CGlobalVar s (Just e)) vars)
cdecl _ = Nothing

-- Converts a single JS statement into a list of C statements
cstmt :: JSStatement -> Maybe [CStmt]
cstmt (JSStatementBlock a1 stmts a2 semi) = combine2 (cstmt <$> stmts)
cstmt (JSIf _ _ e _ s) =
  do cond <- cexpr e
     ifBlock <- cstmt s
     return [CIf cond ifBlock]
cstmt (JSIfElse _ _ e _ s1 _ s2) =
  do cond <- cexpr e
     ifBlock <- cstmt s1
     elseBlock <- cstmt s2
     return [CIfElse cond ifBlock elseBlock]
cstmt (JSAssignStatement e1 op e2 _) =
  do lhs <- cexpr e1
     rhs <- cexpr e2
     lhs' <- case lhs of
       CVar x -> Just x
       _      -> Nothing
     rhs' <- case op of
       JSAssign _      -> Just rhs
       JSTimesAssign _ -> Just (CMul lhs rhs)
       JSPlusAssign _  -> Just (CAdd lhs rhs)
       JSMinusAssign _ -> Just (CSub lhs rhs)
     return [CSetVar lhs' rhs']
cstmt (JSWhile _ _ e _ s) =
  do cond <- cexpr e
     whileBlock <- cstmt s
     return [CWhile cond whileBlock]
cstmt (JSMethodCall (JSIdentifier _ fname) _ argList _ _) =
  do args <- combine1 (cexpr <$> (fromCommaList argList))
     return [CFuncStmt fname args]
cstmt (JSReturn _ Nothing _) = Just [CReturn CUndefined]
cstmt (JSReturn _ (Just e) _) = cexpr e >>= (\x -> Just [CReturn x])
cstmt (JSVariable _ clist _) =
  do vars1 <- getVarInits clist
     return (fmap (\(s,e) -> CVarDecl s (Just e)) vars1)
cstmt _ = Nothing

-- Builds a C expression
cexpr :: JSExpression -> Maybe CExpr
cexpr (JSIdentifier _ "undefined") = Just CUndefined
cexpr (JSIdentifier _ s) = Just (CVar s)
cexpr (JSDecimal _ s) = Just (CNumber s)
cexpr (JSLiteral _ "true") = Just CTrue
cexpr (JSLiteral _ "false") = Just CFalse
cexpr (JSExpressionBinary e1 op e2) =
  do loperand <- cexpr e1
     roperand <- cexpr e2
     operator <- case op of
       JSBinOpPlus _  -> Just CAdd
       JSBinOpMinus _ -> Just CSub
       JSBinOpTimes _ -> Just CMul    -- Should include division and mod also
       JSBinOpEq _    -> Just CEqu
       JSBinOpNeq _   -> Just CNeq
       JSBinOpGe _    -> Just CGE
       JSBinOpGt _    -> Just CGT
       JSBinOpLe _    -> Just CLE
       JSBinOpLt _    -> Just CLT
       JSBinOpAnd _   -> Just CAnd
       JSBinOpOr _    -> Just COr
       _              -> Nothing
     return (operator loperand roperand)
cexpr (JSExpressionParen _ e _) = cexpr e
cexpr (JSMemberExpression (JSIdentifier _ fname) _ argList _) =
  do args <- combine1 (cexpr <$> (fromCommaList argList))
     return (CFuncCall fname args)
cexpr (JSUnaryExpression op e) =
  do operand <- cexpr e
     operator <- case op of
       JSUnaryOpNot _ -> Just CNot
       _              -> Nothing
     return (operator operand)
cexpr _ = Nothing

-- Utility functions
combine1 :: [Maybe a] -> Maybe [a]
combine1 = foldr (liftA2 (:)) (Just [])

combine2 :: [Maybe [a]] -> Maybe [a]
combine2 = foldr (liftA2 (++)) (Just [])
