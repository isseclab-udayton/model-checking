{- SMV.hs
 - Defines the algebraic data types for SMV modules and expressions.
 -}

module SMV where

import Text.Printf (printf)
import InterJS

data SMVModule = SMVModule String [String] [SMVModuleElement]

data SMVModuleElement =
   SMVVAR [(String, String)] -- [(var name, type)]
 | SMVASSIGN [(String, String, SMVBasicExpr)] -- [(init or next, var name, expr)]

data SMVBasicExpr = 
   SMVCon String -- a constant
 | SMVVar String -- a variable identifier
 | SMVDefVar String -- a define identifier
 | SMVNot SMVBasicExpr -- logical or bitwise NOT
 | SMVAbs SMVBasicExpr -- absolute value
 | SMVMax SMVBasicExpr SMVBasicExpr -- max
 | SMVMin SMVBasicExpr SMVBasicExpr -- min
 | SMVAnd SMVBasicExpr SMVBasicExpr -- logical or bitwise AND
 | SMVOr SMVBasicExpr SMVBasicExpr -- logical or bitwise OR
 | SMVXor SMVBasicExpr SMVBasicExpr -- logical or bitwise exclusive OR
 | SMVXnor SMVBasicExpr SMVBasicExpr -- logical or bitwise NOT exclusive OR
 | SMVImpl SMVBasicExpr SMVBasicExpr -- logical or bitwise implication
 | SMVEqv SMVBasicExpr SMVBasicExpr -- logical or bitwise equivalence
 | SMVEqu SMVBasicExpr SMVBasicExpr -- equality
 | SMVNeq SMVBasicExpr SMVBasicExpr -- inequality
 | SMVLT SMVBasicExpr SMVBasicExpr -- less than
 | SMVGT SMVBasicExpr SMVBasicExpr -- greater than
 | SMVLE SMVBasicExpr SMVBasicExpr -- less than or equal
 | SMVGE SMVBasicExpr SMVBasicExpr -- greater than or equal
 | SMVNeg SMVBasicExpr             -- integer unary minus
 | SMVAdd SMVBasicExpr SMVBasicExpr -- integer addition
 | SMVSub SMVBasicExpr SMVBasicExpr -- integer subtraction
 | SMVMul SMVBasicExpr SMVBasicExpr -- integer multiplication
 | SMVDiv SMVBasicExpr SMVBasicExpr -- integer division
 | SMVMod SMVBasicExpr SMVBasicExpr -- integer remainder
 | SMVSR SMVBasicExpr SMVBasicExpr -- bit shift right
 | SMVSL SMVBasicExpr SMVBasicExpr -- bit shift left
 | SMVToBool SMVBasicExpr -- unsigned word[1] and int to boolean conversion
 | SMVToInt SMVBasicExpr -- word and boolean to integer constant conversion
 | SMVIfThenElse SMVBasicExpr SMVBasicExpr SMVBasicExpr -- if-then-else expression
 | SMVCase [(SMVBasicExpr, SMVBasicExpr)] -- case expression
 | SMVNext SMVBasicExpr -- next expression

smvModule :: MProg -> SMVModule
smvModule p = SMVModule "main" [] [smvVarConstraint p, smvAssignConstraint p]

smvVarConstraint :: MProg -> SMVModuleElement
smvVarConstraint (MProg decls stmts) =
  let getVarDecl (MIntVarDecl s _) = (s, "signed word[32]")
      getVarDecl (MBoolVarDecl s _) = (s, "boolean")
      numFStmts = length (concat (fstmt <$> stmts))
      pcvardecl = ("PC", "0.."++(show numFStmts))
      vars = pcvardecl:(getVarDecl <$> decls)
   in SMVVAR vars

smvAssignConstraint :: MProg -> SMVModuleElement
smvAssignConstraint (MProg decls stmts) =
  let getVar (MIntVarDecl s _) = s
      getVar (MBoolVarDecl s _) = s
      getExp (MIntVarDecl _ e) = (smvIntExpr e)
      getExp (MBoolVarDecl _ e) = (smvBoolExpr e)
      
      getInitConstr d = ("init", getVar d, getExp d)
      pcinit = ("init", "PC", (SMVCon "0"))
      inits = pcinit:(getInitConstr <$> decls)

      varNames = "PC":(getVar <$> decls)
      fstmts = concat (fstmt <$> stmts)
      cases = (\x -> SMVCase (smvNextConstraint x 0 fstmts)) <$> varNames
      nexts = zip3 (repeat "next") varNames cases
   in SMVASSIGN (inits ++ nexts)


-- A "flat statement". Flat statements do not have nested statement
-- blocks. Each flat statement corresponds to one PC address. The
-- addresses in goto/ifgoto statements are relative to the current
-- PC value, not absolute.
data FStmt = FSetBoolVar String MBoolExpr
           | FSetIntVar String MIntExpr
           | FIfGoto MBoolExpr Int
           | FGoto Int

-- Flattens a statement into a list of flat statements
fstmt :: MStmt -> [FStmt]
fstmt (MSetBoolVar s e) = [FSetBoolVar s e]
fstmt (MSetIntVar s e) = [FSetIntVar s e]
fstmt (MIf cond ifblock) =
  let ifblock' = concat (fstmt <$> ifblock)
      lenIf = length ifblock'
   in [FIfGoto (MNot cond) (lenIf + 1)] ++ ifblock'
fstmt (MIfElse cond ifblock elseblock) =
  let ifblock' = concat (fstmt <$> ifblock)
      elseblock' = concat (fstmt <$> elseblock)
      lenIf = length ifblock'
      lenElse = length elseblock'
   in [FIfGoto (MNot cond) (lenIf + 2)]
       ++ ifblock'
       ++ [FGoto (lenElse + 1)]
       ++ elseblock'
fstmt (MWhile cond block) =
  let block' = concat (fstmt <$> block)
      lenBlock = length block
   in [FIfGoto (MNot cond) (lenBlock + 2)]
       ++ block'
       ++ [FGoto (-lenBlock - 1)]

-- Parameters: a variable v, an initial PC value, and a list of statements,
-- Returns: next constraint for the variable v.
smvNextConstraint :: String -> Int -> [FStmt] -> [(SMVBasicExpr, SMVBasicExpr)]
smvNextConstraint "PC" pc [] = [
  (SMVGE (SMVVar "PC") (SMVCon (show pc)), (SMVVar "PC")),
  (SMVCon "TRUE", SMVAdd (SMVVar "PC") (SMVCon "1"))]
smvNextConstraint v _ [] = [(SMVCon "TRUE", SMVCon v)]
smvNextConstraint v pc (s:ss) = 
  case s of
    FSetBoolVar x e -> if x == v then (cond, resp):rest else rest
      where cond = SMVEqu (SMVVar "PC") (SMVCon (show pc))
            resp = smvBoolExpr e
    FSetIntVar x e -> if x == v then (cond, resp):rest else rest
      where cond = SMVEqu (SMVVar "PC") (SMVCon (show pc))
            resp = smvIntExpr e
    FIfGoto e i -> if v == "PC" then (cond, resp):rest else rest
      where cond = SMVAnd
                     (SMVEqu (SMVVar "PC") (SMVCon (show pc)))
                     (smvBoolExpr e)
            resp = SMVCon (show (pc + i))
    FGoto i -> if v == "PC" then (cond, resp):rest else rest
      where cond = SMVEqu (SMVVar "PC") (SMVCon (show pc))
            resp = SMVCon (show (pc + i))
    where rest = smvNextConstraint v (pc + 1) ss

smvBoolExpr :: MBoolExpr -> SMVBasicExpr
smvBoolExpr MTrue = SMVCon "TRUE"
smvBoolExpr MFalse = SMVCon "FALSE"
smvBoolExpr (MBoolVar s) = SMVVar s
smvBoolExpr (MEqu e1 e2) = SMVEqu (smvIntExpr e1) (smvIntExpr e2)
smvBoolExpr (MNeq e1 e2) = SMVNeq (smvIntExpr e1) (smvIntExpr e2)
smvBoolExpr (MLT e1 e2) = SMVLT (smvIntExpr e1) (smvIntExpr e2)
smvBoolExpr (MLE e1 e2) = SMVLE (smvIntExpr e1) (smvIntExpr e2)
smvBoolExpr (MGT e1 e2) = SMVGT (smvIntExpr e1) (smvIntExpr e2)
smvBoolExpr (MGE e1 e2) = SMVGE (smvIntExpr e1) (smvIntExpr e2)
smvBoolExpr (MOr e1 e2) = SMVOr (smvBoolExpr e1) (smvBoolExpr e2)
smvBoolExpr (MAnd e1 e2) = SMVAnd (smvBoolExpr e1) (smvBoolExpr e2)
smvBoolExpr (MNot e) = SMVNot (smvBoolExpr e)
smvBoolExpr (MEqv e1 e2) = SMVEqv (smvBoolExpr e1) (smvBoolExpr e2)
smvBoolExpr (MXor e1 e2) = SMVXor (smvBoolExpr e1) (smvBoolExpr e2)

smvIntExpr :: MIntExpr -> SMVBasicExpr
smvIntExpr (MIntVal i) = SMVCon ("0sd32_" ++ (show i))
smvIntExpr (MIntVar s) = SMVVar s
smvIntExpr (MAdd e1 e2) = SMVAdd (smvIntExpr e1) (smvIntExpr e2)
smvIntExpr (MSub e1 e2) = SMVSub (smvIntExpr e1) (smvIntExpr e2)
smvIntExpr (MMul e1 e2) = SMVMul (smvIntExpr e1) (smvIntExpr e2)
smvIntExpr (MDiv e1 e2) = SMVDiv (smvIntExpr e1) (smvIntExpr e2)
smvIntExpr (MMod e1 e2) = SMVMod (smvIntExpr e1) (smvIntExpr e2)
smvIntExpr (MNeg e) = SMVNeg (smvIntExpr e)
