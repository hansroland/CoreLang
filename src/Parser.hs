-- -----------------------------------------------------------
-- A parser for the Core language
-- -----------------------------------------------------------

module Parser(
    syntax
)

where

import Lex
import ParserBase
import Syntax

syntax :: [Token] -> CoreProgram
syntax = takeFirstParse . pProgram
    where
      takeFirstParse ((prog, []) : _) = prog
      takeFirstParse ((_, x) : y) = error $ "Syntax error x =" ++ show x ++ " y=" ++ show y
      takeFirstParse _ = error "Syntax error"
   
-- | pProgram Parser for a Core Program
pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

-- | pSc - Parser for a Super Combinators
pSc :: Parser CoreScDefn
pSc = pThen4 mkSc pVar (pZeroOrMore pVar) (pLit "=") pExpr
   where mkSc a b _ d = (a,b,d)

-- | pExpr - Parser for a Core Expression
pExpr :: Parser CoreExpr
pExpr = pLet
    `pAlt` pLetRec
    -- `pAlt` pCase
    `pAlt` pLambda
    `pAlt` pExpr1

pAppl :: Parser CoreExpr
pAppl = (pOneOrMore pAExpr) `pApply` mk_ap_chain 

pLet :: Parser CoreExpr
pLet = pThen4 mkLet (pLit "let") pDefns (pLit "in") pExpr
    where mkLet _ ds _ ex = ELet False ds ex

pLetRec :: Parser CoreExpr
pLetRec = pThen4 mkLetRec (pLit "letrec") pDefns (pLit "in") pExpr
    where mkLetRec _ ds _ ex = ELet True ds ex

-- | Subdefinitions for pLet and pLetRec
pDefns :: Parser [(String, CoreExpr)]
pDefns = pThen const (pOneOrMore pDefn) (pLit ";")

pDefn :: Parser (String, CoreExpr)
pDefn = pThen3 mkDef pVar (pLit "=") pExpr
   where mkDef v _ ex = (v,ex)

pLambda :: Parser CoreExpr
pLambda = pThen4 mkLambda (pLit "\\") (pOneOrMore pVar) (pLit ".") pExpr
   where mkLambda _ b _ = ELam b

pAExpr :: Parser CoreExpr
pAExpr = pApply pVar EVar
       `pAlt` pApply pInt ENum
       `pAlt` pConstr
       `pAlt` pParenExpr

-- | Parse a constructor: "Pack{tag,arity}""
pConstr :: Parser CoreExpr
pConstr = pThen6 mkConstr (pLit "Pack") (pLit "{") pInt (pLit ",") pInt (pLit "}") 
  where mkConstr _ _ tag _ arity _ = EConstr tag arity

pParenExpr :: Parser CoreExpr
pParenExpr = pThen3 mkPExpr (pLit "(") pExpr (pLit ")")
    where mkPExpr _ ex _ = ex

-- | Add Eap's between a list of functions / variables
mk_ap_chain :: [CoreExpr] -> CoreExpr
mk_ap_chain exps = foldl EAp (head exps) (tail exps)

-- Functions for the Grammaar expressing operator prcedence (See Figure 1.3) 
data PartialExpr = NoOp | FoundOp String CoreExpr

pExpr1 :: Parser CoreExpr
pExpr1 = pThen assembleOp pExpr2 pExpr1c

pExpr1c :: Parser PartialExpr
pExpr1c = (pThen FoundOp (pLit "|") pExpr1) `pAlt` (pEmpty NoOp)

pExpr2 :: Parser CoreExpr
pExpr2 = pThen assembleOp pExpr3 pExpr2c

pExpr2c :: Parser PartialExpr
pExpr2c = (pThen FoundOp (pLit "&") pExpr2) `pAlt` (pEmpty NoOp)

pExpr3 :: Parser CoreExpr
pExpr3 = pThen assembleOp pExpr4 pExpr3c

pExpr3c :: Parser PartialExpr
pExpr3c = (pThen FoundOp (pSat (\s -> s `elem` relop)) pExpr4) `pAlt` (pEmpty NoOp)

pExpr4 :: Parser CoreExpr
pExpr4 = pThen assembleOp pExpr5 pExpr4c

pExpr4c :: Parser PartialExpr
pExpr4c = (pThen FoundOp (pLit "+") pExpr4) `pAlt` (pThen FoundOp (pLit "-") pExpr5) `pAlt` (pEmpty NoOp)

pExpr5 :: Parser CoreExpr
pExpr5 = pThen assembleOp pExpr6 pExpr5c

pExpr5c :: Parser PartialExpr
pExpr5c = (pThen FoundOp (pLit "*") pExpr5) `pAlt` (pThen FoundOp (pLit "/") pExpr6) `pAlt` (pEmpty NoOp)

pExpr6 :: Parser CoreExpr
pExpr6 =  pAppl

assembleOp :: CoreExpr -> PartialExpr -> CoreExpr
assembleOp e1 NoOp = e1
assembleOp e1 (FoundOp op e2) = EAp (EAp (EVar op) e1) e2

relop :: [String]
relop = ["==", "//=", ">", ">=", "<", "<=", "->"]