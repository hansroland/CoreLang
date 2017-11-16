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
--import Data.Char

syntax :: [Token] -> CoreProgram
syntax = takeFirstParse . pProgram
    where
      takeFirstParse ((prog, []) : _) = prog
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
pExpr = pAppl
    -- `pAlt` pBinop
    `pAlt` pLet
    `pAlt` pLetRec
    -- `pAlt` pCase
    `pAlt` pLambda
    `pAlt` pAExpr


pAppl :: Parser CoreExpr
pAppl = pThen EAp pExpr pAExpr

pLet :: Parser (Expr String)
pLet = pThen4 mkLet (pLit "let") pDefns (pLit "in") pExpr
    where mkLet _ ds _ ex = ELet False ds ex

pLetRec :: Parser (Expr String)
pLetRec = pThen4 mkLetRec (pLit "let") pDefns (pLit "in") pExpr
    where mkLetRec _ ds _ ex = ELet True ds ex

-- | Subdefinitions for pLet and pLetRec
pDefns :: Parser [(String, CoreExpr)]
pDefns = pOneOrMoreWithSep pDefn (pLit ";")

pDefn :: Parser (String, CoreExpr)
pDefn = pThen3 mkDef pVar (pLit "=") pExpr
   where mkDef v _ ex = (v,ex)

pLambda :: Parser (Expr String)
pLambda = pThen4 mkLambda (pLit "\\") (pOneOrMore pVar) (pLit ".") pExpr
   where mkLambda _ b _ = ELam b

pAExpr :: Parser (Expr String)
pAExpr = pApply pVar EVar
       `pAlt` pApply pInt ENum
       -- `pAlt` pConstr
       `pAlt` pParenExpr

-- pConstr

pParenExpr :: Parser CoreExpr
pParenExpr = pThen3 mkPExpr (pLit "(") pExpr (pLit ")")
    where mkPExpr _ ex _ = ex
