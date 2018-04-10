module PrettyPrintSyntax
  (pprint)
  where

import Syntax
import PrettyPrintBase

-- Define a Pretty Printer for the Core Language
pprExpr :: CoreExpr -> Iseq
pprExpr (ENum n)    = iNum n
pprExpr (EVar v)    = iStr v
pprExpr (EAp e1 e2) = (pprExpr e1 `iAppend` iStr " ") `iAppend` pprAExpr e2
pprExpr (ELet isrec defns expr)
  = iConcat [ iStr keyword, iNewline,
              iIndent (pprDefns defns), iNewline,
              iIndent (iStr "in "), pprExpr expr]
 where
  keyword
     | isrec     = "letrec"
     | otherwise = "let"
pprExpr (ELam vars expr) = iStr "\\ " `iAppend` iInterleave (iStr " ") (map iStr vars) `iAppend` iStr " . " `iAppend` pprExpr expr
pprExpr (EConstr tag arity) = iConcat [ iStr "Pack{", iNum tag, iStr ",", iNum arity, iStr "}" ]
pprExpr (ECase expr as) = iStr "case " `iAppend` pprExpr expr `iAppend` iStr " of " `iAppend` iIndent (pprCases as)

pprAExpr :: CoreExpr -> Iseq
pprAExpr e
   | isAtomicExpr e  = pprExpr e
   | otherwise       = (iStr "(" `iAppend` pprExpr e) `iAppend` iStr ")"

pprDefns :: [(String,CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
   where
   sep = iConcat [iStr ";" , iNewline]

pprDefn :: (String, CoreExpr) -> Iseq
pprDefn (name, expr) = 
   iConcat [iStr name, iStr " ", iStr " = ",  iIndent(pprExpr expr) ]

pprProgram :: [CoreScDefn]  -> Iseq
pprProgram  scDefs  = iInterleave sep (map pprScDef scDefs)
   where
   sep = iConcat [iStr ";" , iNewline]

pprScDef :: (String, [String], CoreExpr) -> Iseq
pprScDef (name, vars ,expr) =
   iConcat [iStr name, iStr " ",
   iInterleave (iStr " ") (map iStr vars), iStr " = ", iIndent(pprExpr expr)]

pprCases :: [Alter String] -> Iseq
pprCases as = iInterleave sep (map pprCase as)
  where 
    sep = iConcat [ iStr " ;", iNewline ]

pprCase :: Alter String -> Iseq
pprCase (tag, as, expr) = prTag `iAppend` iInterleave (iStr " ") (map iStr as) `iAppend` pprExpr expr
  where 
    prTag = iStr "<" `iAppend` iStr (show tag) `iAppend` iStr ">"

pprint :: CoreProgram -> String
pprint prog = iDisplay (pprProgram prog)