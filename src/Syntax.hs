--------------------------------------------------------------------
-- Define the Basic DataTypes to describe the Core Language
--------------------------------------------------------------------

module Syntax
  where

-- | A Core program is just a list of supercombinators
type Program a    = [ScDefn a]
type CoreProgram  = Program String

-- | A Supercombinatordef contains the name, its arguments and its body
type ScDefn  a   = (String, [a], Expr a)
type CoreScDefn  = ScDefn String

-- | The base data structure for teh core language
data Expr a = EVar String            -- Variables
            | ENum Int               -- Numbers
            | EConstr Int Int        -- Contructor tag arity
            | EAp (Expr a) (Expr a)  -- Applications
            | ELet                   -- Let (rec) expresions
                IsRec                --    bolean with True = recursive
                [(a,Expr a)]         --    Definitions
                (Expr a)             --    Body of let(rec)
            | ECase                  -- Case expression
              (Expr a)               --   Expression to scruntinise
              [Alter a]              --   Alternatives
            | ELam [String] (Expr a) -- Lambda abstraction
    deriving (Show) 

type CoreExpr   = Expr String

type Alter a    = (Int, [String], Expr a )            
type CoreAlter  = Alter String


-- Little helpers for the ELet Constructor
type IsRec = Bool


recursive :: Bool
recursive    = True

nonRecursive :: Bool
nonRecursive = False

bindersOf :: [(a,b)] -> [a]
bindersOf defns = [name | (name, _) <- defns]

rhssOf    :: [(a,b)] -> [b]
rhssOf  defns = [rhs | (_ ,rhs) <- defns]
-- 

-- | A function to find out whether we have an atomic expression
isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _)   =  True
isAtomicExpr (ENum _)   =  True
isAtomicExpr _          =  False


-- | Define the Standard Core Prelude
--   I x = x
--   K x y  = x
--   K1 x y = y
--   S f g x = f x (g x)
--   compose f g x = f (g x)
--   twice f = compose f f
preludeDefs :: CoreProgram
preludeDefs
  = [ ("I", ["x"], EVar "x"),
      ("K", ["x", "y"], EVar "x"),
      ("K1", ["x","y"], EVar "y"),
      ("S", ["f","g","x"], EAp (EAp (EVar "f") (EVar "x"))
                               (EAp (EVar "g") (EVar "x"))),
      ("compose", ["f","g","x"], EAp (EVar "f")
                                (EAp (EVar "g") (EVar "x"))),
      ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f"))(EVar "f"))
    ]

