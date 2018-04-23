-- ---------------------------------------------------------------------
-- Template Instantiation
-- ---------------------------------------------------------------------
module Template 
   ( eval
   , compile
   , showResults
   ) where 

import Utils.Heap
import Utils.Assoc
import Syntax
import Data.List (mapAccumL, sort)
import PrettyPrintBase

-- | The State of our machine 
type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

-- | The spine stack 
type TiStack = [Addr]

-- | The dump
data TiDump = DummyTiDump

initialTiDump :: TiDump
initialTiDump = DummyTiDump

type TiHeap = Heap Node

type TiGlobals = Assoc String Addr

data TiStats = TiStats Int 

-- | A heap node
data Node = NAp Addr Addr 
          | NSupercomb String [String] CoreExpr
          | NNum  Int  

-- | Take a program and create the initial state
compile :: CoreProgram -> TiState 
compile program = (initialStack, initialTiDump, initialHeap, globals, tiStatInitial)
  where 
    initialStack = [addressOfMain]
    (initialHeap, globals) = buildInitialHeap scDefs 
    addressOfMain = aLookup globals "main" (error "main is not defined")
    scDefs = program ++ preludeDefs ++ extraPreludeDefs

extraPreludeDefs :: [a]
extraPreludeDefs = []

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)     
buildInitialHeap scDefs = mapAccumL allocateSc hInitial scDefs 

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (String, Addr))
allocateSc heap (name, args, body) = (heap', (name, addr))
  where
    (heap', addr) = hAlloc heap (NSupercomb name args body)

-- | do evaluation step by step
eval :: TiState -> [TiState]
eval state = state : restStates
  where 
    restStates | tiFinal state = []
               | otherwise     = eval nextState 
    nextState = doAdmin (step state) 

doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStatIncSteps state

-- | Testing for final step
tiFinal :: TiState -> Bool 
tiFinal ([soleAddr], _, heap, _, _) = isDataNode (hLookup heap soleAddr)
tiFinal ([], _, _, _, _) = error "Empty stack"
tiFinal _ = False

isDataNode :: Node -> Bool
isDataNode (NNum _) = True 
isDataNode _        = False

-- | Taking a step
step :: TiState -> TiState 
step state@(stack, _, heap, _, _) = dispatch (hLookup heap (head stack))
  where
    dispatch (NNum n )                 = numStep state n 
    dispatch (NAp a1 a2)               = apStep state a1 a2
    dispatch (NSupercomb sc args body) = scStep state sc args body

numStep :: TiState -> Int -> TiState 
numStep _ n = error $ "Number applied as a function " ++ show n

apStep :: TiState -> Addr -> Addr -> TiState 
apStep (stack, dump, heap, globals, stats) a1 _ = (a1 : stack, dump, heap, globals, stats)

scStep :: TiState -> String -> [String] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) sc argNames body
     = (new_stack, dump, new_heap, globals, stats)
  where
    new_stack = result_addr : (drop (length argNames+1) stack)
    (new_heap, result_addr) = instantiate body heap env
    env = arg_bindings ++ globals
    stackArgs = getArgs heap stack
    arg_bindings 
      | length stackArgs >= (length argNames) = zip argNames stackArgs
      | otherwise = error $ "Not enough arguments for function " ++ sc
      

getArgs :: TiHeap -> TiStack -> [Addr]
getArgs heap (_ : stack) = map getArg stack
  where  
    getArg addr = arg where (NAp _ arg) = hLookup heap addr
getArgs _ [] = []

--
instantiate :: CoreExpr                  -- Body of supercombinator
               -> TiHeap                 -- Heap before instantiation
               -> Assoc String Addr      -- Association of names to addresses
               -> (TiHeap, Addr)         -- Heap after instantiation, and address of root of instance
instantiate (ENum n) heap _ = hAlloc heap (NNum n)
instantiate (EAp e1 e2) heap env = hAlloc heap2 (NAp a1 a2)
  where
    (heap1, a1) = instantiate e1 heap  env 
    (heap2, a2) = instantiate e2 heap1 env
instantiate (EVar v) heap env = (heap, aLookup env v (error ("Undefined name " ++ show v)))
instantiate (EConstr tag arity) heap env = instantiateConstr tag arity heap env 
instantiate (ELet isrec defs body) heap env = instantiateLet isrec defs body heap env 
instantiate (ECase _ _) _ _ = error "Can't instantiate case exprs"
instantiate (ELam _ _ ) _ _ = error "Can't instantiate lambda exprs"

instantiateConstr :: p1 -> p2 -> p3 -> p4 -> a 
-- instantiateConstr tag arity heap env = undefined
instantiateConstr _ _ _ _ = error "Can't instantiate construtors yet"

instantiateLet :: p1 -> p2 -> p3 -> p4 -> p5 -> a
-- instantiateLet isrec defs body heap env = undefined
instantiateLet _ _ _ _ _ = error "Can't instantiate let(rec)s yet"

-- | Show results
showResults :: [TiState] -> String
showResults states = iDisplay (iConcat [iLayn (map showState states), showStats (last states)])

showState :: TiState -> Iseq 
showState (stack, _, heap, _, _) = iConcat [showStack heap stack, iNewline]

showStack :: TiHeap -> TiStack -> Iseq 
showStack heap stack = iConcat [ 
    iStr "Stk [",
    iIndent (iInterleave iNewline (map showStackItem stack)),
    iStr " ]"
    ]
  where
    showStackItem addr = iConcat [ showFWAddr addr, iStr ": ", 
                                   showStkNode heap (hLookup heap addr)]

showStkNode :: TiHeap -> Node -> Iseq 
showStkNode heap (NAp funAddr argAddr) = 
    iConcat [iStr "NAp ", showFWAddr funAddr, showDetail funAddr, 
             iStr " ", showFWAddr argAddr, showDetail argAddr]
  where 
    showDetail addr = iConcat [ iStr " (", showNode (hLookup heap addr), iStr ")"]
showStkNode _ node = showNode node
  
showNode :: Node -> Iseq 
showNode (NAp a1 a2) = iConcat [iStr "NAp ", showAddr a1, iStr " ", showAddr a2] 
showNode (NSupercomb name _ _) = iStr ("NSupercomb " ++ name) 
showNode (NNum n) = (iStr "NNum ") `iAppend` (iNum n)

showAddr :: Addr -> Iseq 
showAddr addr = iStr (show addr)

showFWAddr :: Addr -> Iseq  
showFWAddr addr = iStr (genSpaces (4 - length str) ++ str)
  where 
    str = show addr

showStats :: TiState -> Iseq 
showStats (_, _, heap, _, stats) = 
    iConcat [iStr "Total number of steps = ",
             iNum (tiStatGetSteps stats),
             iNewline, iNewline,
             showHeap heap]

-- | Print out the contents of the heap
showHeap :: TiHeap -> Iseq
showHeap heap = iConcat [iStr "Heap:",
    iIndent (iConcat (map showHeapAddr addrs))]
  where 
    addrs = sort $ hAddresses heap
    showHeapAddr addr = iConcat [iNewline, iNum addr, iStr " ", showNode (hLookup heap addr)]
      
-- Helper Functions for TiStats 

tiStatInitial :: TiStats 
tiStatInitial = TiStats 0

tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps (TiStats n) = TiStats $ n + 1

tiStatGetSteps :: TiStats -> Int 
tiStatGetSteps (TiStats n) = n

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState 
applyToStats statsFun (stack, dump, heap, scDefs, stats) = (stack, dump, heap, scDefs, statsFun stats)