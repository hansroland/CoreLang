-- -----------------------------------------------------------
-- Main.hs
-- -----------------------------------------------------------

-- A preliminary main program

import Parser
import PrettyPrintSyntax
import System.Environment
import Template

main :: IO ()
main = do 
    args <- getArgs
    if length args == 1 
        then run $ head args 
        else putStrLn "give filename as parameter"

-- | Run the Template Instatiation machine
run :: String -> IO ()
run fn = do 
    inp <- readFile fn 
    let prog = parse inp 
    putStrLn "Program:"
    putStrLn $ pprint prog 
    putStrLn "Results:"
    putStrLn $ showResults $ eval $ compile prog
