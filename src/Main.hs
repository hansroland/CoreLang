-- -----------------------------------------------------------
-- Main.hs
-- -----------------------------------------------------------

-- A preliminary main program

import Parser
import PrettyPrinter
import System.Environment

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
    -- putStrLn "Result:"
    -- putStrln $ showResults . eval . compile
