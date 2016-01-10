{-
 - Lets implement the UNIX echo command
 - The program arguments are simply printed to the standard output.
 - If the first argument is -n, this argument is not printed, and no trailing newline is printed
 -}

import Data.List
import System.Environment
import System.Random  

main = do
    args <- getArgs
    -- case args of ("-n":args') -> (putStr . unwords) args'
    --              _            -> (putStrLn . unwords) args
    -- mapM_ below
    case args of ("-n":args') -> mapM_ putStr (intersperse " " args')
                 _            -> do
                                 mapM_ putStr (intersperse " " args)
                                 putStrLn ""

{- Write a lottery number picker
 - This function should take a StdGen instance, and produce a list of six unique numbers between 1 and 49, in numerical order
 -}
lottery :: StdGen -> [Int]
lottery gen = (sort . take 6 . nub) $ randomRs (1, 49) gen
