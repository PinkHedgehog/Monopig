module Main where

import Monopig4
import Control.Monad (when)
import System.Environment
--[PRTS \"input first number\",ASK,PRTS \"input second number\",ASK,REP [PRT,DUP,INC],PRT]
main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "No input files"
        else do
            src <- readFile (head args)
            putStrLn . show $ last src
            if (last src == '.' || last src == '\n')
                then do
                    let code = read $! init src
                    print code
                    vm <- execM $! fromCodeIO code
                    print vm
                else print (last src)
