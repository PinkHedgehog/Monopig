module Main where

import Monopig4

main :: IO ()
main = do
    code <- getContents
    execM $ fromCodeIO $ read code
