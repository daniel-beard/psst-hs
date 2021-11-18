module Main where

import System.Environment

import Lib

main :: IO ()
main = do
    stdin <- getContents
    args <- getArgs 
    print args
    putStr "stdin: "
    print stdin
    let inputCommand = unwords args
    run inputCommand stdin
