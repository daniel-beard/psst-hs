module Main where

import System.Environment

import Lib

main :: IO ()
main = do
    stdin <- getContents
    args <- getArgs 
    let inputCommand = unwords args
    entry inputCommand stdin
