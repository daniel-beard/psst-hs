module Main where

import System.Environment

import Lib

main :: IO ()
main = do
    args <- getArgs 
    let inputCommand = unwords args
    entry inputCommand
