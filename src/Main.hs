module Main where

import UserIO

main :: IO ()
main = runProgram =<< parseCLI

-- main :: IO ()
-- main = putStrLn "Hello, Skáld!"
