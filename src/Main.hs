module Main where

import UserIO
import Words
import Languages

import Data.Maybe

printStrList :: [String] -> IO ()
printStrList = foldr ((>>) . putStrLn) (return ())

main :: IO ()
--main = print (getLifts "ˈæbdəˌkeɪts")
--main = printStrList (getLifts "ˈæbdəˌkeɪts")
--main = print exPoem
--main = runProgram =<< parseCLI
main = do
    mb <- dictLookup English "help"
    let txt = fromJust mb
    print txt

-- main :: IO ()
-- main = putStrLn "Hello, Skáld!"
-- əbˈdəkʃənz"
-- æbˌduɫəˈziz
