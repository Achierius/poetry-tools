module Main where

import UserIO
import Words

printStrList :: [String] -> IO ()
printStrList [] = return ()
printStrList (x:xs) = putStrLn x >> printStrList xs

main :: IO ()
--main = print (getLifts "ˈæbdəˌkeɪts")
--main = printStrList (getLifts "ˈæbdəˌkeɪts")
main = print exPoem
--main = runProgram =<< parseCLI

-- main :: IO ()
-- main = putStrLn "Hello, Skáld!"
-- əbˈdəkʃənz"
-- æbˌduɫəˈziz
