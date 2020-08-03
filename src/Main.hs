{-# LANGUAGE TemplateHaskell #-}

module Main where

import UserIO
import Words
import Languages
import Dictionaries

import Data.FileEmbed
import Data.Maybe

import qualified Data.ByteString
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
import qualified Data.Text.Encoding         as T.Encoding

printStrList :: [String] -> IO ()
printStrList = foldr ((>>) . putStrLn) (return ())

main :: IO ()
--main = print (getLifts "ˈæbdəˌkeɪts")
--main = printStrList (getLifts "ˈæbdəˌkeɪts")
--main = print exPoem
main = runProgram =<< parseCLI
-- -- main = do
-- --     let myFile = $(embedFile "dist/resources/sample.txt")
-- --     let string = T.Encoding.decodeUtf8 myFile
-- --     T.IO.putStrLn string
    -- print (show string)
--    mb <- dictLookup English "help"
--    let txt = fromJust mb
--    print txt

-- main :: IO ()
-- main = putStrLn "Hello, Skáld!"
-- əbˈdəkʃənz"
-- æbˌduɫəˈziz

testFile :: Data.ByteString.ByteString
testFile = $(embedFile "dist/resources/sample.txt")

-- How I got it to display properly again:
-- str = Data.Text.Encoding.decodeUtf8 testFile
-- Data.Text.IO.putStrLn str
