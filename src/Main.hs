{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications    #-}

module Main where


import           Data.Maybe
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T.IO
import qualified Data.Text.Encoding         as T.Encoding
import qualified Data.List                  as LST

import           UserIO
import           Words
import           Languages
import           Dictionaries
import           FileIO

printStrList :: [String] -> IO ()
printStrList = foldr ((>>) . putStrLn) (return ())

dictt = getPDict SIcelandic
translator = ipait dictt "ERROR"
sagaloc = "resources/texts/is/Heimskringla/Yngling_Saga_Intro.txt"
punctuation = ['.', ',', '“', '„', ';', ':', '"', '\'']

toText :: LangString l -> T.Text
toText (LangString x) = x

cleanWord :: LangString l -> LangString l
cleanWord (LangString w) =
  LangString (T.filter (not . (flip elem punctuation)) (T.toLower w))

main :: IO ()
  {- main = runProgram =<< parseCLI -} -- CANONICAL
main = do
  saga <- readLingFile sagaloc
  let sagaW = lWords saga
  let cleaned = map cleanWord sagaW
  let translation = translator $ lUnwords cleaned
  let untranslated = [x | x <- cleaned, dictLookup dictt x == Nothing]
  let untransText = (lUnlines . LST.nub . LST.sort $ untranslated)
  print untransText 


testFileIOMain :: IO ()
testFileIOMain = do
  sagaLang <- readLingFile sagaloc
  let LangString saga = sagaLang
  let lowersaga = T.words $ T.toLower saga
  let freesaga = map (T.dropWhileEnd (`elem` ['.', ',']))
                     lowersaga 
  let langsaga = map (LangString @'Icelandic) freesaga
  let help = translator $ lUnwords langsaga
  print langsaga
  print (lUnwords help)
  --let trythis = fmap translator saga
--main = print (getLifts "ˈæbdəˌkeɪts")
--main = printStrList (getLifts "ˈæbdəˌkeɪts")
--main = print exPoem
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

-- How I got it to display properly again:
-- str = Data.Text.Encoding.decodeUtf8 testFile
-- Data.Text.IO.putStrLn str
