{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{-# LANGUAGE TemplateHaskell   #-}
module Main where


import qualified Data.ByteString      as BS
import qualified Data.FileEmbed       as Embed
import qualified Data.List            as LST
import qualified Data.List            as LST
import           Data.Maybe
import qualified Data.MonoTraversable as Mono
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T.Encoding
import qualified Data.Text.IO         as T.IO

import           Dictionaries
import           FileIO
import           Languages
import           Languages
import           Poetics
import           UserIO
import           UserIO
import           Words
import           Words

printStrList :: [String] -> IO ()
printStrList = foldr ((>>) . putStrLn) (return ())

--dictt = getPDict SIcelandic
dictt = getPDict SIcelandic
translator = ipait dictt "ERROR"
sagaloc = "resources/texts/is/Heimskringla/Yngling_Saga.txt"
--sagaloc = "resources/texts/is/Poetic_Edda/Hávamál.txt"
--sagaloc = "resources/texts/is/Poetic_Edda/Hávamál_Stanza_1.txt"
--sagaloc = "resources/texts/fi/Kalevala/Kalevala.txt"

testAmal = $(Embed.embedFile "resources/texts/is/Poetic_Edda/Hávamál_Stanza_1.txt")


toText :: LangString l -> T.Text
toText (LangString x) = x


getAllits :: BS.ByteString -> LangString 'Icelandic -> [LangString 'Icelandic]
getAllits bytes word = [words | words <- decoded, allitIcelandic words word]
                         where
                           decoded = map
                                       (cleanWord . LangString)
                                       (T.words (T.Encoding.decodeUtf8 bytes))


main :: IO ()
  {- main = runProgram =<< parseCLI -} -- CANONICAL
main = do
  saga <- readLingFile sagaloc
  let sagaW = map cleanWord $ lWords saga
  --let viable = [x | x <- sagaW, Mono.oelem x dictt]
  let translation = translator $ lUnlines sagaW
  let translated   = [x | x <- sagaW, isJust $ dictLookup x dictt]
  let untranslated = [x | x <- sagaW, isNothing $ dictLookup x dictt]
  let untransList = (LST.nub . LST.sort $ untranslated)
  let transList = (LST.nub . LST.sort $ translated)
  let untransText = lUnwords untransList
  let transText = lUnwords transList
--  print transText
--  print "---"
--  print untransText
--  print "---"
--  print $ "Translated: " ++ (show (length transList))
--  print $ "Untranslated: " ++ (show (length untransList))
  T.IO.writeFile "translated.txt" (toText $ lUnlines (translator $ lUnlines translated))
  --mapM_ (T.IO.appendFile "untranslated.txt") (map ((T.cons '\n') . toText) untransList)


----testFileIOMain :: IO ()
----testFileIOMain = do
----  sagaLang <- readLingFile sagaloc
----  let LangString saga = sagaLang
----  let lowersaga = T.words $ T.toLower saga
----  let freesaga = map (T.dropWhileEnd (`elem` ['.', ',']))
----                     lowersaga
----  let langsaga = map (LangString @'Icelandic) freesaga
----  let help = translator $ lUnwords langsaga
----  print langsaga
----  print (lUnwords help)
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
