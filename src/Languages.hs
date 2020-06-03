{-# LANGUAGE QuasiQuotes #-}

module Languages where

import           Data.Maybe
import qualified Data.List as LST
import qualified Data.ByteString.Lazy.UTF8 as BLU
import           Text.Regex.PCRE
import           Text.RawString.QQ

-- types

data Language = English | Icelandic | Swedish | German
                deriving (Eq, Show, Read, Enum)

type IPAString = BLU.ByteString

-- regexes for splitting raw & ipa from csv dictionaries

csvIPARegex :: String
csvIPARegex = [r|(?<=,)(.+)|]
csvRAWRegex :: String
csvRAWRegex = [r|^(.+)(?=,)|]

-- find line containing word in lang dictionary

dictLookup :: Language -> String -> IO (Maybe Int)
dictLookup lang str = do
    decoratedDict <- readFile (fetchDictionary lang)
    let dictLines = lines decoratedDict
    let rawDict = concat [[m] |
                          (m, n) <- map (break (','==)) dictLines]
    let lineNum = LST.elemIndex str rawDict
    return lineNum

-- IPA lookup from any dictionary

getIPAOnLine :: Language -> Int -> IO IPAString
getIPAOnLine lang lineNo = do
    dict <- readFile (fetchDictionary lang)
    let line = lines dict !! lineNo
    let capturedIPA = line =~ csvIPARegex :: String
    return (BLU.fromString capturedIPA)

-- raw text lookup from any dictionary

getRAWOnLine :: Language -> Int -> IO String
getRAWOnLine lang lineNo = do
    dict <- readFile (fetchDictionary lang)
    let line = lines dict !! lineNo
    let capturedIPA = line =~ csvRAWRegex :: String
    return capturedIPA

-- filesystem location of each language's dictionary

fetchDictionary :: Language -> String
fetchDictionary English = "dist/resources/en/en_US_Processed.csv"
fetchDictionary German = "dist/resources/de/de_Processed.csv"
fetchDictionary Icelandic = "dist/resources/is/is_Processed.csv"
fetchDictionary Swedish = "dist/resources/sv/sv_Processed.csv"
