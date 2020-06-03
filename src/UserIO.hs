{-# LANGUAGE QuasiQuotes #-}

module UserIO where

-- Temporarily using code from the following tutorial:
-- https://thoughtbot.com/blog/refactoring-to-a-monad-transformer-stack


import           Control.Monad.Writer
import           Data.Bool
import           Data.Char
import           Data.Maybe
import           Data.List
import qualified Data.Bifunctor as Bifunctor
import qualified Control.Exception as Exception
import           Words as W
import qualified Data.ByteString.Lazy.UTF8 as BLU
import           Options.Applicative
import           Text.Regex.PCRE
import           Text.RawString.QQ
-- TODO: Migrate to regex-pcre instead

-- data

engDicLoc = "dist/resources/en/en_US_Processed.csv"
engRAWLoc = "dist/resources/en/en_US_raw.csv"
engIPARegex :: String
engIPARegex = [r|(?<=,)(.+)|]
engRAWRegex :: String
engRAWRegex = [r|^(.+)(?=,)|]

englishVowels = ['ɑ', 'ɪ', 'ə', 'ɔ', 'æ', 'ʒ', 'ʊ', 'i', 'u', 'e']
engVowelsRegx = "[" ++ englishVowels ++ "]"

-- types


data Options = Options
    { oStdIn :: Bool, oTarget :: Maybe String }

-- core logic



runProgram :: Options -> IO ()
runProgram o = do
    let word = fromMaybe "" (oTarget o)
    lineM <- dictLookup English word
    let lineNo = fromMaybe 0 lineM
    wordIPA <- getIPAOnLine English lineNo
    let output = case lineM of
            Just x  -> "IPA for " ++ word ++ ":" ++ 
                       "  /" ++ show wordIPA ++ "/"
            Nothing -> "Failed to lookup word " ++
                       "\"" ++ word ++ "\""
    putStrLn output
    -- let lineStr = fromMaybe "0" (oTarget o)
    -- let lineNum = read lineStr :: Int
    -- wordIPA <- getIPAOnLine lineNum
    -- wordRAW <- getRAWOnLine lineNum
    -- putStrLn ("Word " ++ lineStr ++ ":")
    -- putStrLn ("  English Raw: \"" ++ wordRAW ++ "\"")
    -- putStrLn ("  English IPA: /" ++ show wordIPA ++ "/")

dictLookup :: W.Language -> String -> IO (Maybe Int)
dictLookup lang str = do
    decoratedDict <- readFile (fetchDictionary lang)
    let dictLines = lines decoratedDict
    let rawDict = concat [[m] |
                          (m, n) <- map (break (','==)) dictLines]
    let lineNum = elemIndex str rawDict
    return lineNum

  -- IPA lookup from any dictionary

getIPAOnLine :: W.Language -> Int -> IO IPAString
getIPAOnLine lang lineNo = do
    dict <- readFile (fetchDictionary lang)
    let line = lines dict !! lineNo
    let capturedIPA = line =~ engIPARegex :: String
    return (BLU.fromString capturedIPA)

  -- raw text lookup from any dictionary

getRAWOnLine :: W.Language -> Int -> IO String
getRAWOnLine lang lineNo = do
    dict <- readFile (fetchDictionary lang)
    let line = lines dict !! lineNo
    let capturedIPA = line =~ engRAWRegex :: String
    return capturedIPA

fetchDictionary :: W.Language -> String
fetchDictionary English = "dist/resources/en/en_US_Processed.csv"
fetchDictionary German = "dist/resources/de/de_Processed.csv"
fetchDictionary Icelandic = "dist/resources/is/is_Processed.csv"
fetchDictionary Swedish = "dist/resources/sv/sv_Processed.csv"

-- tutorial code for handling CLI and file IO

loadInputInfo :: Options -> IO (Either String String)
loadInputInfo o =
    maybe defaultResponse readFileFromOptions $ oTarget o
  where
    readFileFromOptions f = Bifunctor.first show <$> safeReadFile f
    defaultResponse = return $ Right "Default Response?"

  -- CLI

parseCLI :: IO Options
parseCLI = execParser (withInfo parseOptions "File Fun")
  where
    withInfo opts h = info (helper <*> opts) $ header h

parseOptions :: Parser Options
parseOptions = Options
    <$> (switch $ long "stdin")
    <*> (optional $ strOption $ long "file")

  -- safer reading of files

safeReadFile :: FilePath -> IO (Either Exception.IOException String)
safeReadFile = Exception.try . readFile
