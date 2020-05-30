{-# LANGUAGE QuasiQuotes #-}

module UserIO where

-- Temporarily using code from the following tutorial:
-- https://thoughtbot.com/blog/refactoring-to-a-monad-transformer-stack


import           Control.Monad.Writer
import           Data.Bool
import           Data.Char
import           Data.Maybe
import qualified Data.Bifunctor as Bifunctor
import qualified Control.Exception as Exception
import           Options.Applicative
import           Text.Regex.PCRE
import           Text.RawString.QQ
-- TODO: Migrate to regex-pcre instead

-- data

engDicLoc = "dist/resources/en/en_US_Processed.csv"
engIPARegex :: String
engIPARegex = [r|(?<=,)(.+)|]
engRAWRegex :: String
engRAWRegex = [r|^(.+)(?=,)|]

englishVowels = ['ɑ', 'ɪ', 'ə', 'ɔ', 'æ', 'ʒ', 'ʊ', 'i', 'u', 'e']
engVowelsRegx = "[" ++ englishVowels ++ "]"

-- types

newtype IPAString = IPAString String deriving (Eq)
instance Show IPAString where
    show (IPAString a) = a

data Options = Options
    { oStdIn :: Bool, oTarget :: Maybe String }

-- core logic

runProgram :: Options -> IO ()
runProgram o = do
    let lineStr = fromMaybe "0" (oTarget o)
    let lineNum = read lineStr :: Int
    wordIPA <- getIPAOnLine lineNum
    wordRAW <- getRAWOnLine lineNum
    putStrLn ("Word " ++ lineStr ++ ":")
    putStrLn ("  English Raw: \"" ++ wordRAW ++ "\"")
    putStrLn ("  English IPA: /" ++ show wordIPA ++ "/")

  -- IPA lookup from english dictionary

getIPAOnLine :: Int -> IO IPAString
getIPAOnLine lineNo = do
    dict <- readFile engDicLoc
    let line = lines dict !! lineNo
    let capturedIPA = line =~ engIPARegex :: [[String]]
    return (IPAString . head . head $ capturedIPA)

  -- raw text lookup from english dictionary

getRAWOnLine :: Int -> IO String
getRAWOnLine lineNo = do
    dict <- readFile engDicLoc
    let line = lines dict !! lineNo
    let capturedIPA = line =~ engRAWRegex :: [[String]]
    return (head . head $ capturedIPA)

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
