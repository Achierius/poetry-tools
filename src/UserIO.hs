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

engIPALoc = "dist/resources/en/en_US_Processed.csv"
engIPARegex :: String
engIPARegex = [r|(?<=,)(.+)|]

englishVowels = ['ɑ', 'ɪ', 'ə', 'ɔ', 'æ', 'ʒ', 'ʊ', 'i', 'u', 'e']
engVowelsRegx = "[" ++ englishVowels ++ "]"

-- types

newtype IPAString = IPAString String deriving (Show, Eq)

data Options = Options
    { oStdIn :: Bool, oTarget :: Maybe String }

-- core logic

runProgram :: Options -> IO ()
runProgram o =
    putStrLn =<< getIPALine o

  -- lookup from IPA dictionary

getIPALine :: Options -> IO String
getIPALine o = do
    let option = fromMaybe "0" (oTarget o)
    let lineNo = read option :: Int
    dict <- readFile engIPALoc
    let line = lines dict !! lineNo
    let capturedIPA = line =~ engIPARegex :: [[String]]
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
