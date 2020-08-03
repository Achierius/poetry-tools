{-# LANGUAGE OverloadedStrings #-}

module UserIO where

-- Temporarily using code from the following tutorial:
-- https://thoughtbot.com/blog/refactoring-to-a-monad-transformer-stack

import           Data.Maybe
import qualified Data.Bifunctor as Bifunctor
import qualified Control.Exception as Exception
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Text as T
import           Options.Applicative
import           Languages
import           Dictionaries
import qualified Data.Text.IO

-- types

data Options = Options
    { oStdIn :: Bool, oTarget :: Maybe String }

-- core logic

runProgram :: Options -> IO ()
runProgram o = do
    let word = T.pack $ fromMaybe "" (oTarget o)
    let dict = getDictionary English
    let entry = dictLookup dict word
    let output = case entry of
            Just x  -> "IPA for " <> word <> ":" <> 
                       "  /" <> (dictEntryIPA x) <> "/"
            Nothing -> "Failed to lookup word " <>
                       "\"" <> word <> "\""
    Data.Text.IO.putStrLn output                   

-- runProgram :: Options -> IO ()
-- runProgram o = do
--     let word = fromMaybe "" (oTarget o)
--     lineM <- dictLookup English word
--     let lineNo = fromMaybe 0 lineM
--     wordIPA <- getIPAOnLine English lineNo
--     let output = case lineM of
--             Just x  -> "IPA for " ++ word ++ ":" ++ 
--                        "  /" ++ show wordIPA ++ "/"
--             Nothing -> "Failed to lookup word " ++
--                        "\"" ++ word ++ "\""
--     putStrLn output

-- tutorial code for handling CLI and file IO

loadInputInfo :: Options -> IO (Either String String)
loadInputInfo o =
    maybe defaultResponse readFileFromOptions $ oTarget o
  where
    readFileFromOptions f = Bifunctor.first show <$> safeReadFile f
    defaultResponse = return $ Right "Default Response?"

-- CLI handling

parseCLI :: IO Options
parseCLI = execParser (withInfo parseOptions "File Fun")
  where
    withInfo opts h = info (helper <*> opts) $ header h

parseOptions :: Parser Options
parseOptions = Options
    <$> (switch $ long "stdin")
    <*> (optional $ strOption $ long "file")

-- "safer reading of files"

safeReadFile :: FilePath -> IO (Either Exception.IOException String)
safeReadFile = Exception.try . readFile
