{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module UserIO where

-- Temporarily using code from the following tutorial:
-- https://thoughtbot.com/blog/refactoring-to-a-monad-transformer-stack

import qualified Control.Exception         as Exception
import qualified Data.Bifunctor            as Bifunctor
import qualified Data.ByteString.Lazy.UTF8 as BLU
import           Data.Maybe
import qualified Data.Text                 as T
import qualified Data.Text.IO
import           Options.Applicative

import           Dictionaries
import           Languages

{- main body of program -}

data Options
  = Options
      { oWord   :: String
      , oTarget :: Maybe String
      }

runProgram :: Options -> IO ()
runProgram o = do
    let word = T.pack $ oWord o
    let dict = getPDict SEnglish
    let entry = dictLookup (LangString @'English word) dict
    let output = case entry of
            Just x  -> "IPA for " <> word <> ": " <>
                       "  /" <> (T.pack . show $ x) <> "/"
            Nothing -> "Failed to lookup word " <>
                       "\"" <> word <> "\""
    Data.Text.IO.putStrLn output


{- tutorial code for handling CLI and file IO -}

-- CLI parsing
parseCLI :: IO Options
parseCLI = execParser (withInfo parseOptions "File Fun")
  where
    withInfo opts h = info (helper <*> opts) $ header h

-- CLI options handling
parseOptions :: Parser Options
parseOptions = Options
    <$> (strOption $ long "word")
    <*> (optional $ strOption $ long "file")
