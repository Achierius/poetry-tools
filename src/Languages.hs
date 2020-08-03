{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Languages where

import           Data.Maybe
import qualified Data.List as LST
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.UTF8 as BLU
import           Text.Regex.PCRE
import           Text.RawString.QQ

-- types

data Language = English | Icelandic | Swedish | German
                deriving (Eq, Show, Read, Enum)

type LangString = T.Text -- used to be BLU.ByteString
type IPAString = T.Text

-- regexes for splitting raw & ipa from csv dictionaries; obsolete?

csvIPARegex :: String
csvIPARegex = [r|(?<=,)(.+)|]
csvRAWRegex :: String
csvRAWRegex = [r|^(.+)(?=,)|]
