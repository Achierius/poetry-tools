{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Languages where

import           Data.Maybe
import qualified Data.List as LST
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Kind
import           Text.Regex.PCRE
import           Text.RawString.QQ

-- core language type

data Language = English
              | Icelandic
              | Swedish
              | German
              deriving (Eq, Show, Read, Enum)

-- types for linguistic text

newtype LangString (a::Language) = LangString T.Text
  deriving (Eq, Ord, Show)

newtype IPAString = IPAString T.Text
  deriving (Eq, Ord, Show)

-- singleton implementation
-- requires -XGADTs, -XDataKinds

class CLanguage l where
  language :: SLanguage l

data SLanguage :: Language -> Data.Kind.Type where
  SEnglish   :: SLanguage 'English
  SIcelandic :: SLanguage 'Icelandic
  SSwedish   :: SLanguage 'Swedish
  SGerman    :: SLanguage 'German

instance CLanguage 'English where
  language = SEnglish
instance CLanguage 'German where
  language = SGerman
instance CLanguage 'Icelandic where
  language = SIcelandic
instance CLanguage 'Swedish where
  language = SSwedish


-- functions for runtime reflection on the language underlying a value
-- requires -XKindSignatures

reflectLanguage :: SLanguage l -> (a :: Language -> *) l -> Language
reflectLanguage SEnglish   _ = English
reflectLanguage SGerman    _ = German
reflectLanguage SSwedish   _ = Swedish
reflectLanguage SIcelandic _ = Icelandic

extractLang :: CLanguage l => (a :: Language -> *) l -> Language
extractLang = reflectLanguage language

-- regexes for splitting raw & ipa from csv dictionaries; obsolete?
-- requires -XQuasiQuotes

csvIPARegex :: String
csvIPARegex = [r|(?<=,)(.+)|]
csvRAWRegex :: String
csvRAWRegex = [r|^(.+)(?=,)|]
