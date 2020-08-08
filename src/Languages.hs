{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Languages (Language(..), SLanguage(..), CLanguage(..),
                  LangString(..), IPAString(..), lWords, lWordsIpa,
                  reflectLang, extractLang) where

import qualified Data.Text as T
import qualified Text.RawString.QQ as R
import qualified Text.Regex.PCRE
import qualified Data.String as S
import qualified Data.Kind

{- core language types -}

data Language = English
              | Icelandic
              | Swedish
              | German
              deriving (Eq, Show, Read, Enum)


{- types for linguistic text -}

newtype LangString (a∷Language) = LangString T.Text
  deriving (Eq, Ord, S.IsString)
instance Show (LangString (l ∷ Language)) where
  show (LangString x) = T.unpack x

newtype IPAString = IPAString T.Text
  deriving (Eq, Ord, S.IsString)
instance Show IPAString where
  show (IPAString x) = T.unpack x

{- helper functions for language strings -}

-- |words for LangString
lWords ∷ LangString l → [LangString l]
lWords (LangString x) = map LangString $ T.words x

-- |words for IPAString
lWordsIpa ∷ IPAString → [IPAString]
lWordsIpa (IPAString x) = map IPAString $ T.words x


{- type safety
 - implements singleton to plug into the phantom types defined above
 - requires -XGADTs, -XDataKinds
 -}

class CLanguage l where
  language ∷ SLanguage l

data SLanguage ∷ Language → Data.Kind.Type where
  SEnglish   ∷ SLanguage 'English
  SIcelandic ∷ SLanguage 'Icelandic
  SSwedish   ∷ SLanguage 'Swedish
  SGerman    ∷ SLanguage 'German

instance CLanguage 'English where
  language = SEnglish
instance CLanguage 'German where
  language = SGerman
instance CLanguage 'Icelandic where
  language = SIcelandic
instance CLanguage 'Swedish where
  language = SSwedish


{- functions for runtime reflection on the language underlying a value
 - requires -XKindSignatures
 -}

-- |retrieve Language type at runtime from SLanguage witness
reflectLang ∷ SLanguage l → (a ∷ Language → ★) l → Language
reflectLang SEnglish   _ = English
reflectLang SGerman    _ = German
reflectLang SSwedish   _ = Swedish
reflectLang SIcelandic _ = Icelandic

-- |take any phantom type parameterized on a Language data type
--  and return the phantom parameter Language value
extractLang ∷ CLanguage l ⇒ (a ∷ Language → ★) l → Language
extractLang = reflectLang language


{- regexes for splitting raw & ipa from csv dictionaries; obsolete?
 - requires -XQuasiQuotes
 -}

csvIPARegex ∷ String
csvIPARegex = [R.r|(?<=,)(.+)|]
csvRAWRegex ∷ String
csvRAWRegex = [R.r|^(.+)(?=,)|]
