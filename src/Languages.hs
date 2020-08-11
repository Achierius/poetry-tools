{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE UnicodeSyntax              #-}

module Languages (Language(..), SLanguage(..), CLanguage(..), LangString(..),
                  lWords, lLines, lUnwords, lUnlines,
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
              | Ipa
              deriving (Eq, Show, Read, Enum)


{- types for linguistic text -}

newtype LangString (a∷Language) = LangString T.Text
  deriving (Eq, Ord, Semigroup, Monoid)
instance Show (LangString (l ∷ Language)) where
  -- TODO: Make this display "LangString 'English ~"
  --       ^ will require XScopedTypeVariables
  show (LangString x) = T.unpack x
instance Read (LangString (l ∷ Language)) where
  readsPrec _ = \x -> [(LangString . T.pack $ x, "")]
instance S.IsString (LangString (l ∷ Language)) where
  fromString = LangString . T.pack


{- helper functions for language strings -}

-- |words for LangString
lWords ∷ LangString l → [LangString l]
lWords (LangString x) = map LangString $ T.words x

lLines ∷ LangString l → [LangString l]
lLines (LangString x) = map LangString $ T.lines x

lUnwords ∷ [LangString l] → LangString l
lUnwords lst = LangString $ T.unwords [x | LangString x ← lst]

lUnlines ∷ [LangString l] → LangString l
lUnlines lst = LangString $ T.unlines [x | LangString x ← lst]


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
  SIpa       ∷ SLanguage 'Ipa

instance CLanguage 'English where
  language = SEnglish
instance CLanguage 'German where
  language = SGerman
instance CLanguage 'Icelandic where
  language = SIcelandic
instance CLanguage 'Swedish where
  language = SSwedish
instance CLanguage 'Ipa where
  language = SIpa


{- functions for runtime reflection on the language underlying a value
 - requires -XKindSignatures
 -}

-- |retrieve Language type at runtime from SLanguage witness
reflectLang ∷ SLanguage l → (a ∷ Language → ★) l → Language
reflectLang SEnglish   _ = English
reflectLang SGerman    _ = German
reflectLang SSwedish   _ = Swedish
reflectLang SIcelandic _ = Icelandic
reflectLang SIpa       _ = Ipa

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
