{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

  {-# LANGUAGE TemplateHaskell #-}
    {-# LANGUAGE TypeFamilies #-}
      {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric              #-}

  {-# LANGUAGE FlexibleContexts #-}

module Languages (Language(..), SLanguage(..), CLanguage(..), LangString(..),
                  lWords, lLines, lUnwords, lUnlines,
                  reflectLang, extractLang) where

import qualified Data.Text as T
import qualified Data.String as S
import qualified Data.Kind
import qualified Data.Coerce
import qualified GHC.Generics as Gen

import           Control.Lens.Iso (iso)
import qualified Control.Lens.TH as L.TH
import qualified Control.Lens.Wrapped as L.W

import qualified Control.Newtype as NT

{- core language types -}

data Language = English
              | Icelandic
              | Swedish
              | German
              | Finnish
              | Ipa
              deriving (Eq, Show, Read, Enum)


{- types for linguistic text -}

newtype LangString (a ∷ Language) = LangString T.Text
  deriving (Eq, Ord, Semigroup, Monoid, Gen.Generic)
instance Show (LangString (l ∷ Language)) where
  -- TODO: Make this display "LangString 'English ~"
  --       ^ will require XScopedTypeVariables
  show (LangString x) = T.unpack x
instance Read (LangString (l ∷ Language)) where
  readsPrec _ = \x -> [(LangString . T.pack $ x, "")]
instance S.IsString (LangString (l ∷ Language)) where
  fromString = LangString . T.pack
instance L.W.Wrapped (LangString (l ∷ Language))
--  type Unwrapped (LangString l) = T.Text
--  _Wrapped' = iso (\(LangString x) -> x) LangString
instance NT.Newtype (LangString (l ∷ Language)) T.Text

$(L.TH.makeLenses ''LangString)

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
  SFinnish   ∷ SLanguage 'Finnish
  SIpa       ∷ SLanguage 'Ipa

instance CLanguage 'English where
  language = SEnglish
instance CLanguage 'Icelandic where
  language = SIcelandic
instance CLanguage 'Swedish where
  language = SSwedish
instance CLanguage 'German where
  language = SGerman
instance CLanguage 'Finnish where
  language = SFinnish
instance CLanguage 'Ipa where
  language = SIpa


{- functions for runtime reflection on the language underlying a value
 - requires -XKindSignatures
 -}

-- |retrieve Language type at runtime from SLanguage witness
reflectLang ∷ SLanguage l → (a ∷ Language → ★) l → Language
reflectLang SEnglish   _ = English
reflectLang SSwedish   _ = Swedish
reflectLang SIcelandic _ = Icelandic
reflectLang SGerman    _ = German
reflectLang SFinnish   _ = Finnish
reflectLang SIpa       _ = Ipa

-- |take any phantom type parameterized on a Language data type
--  and return the phantom parameter Language value
extractLang ∷ CLanguage l ⇒ (a ∷ Language → ★) l → Language
extractLang = reflectLang language
