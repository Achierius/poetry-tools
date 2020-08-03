{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Dictionaries where

import           Data.Maybe
import qualified Data.List as LST
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B

import qualified Data.FileEmbed as Embed

import Languages

-- types

type DictEntry = (LangString, LangString)
data Dictionary = Dictionary Language [DictEntry] -- [(T.Text, T.Text)]
data RawDictionary = RawDictionary Language B.ByteString

-- embed dictionaries at compile-time using Template Haskell

getRawDictionary :: Language -> RawDictionary
getRawDictionary English   = RawDictionary English $(Embed.embedFile
                                   "dist/resources/en/en_US_Processed.csv")
getRawDictionary German    = RawDictionary English $(Embed.embedFile
                                   "dist/resources/de/de_Processed.csv")
getRawDictionary Icelandic = RawDictionary English $(Embed.embedFile
                                   "dist/resources/is/is_Processed.csv")
getRawDictionary Swedish   = RawDictionary English $(Embed.embedFile
                                   "dist/resources/sv/sv_Processed.csv")

-- wrapper for externally fetching dictionaries
-- TODO: make this "cache" already-processed dictionaries in some way?
--       or will haskell do that on its own...?
getDictionary :: Language -> Dictionary
getDictionary = processDict . getRawDictionary

-- core functionality

-- TODO: strip '/' from IPA
-- TODO: handle entries with multiple IPA definitions
processDict :: RawDictionary -> Dictionary
processDict (RawDictionary lang bytes) = 
                           Dictionary
                             lang
                             (map
                               (\(x, y) -> (x, T.drop 1 y))
                               (map
                                 (\x -> head $ T.breakOnAll "," x)
                                 (T.lines (TE.decodeUtf8 bytes))))

-- could we improve runtime if we guarantee dict sortedness
--   maybe add an attribute to the Dictionary type?

-- need to add language annotation to LangStrings...


-- find entry of word in dictionary
dictLookup :: Dictionary -> LangString -> Maybe DictEntry
dictLookup (Dictionary _ vals) str = LST.find ((== str) . fst) vals

-- find entry with equivalent ipa in dictionary
dictLookupIPA :: Dictionary -> LangString -> Maybe DictEntry
dictLookupIPA (Dictionary _ vals) str = LST.find ((== str) . snd) vals

-- extract IPA of given dictionary entry
dictEntryIPA :: DictEntry -> IPAString
dictEntryIPA = snd

-- extract raw language text of given dictionary entry
dictEntryLang :: DictEntry -> LangString
dictEntryLang = fst
