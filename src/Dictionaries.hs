{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Dictionaries (DictEntry(..), Dict(..),
                     getDict, dictJoin, dictAppend, 
                     dictLookup, dictLookupIPA,
                     dictEntryLang, dictEntryIPA,
                     nilDict, nilDictEntry) where

import           Data.Maybe
import qualified Data.List as LST
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString
import qualified Data.FileEmbed as Embed

import Languages


{- core types -}

-- |type of a single Word-IPA correspondence in a Dict
data DictEntry (a ∷ Language) = DictEntry (LangString a) (LangString 'Ipa)
  deriving (Eq, Ord)

-- |type containing mappings from words in a language to IPA pronounciations;
--  parameterized on a data-kin of type Language to prevent cross-
--  language contamination
data Dict (a ∷ Language) = Dict [DictEntry a] -- [(T.Text, T.Text)]
  deriving (Eq, Ord)

-- |internal type containing raw underlying dictionary data
data RawDict (a ∷ Language) = RawDict Data.ByteString.ByteString
  deriving (Eq, Ord)

-- |internal dictionary type, Language-annotated at the type level,
--  but with non-annotated text contents.
data TupleDict (a ∷ Language) = TupleDict [(T.Text, T.Text)]
  deriving (Eq, Ord)


{- compile-time dictionary embeddings -}

-- |internal function for embedding dictionary files with TemplateHaskell
getRawDict ∷ SLanguage l → RawDict l
getRawDict SEnglish   = RawDict @'English $(Embed.embedFile
                                 "dist/resources/en/en_US_Processed.csv")
getRawDict SGerman    = RawDict @'German $(Embed.embedFile
                                 "dist/resources/de/de_Processed.csv")
getRawDict SIcelandic = RawDict @'Icelandic $(Embed.embedFile
                                 "dist/resources/is/is_Processed.csv")
getRawDict SSwedish   = RawDict @'Swedish $(Embed.embedFile
                                 "dist/resources/sv/sv_Processed.csv")


{- functions for processing raw dictionaries into Dict type -}

-- |external wrapper for retrieving the dictionary of any given language
getDict ∷ SLanguage l → Dict l
getDict = processDict . rawDictToTupleDict . getRawDict

-- |create Dict from given TupleDict
processDict ∷ TupleDict e → Dict e
processDict (TupleDict []) = Dict []
processDict (TupleDict (x:xs)) = dictAppend (processDict (TupleDict xs))
                                         (DictEntry
                                           (LangString $ fst x)
                                           (LangString @'Ipa $ snd x))

-- TODO: handle entries with multiple IPA definitions
-- TODO: stop this from breaking on invalid dictionaries xd
-- |convert RawDict to TupleDict
rawDictToTupleDict ∷ RawDict e → TupleDict e
rawDictToTupleDict (RawDict bytes) =
                      TupleDict $ map
                                    (\(x, y) → (x, T.drop 1 y))
                                    (map
                                      (\x → head $ T.breakOnAll "," x)
                                      (T.lines (TE.decodeUtf8 bytes)))


{- dictionary utility functions -}

-- |concatenate two same-language dictionaries into one
dictJoin ∷ Dict l → Dict l → Dict l
dictJoin (Dict x) (Dict y) = Dict (x ++ y)

-- |append DictEntry to end of given Dict
dictAppend ∷ Dict l → DictEntry l → Dict l
dictAppend (Dict []) y = Dict [y]
dictAppend (Dict x)  y = Dict (y:x)

-- could we improve runtime if we guarantee dict sortedness
--   maybe add an attribute to the Dict type to improve ord implementation?
-- |find entry of word in dictionary
dictLookup ∷ Dict e → LangString e → Maybe (DictEntry e)
dictLookup (Dict vals) str = LST.find ((== str) . dictEntryLang) vals

-- |find entry with equivalent ipa in dictionary
dictLookupIPA ∷ Dict e → LangString 'Ipa → Maybe (DictEntry e)
dictLookupIPA (Dict vals) str = LST.find ((== str) . dictEntryIPA) vals

-- |extract IPA of given dictionary entry
dictEntryIPA ∷ DictEntry e → LangString 'Ipa
dictEntryIPA (DictEntry _ y) = y

-- |extract raw language text of given dictionary entry
dictEntryLang ∷ DictEntry e → LangString e
dictEntryLang (DictEntry x _) = x

-- empty values for external use
nilDictEntry = DictEntry (LangString "") (LangString @'Ipa "")
nilDict = Dict []
