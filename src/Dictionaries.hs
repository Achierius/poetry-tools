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
                     getPDict, dictJoin, dictInsert, dictLookup,
                     cleanWord,
                     nilDict, nilDictEntry) where

import           Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.List as LST
import qualified Data.Bifunctor as BF
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString
import qualified Data.FileEmbed as Embed
import qualified Data.Map as Map
import           Data.Map (Map)

import Languages


{- core types -}

-- |type of a single Word-IPA correspondence in a Dict
data DictEntry (a ∷ Language) (b ∷ Language) = 
       DictEntry 
         {
           lTerm ∷ LangString a
         , rTerm ∷ LangString b 
         }
  deriving (Eq, Ord)

-- |type containing mappings from words in a language to IPA pronounciations;
--  parameterized on a data-kin of type Language to prevent cross-
--  language contamination
newtype Dict (a ∷ Language) (b ∷ Language) =
          Dict (Map (LangString a) (DictEntry a b))
  deriving (Eq, Ord)

-- |internal dictionary type, Language-annotated at the type level,
--  but with non-annotated text contents.
newtype TupleDict (a ∷ Language) (b ∷ Language) = TupleDict [(T.Text, T.Text)]
  deriving (Eq, Ord)


{- compile-time dictionary embeddings -}

-- |internal function for embedding dictionary files with TemplateHaskell
getRawPDict ∷ SLanguage l → BS.ByteString
getRawPDict SEnglish   = $(Embed.embedFile
                          "resources/ipa/en/en_US_Processed.csv")
getRawPDict SGerman    = $(Embed.embedFile
                          "resources/ipa/de/de_Processed.csv")
getRawPDict SIcelandic = $(Embed.embedFile
                          "resources/ipa/is/is_Processed.csv")
getRawPDict SSwedish   = $(Embed.embedFile
                          "resources/ipa/sv/sv_Processed.csv")


{- functions for processing raw dictionaries into Dict type -}

-- |external wrapper for retrieving the phonetic dictionary of any given language
getPDict ∷ SLanguage l → Dict l 'Ipa
getPDict = processDict . getTuplePDict

-- |create Dict from given TupleDict
processDict ∷ TupleDict l l' → Dict l l'
processDict (TupleDict [])= nilDict
processDict (TupleDict (x:xs)) = dictInsert (DictEntry
                                              (LangString $ fst x)    
                                              (LangString $ snd x))   
                                            (processDict (TupleDict xs))

-- TODO: handle entries with multiple IPA definitions
-- TODO: stop this from breaking on invalid dictionaries xd
-- |convert RawDict to TupleDict
procTupleDict ∷ BS.ByteString → SLanguage l → SLanguage l' → TupleDict l l'
procTupleDict bytes _ _ = 
  TupleDict $ map
    (BF.second (T.drop 1) . head . T.breakOnAll ",")
    (T.lines (TE.decodeUtf8 bytes))

getTuplePDict ∷ SLanguage l → TupleDict l 'Ipa
getTuplePDict l = procTupleDict (getRawPDict l) l SIpa


punctuation = ['.', ',', '“', '„', ';', ':', '"', '\'', '?', '!', '}', '{', '[', ']']

-- |strip punctuation from a word and turn it lowercase
cleanWord ∷ LangString l → LangString l
cleanWord (LangString w) =
  LangString (T.filter (not . (flip elem punctuation)) (T.toLower w))

{- dictionary utility functions -}

checkEq ∷ LangString e' → LangString e' → Bool
checkEq (LangString a) (LangString b) =
  ((T.toLower a) == (T.toLower b))

-- |concatenate two same-language dictionaries into one
dictJoin ∷ Dict l l' → Dict l l' → Dict l l'
dictJoin (Dict x) (Dict y) = Dict (Map.union x y)

-- |append DictEntry to end of given Dict
dictInsert ∷ DictEntry l l' → Dict l l' → Dict l l'
dictInsert y (Dict x) = Dict (Map.insert (cleanWord $ lTerm y) y x)

dictLookup ∷ Dict e e' → LangString e → Maybe (DictEntry e e')
--dictLookup (Dict vals) str = LST.find ((checkEq str) . lTerm) vals
dictLookup (Dict x) y = Map.lookup (cleanWord y) x

-- |find entry with equivalent right-side term in given dict
--dictRLookup ∷ Dict e e' → LangString e' → Maybe (DictEntry e e')
--dictRLookup (Dict vals) str = LST.find ((checkEq str) . rTerm) vals

-- empty values for external use
nilDictEntry = DictEntry (LangString "") (LangString @'Ipa "")
nilDict = Dict Map.empty
