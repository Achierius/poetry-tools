{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UnicodeSyntax              #-}

{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}

module Dictionaries (Dict(..), dMap,
                     getPDict, dictJoin, dictInsert, dictLookup,
                     cleanWord,
                     nilDict) where

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

import qualified Control.Lens as L
import qualified Control.Lens.TH as L.TH
import qualified Data.MonoTraversable as Mono

import           Languages


{- core types -}

-- |type of a single Word-IPA correspondence in a Dict
--data DictEntry (a ∷ Language) (b ∷ Language) = 
--       DictEntry 
--         {
--           lTerm ∷ LangString a
--         , rTerm ∷ LangString b 
--         }
--  deriving (Eq, Ord)


-- |type containing mappings from words in a language to IPA pronounciations;
--  parameterized on a data-kin of type Language to prevent cross-
--  language contamination
data Dict (a ∷ Language) (b ∷ Language) =
        Dict
          {
            _dMap ∷ Map (LangString a) (LangString b)
          }
  deriving (Eq, Ord)
  --deriving (Semigroup, Monoid)
  --  via (Map (LangString a) (LangString b))
nilDict = Dict Map.empty
instance Semigroup (Dict l l') where
  (<>) (Dict m1) (Dict m2) = Dict (m1 <> m2)
instance Monoid (Dict l l') where
  mempty = nilDict

$(L.makeLenses ''Dict)

--type instance Mono.Element (Dict a b) = DictEntry a b
--deriving via (Map (LangString a) (LangString b)) instance
--  Mono.MonoFoldable (Dict a b)

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
getRawPDict SFinnish   = $(Embed.embedFile
                          "resources/ipa/fi/fi_Processed.csv")


{- functions for processing raw dictionaries into Dict type -}

-- |external wrapper for retrieving the phonetic dictionary of any given language
getPDict ∷ SLanguage l → Dict l 'Ipa
getPDict = processDict . getTuplePDict

-- |create Dict from given TupleDict
processDict ∷ TupleDict l l' → Dict l l'
processDict (TupleDict [])= nilDict
processDict (TupleDict (x:xs)) = dictInsert 
                                   (LangString $ fst x)
                                   (LangString $ snd x)
                                   (processDict $ TupleDict xs)

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
cleanWord (LangString w) = LangString (cleanText w)

cleanText ∷ T.Text → T.Text
cleanText w = T.filter (not . (`elem` punctuation)) (T.toLower w)


{- dictionary utility functions -}

-- |concatenate two same-language dictionaries into one
dictJoin ∷ Dict l l' → Dict l l' → Dict l l'
dictJoin (Dict d1) (Dict d2) = Dict (Map.union d1 d2)

-- |append new correspondence to end of given Dict
dictInsert ∷ LangString l → LangString l' → Dict l l' → Dict l l'
dictInsert w1 w2 (Dict d) = Dict (Map.insert w1 w2 d)

dictLookup ∷ LangString e → Dict e e' → Maybe (LangString e')
--dictLookup (Dict vals) str = LST.find ((checkEq str) . lTerm) vals
dictLookup w (Dict d) = Map.lookup (cleanWord w) d

-- |find entry with equivalent right-side term in given dict
--dictRLookup ∷ Dict e e' → LangString e' → Maybe (DictEntry e e')
--dictRLookup (Dict vals) str = LST.find ((checkEq str) . rTerm) vals

-- empty values for external use
--nilDictEntry = DictEntry (LangString "") (LangString @'Ipa "")
