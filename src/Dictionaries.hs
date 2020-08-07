{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE GADTs #-}

module Dictionaries where

import           Data.Maybe
import qualified Data.List as LST
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B

import qualified Data.FileEmbed as Embed

import Languages

-- types

data DictEntry (a :: Language) = DictEntry (LangString a) IPAString
  deriving (Eq, Ord)

data Dict (a :: Language) = Dict [DictEntry a] -- [(T.Text, T.Text)]
  deriving (Eq, Ord)

data RawDict (a :: Language) = RawDict B.ByteString

data TupleDict (a :: Language) = TupleDict [(T.Text, T.Text)]

-- embed dictionaries at compile-time using Template Haskell

getRawDict :: SLanguage l -> RawDict l
getRawDict SEnglish   = RawDict @'English $(Embed.embedFile
                                 "dist/resources/en/en_US_Processed.csv")
getRawDict SGerman    = RawDict @'German $(Embed.embedFile
                                 "dist/resources/de/de_Processed.csv")
getRawDict SIcelandic = RawDict @'Icelandic $(Embed.embedFile
                                 "dist/resources/is/is_Processed.csv")
getRawDict SSwedish   = RawDict @'Swedish $(Embed.embedFile
                                 "dist/resources/sv/sv_Processed.csv")

joinDict :: Dict l -> Dict l -> Dict l
joinDict (Dict x) (Dict y) = Dict (x ++ y)

catDict :: Dict l -> DictEntry l -> Dict l
catDict (Dict []) y = Dict [y]
catDict (Dict x)  y = Dict (y:x)

-- wrapper for externally fetching dictionaries
getDict :: SLanguage l -> Dict l
getDict = processDict . tupleizeDict . getRawDict

-- core functionality

processDict :: TupleDict e -> Dict e
processDict (TupleDict []) = Dict []
processDict (TupleDict (x:xs)) = catDict (processDict (TupleDict xs))
                                       (DictEntry
                                         (LangString $ fst x)
                                         (IPAString $ snd x))

-- TODO: strip '/' from IPA
-- TODO: handle entries with multiple IPA definitions
tupleizeDict :: RawDict e -> TupleDict e
tupleizeDict (RawDict bytes) = TupleDict
                             (map
                               (\(x, y) -> (x, T.drop 1 y))
                               (map
                                 (\x -> head $ T.breakOnAll "," x)
                                 (T.lines (TE.decodeUtf8 bytes))))

-- could we improve runtime if we guarantee dict sortedness
--   maybe add an attribute to the Dict type?

-- find entry of word in dictionary
dictLookup :: Dict e -> LangString e -> Maybe (DictEntry e)
dictLookup (Dict vals) str = LST.find ((== str) . dictEntryLang) vals

-- -- find entry with equivalent ipa in dictionary
-- dictLookupIPA :: Dict e -> IPAString -> Maybe (DictEntry e)
-- dictLookupIPA (Dict vals) str = LST.find ((== str) . dictEntryIPA) vals

-- -- extract IPA of given dictionary entry
dictEntryIPA :: DictEntry e -> IPAString
dictEntryIPA (DictEntry _ y) = y

-- extract raw language text of given dictionary entry
dictEntryLang :: DictEntry e -> LangString e
dictEntryLang (DictEntry x _) = x

nilDictEntry = DictEntry (LangString "") (IPAString "")
