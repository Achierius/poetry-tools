{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

--{-# LANGUAGE TemplateHaskell            #-}
--{-# LANGUAGE TypeFamilies               #-}
--{-# LANGUAGE DeriveGeneric              #-}
--{-# LANGUAGE FlexibleContexts           #-}
--{-# LANGUAGE MultiParamTypeClasses      #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Poetics where

import           Data.Maybe
import qualified Data.List              as LST
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Data.ByteString        as BStr

import           Data.String (IsString)
--import qualified Data.Bifunctor      as BF
--import qualified Data.Map as Map
--import           Data.Map (Map)

import           Languages
import           Words

-- TODO: Make this a descendant typeclass of IsString, might need fancy kind
--       extensions of some sort
-- class IsString (s l) ⇒ Allit (s ∷ Language → *) (l ∷ Language) where
--   (~@~) ∷ (l ~ l') ⇒ (s l) → (s l') → Bool
--   (!@~) ∷ (l ~ l') ⇒ (s l) → (s l') → Bool
class Allit (l ∷ Language) where
  (~@~) ∷ LangString l → LangString l → Bool
  (!@~) ∷ LangString l → LangString l → Bool
infix 4 ~@~, !@~

instance Allit Icelandic where
  (~@~) = allitIcelandic
  (!@~) x y = not $ allitIcelandic x y

vowelIcelandic ∷ Char → Bool
vowelIcelandic c = elem c ['œ', 'ɔ', 'ɛ', 'u', 'o', 'i', 'e', 'a', 'ɪ',  'ɣ', 'ʏ']

allitIcelandic ∷ (l ~ l') ⇒ LangString l → LangString l' → Bool
allitIcelandic (LangString x) (LangString x')
  | vowelIcelandic c = vowelIcelandic c'
  | otherwise = T.head x == T.head x'
    where
      c = T.head x
      c' = T.head x'
