{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

  {-# LANGUAGE TypeFamilies #-}
  {-# LANGUAGE PolyKinds   #-}
    {-# LANGUAGE TypeOperators #-}  
    {-# LANGUAGE InstanceSigs #-}  


  {-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
  --  {-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE UnicodeSyntax              #-}
--{-# LANGUAGE TemplateHaskell            #-}
--{-# LANGUAGE TypeFamilies               #-}
--{-# LANGUAGE DeriveGeneric              #-}
--{-# LANGUAGE FlexibleContexts           #-}
--{-# LANGUAGE MultiParamTypeClasses      #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Poetics where

import           Data.Maybe
import qualified Data.List               as LST
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import qualified Data.ByteString         as BStr
import           Data.String (IsString)

--import           GHC.Types
import           GHC.TypeLits (Nat, KnownNat, CmpNat, type (<=), type (-), type (+))
import           Data.Vector.Sized hiding (elem)
--import qualified Data.Bifunctor      as BF
--import qualified Data.Map as Map
--import           Data.Map (Map)

import           Languages
import           TypeInequality

import Data.Proxy
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
vowelIcelandic c = c `elem` ['œ', 'ɔ', 'ɛ', 'u', 'o', 'i', 'e', 'a', 'ɪ',  'ɣ', 'ʏ']

allitIcelandic ∷ (l ~ l') ⇒ LangString l → LangString l' → Bool
allitIcelandic (LangString x) (LangString x')
  | vowelIcelandic c = vowelIcelandic c'
  | otherwise = T.head x == T.head x'
    where
      c = T.head x
      c' = T.head x'

data Stanza (c1 :: Nat) (c2 :: Nat) (c3 :: Nat) (c4 :: Nat) (c5 :: Nat)
  = Stanza { line1 :: Line c1
           , line2 :: Line c2
           , line3 :: Line c3
           , line4 :: Line c4
           , line5 :: Line c5
           }

data Line (c :: Nat) where
    LineA :: (c !~ x, c !~ y) => { hlA :: HalfLine c y
                                 , hlS :: HalfLine c x
                                 } -> Line c
    LineB :: (c !~ x, c !~ y) => { hlB :: HalfLine y c
                                 , hlS :: HalfLine c x
                                 } -> Line c
    LineC :: (c !~ x)         => { hlC :: HalfLine c c
                                 , hlS :: HalfLine c x
                                 } -> Line c

data HalfLine (a :: Nat) (b :: Nat) where
    HalfLine :: ((<=)   (n1 + n2 + n3) 6,
                 CmpNat (n1 + n2     ) 0 ~ 'GT,
                 CmpNat (     n2 + n3) 0 ~ 'GT,
                 CmpNat (n1      + n3) 0 ~ 'GT) =>
                  { clusterA :: Vector n1 Fall
                  , clusterB :: Vector n2 Fall
                  , clusterC :: Vector n3 Fall
                  , staveA   :: Lift a
                  , staveBx  :: Lift b
                  } -> HalfLine a b


data (KnownNat c) => Lift c = Lift (LangString 'Ipa)
data Fall = Fall (LangString 'Ipa)


-- TODO I FEEL LIKE THIS COULD BE BETTER WITH ARROWS...
-- _mkHalfLine :: Maybe (Vector n1 Fall) ->
--                Maybe (Vector n2 Fall) ->
--                Maybe (Vector n3 Fall) ->
--                Lift a -> Lift b ->
--                Maybe (HalfLine a b)
-- _mkHalfLine (Just f1) (Just f2) (Just f3) l1 l2 = Just $ HalfLine f1 f2 f3 l1 l2
-- _mkHalfLine _ _ _ _ _ = Nothing
-- 
-- mkHalfLine :: [Fall] -> [Fall] -> [Fall] -> Lift a -> Lift b -> Maybe (HalfLine a b)
-- mkHalfLine f1 f2 f3 = _mkHalfLine (fromList f1) (fromList f2) (fromList f3)
