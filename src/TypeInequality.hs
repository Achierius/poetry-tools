{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE PolyKinds       #-}

module TypeInequality ( type (!~) ) where

import           GHC.TypeLits


-- |internal type family used to map type equality to a type-level Bool
type family SameType (x :: k) (x' :: k) where
  SameType x x  = True
  SameType x x' = False

-- |type operator producing a representation of the inequality of two given types (of any kind)
type (!~) x x' = False ~ (SameType x x')
