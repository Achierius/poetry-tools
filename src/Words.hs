{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Words where

import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import           Data.String
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL

import           Languages

halfLineSeparator = "\t\t"
lineSeparator     = "\n"
stanzaSeparator   = "\n\n"

englishVowels = ['ɑ', 'ɪ', 'ə', 'ɔ', 'æ', 'ʒ', 'ʊ', 'i', 'u', 'e']
engVowelsRegx = "[" ++ englishVowels ++ "]"


data Rule = RuleNothing deriving (Eq)

type HalfLine (l :: Language) = [LangString l]

data Line (l :: Language)
  = Line
      { firstLine  :: [LangString l]
      , secondLine :: [LangString l]
      }
  deriving (Eq)
instance Show (Line (l :: Language)) where
    show (Line x y) = (unwords (map show x))
                   ++ halfLineSeparator
                   ++ (unwords (map show y))


-- TODO: Redo printing alignment stuff
newtype Stanza (l :: Language) = Stanza [Line l] deriving (Eq)
instance Show (Stanza (l :: Language)) where
    show (Stanza []) = ""
    show (Stanza [x]) = show x
    show (Stanza (x : xs)) = show x
                          ++ lineSeparator
                          ++ show (Stanza xs)
                          -- TODO: I feel like this is inefficient

data Poem (l :: Language)
  = Poem
      { stanzas :: [Stanza l]
      , ruleset :: Rule
      }
  deriving (Eq)
instance Show (Poem (l :: Language)) where
    show (Poem [] _ ) = ""
    show (Poem [x] _ ) = show x
    show (Poem (x : xs) rls) = show x ++
                               stanzaSeparator ++
                               show (Poem xs rls)
                               -- TODO: I feel like this is inefficient


-- Example, it works!
exHL1 = map (LangString @'English) (T.words "what the what")
exHL2 = map (LangString @'English) (T.words "zog zog")
exHL3 = map (LangString @'English) (T.words "does this work at all tho")
exHL4 = map (LangString @'English) (T.words "halp")
exHL5 = map (LangString @'English) (T.words "okay this is the REɑɑɑɑL test")
exHL6 = map (LangString @'English) (T.words "")
exL1 = Line exHL1 exHL2
exL2 = Line exHL3 exHL4
exL3 = Line exHL5 exHL6
exSZ1 = Stanza [exL1, exL2]
exSZ2 = Stanza []
exSZ3 = Stanza [exL3]
exPoem = Poem @'English [exSZ1, exSZ2, exSZ3] (RuleNothing)

exHL1_G = map (LangString @'German) (T.words "nice")

-- correctly fails to compile
-- exL1_G = Line exHL1_G exHL1
