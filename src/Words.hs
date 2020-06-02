{-# LANGUAGE OverloadedStrings #-}

module Words where

import           Data.String
import           Text.RawString.QQ
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.UTF8  as BLU


-- TODO later; fromIPA is nontrivial
-- class (Show a) => Phonetic a where 
-- toIPA :: IO a -> IO IPAWord
-- fromIPA :: IO IPAWord -> IO a

halfLineSeparator = "\t\t"
lineSeparator     = "\n"
stanzaSeparator   = "\n\n"


data Language = English | Icelandic | Swedish | OldEnglish
                deriving (Eq, Show, Read, Enum)

data Rule = RuleNothing deriving (Eq)

-- TODO later; only necessary when we want to start typeclassing
-- newtype IPAWord = IPAWord BLU.ByteString deriving (Eq, Show, Read)
-- instance Phonetic IPAWord where
--     toIPA = id
type IPAString = BLU.ByteString
-- TODO make polymorphic
type HalfLine = [T.Text]

data Line = Line { firstLine :: HalfLine
                 , secondLine :: HalfLine
                 } deriving (Eq)
instance Show Line where
    show (Line x y) = (unwords (map T.unpack x))
                   ++ halfLineSeparator
                   ++ (unwords (map T.unpack y))


-- TODO: Redo printing alignment stuff
newtype Stanza = Stanza [Line] deriving (Eq)
instance Show Stanza where
    show (Stanza []) = ""
    show (Stanza [x]) = show x
    show (Stanza (x : xs)) = show x
                          ++ lineSeparator
                          ++ show (Stanza xs)
                          -- TODO: I feel like this is inefficient

data Poem = Poem { stanzas :: [Stanza]
                 , ruleset :: Rule
                 , language :: Language
                 } deriving (Eq)
instance Show Poem where
    show (Poem [] _ _) = ""
    show (Poem [x] _ _) = show x
    show (Poem (x : xs) rls lng) = show x ++
                               stanzaSeparator ++
                               show (Poem xs rls lng)
                               -- TODO: I feel like this is inefficient


-- Example, it works!
exHL1 = map T.pack (words "what the what")
exHL2 = map T.pack (words "zog zog")
exHL3 = map T.pack (words "does this work at all tho")
exHL4 = map T.pack (words "halp")
exHL5 = map T.pack (words "okay this is the REAL test")
exHL6 = map T.pack (words "")
exL1 = Line exHL1 exHL2
exL2 = Line exHL3 exHL4
exL3 = Line exHL5 exHL6
exSZ1 = Stanza [exL1, exL2]
exSZ2 = Stanza []
exSZ3 = Stanza [exL3]
exPoem = Poem [exSZ1, exSZ2, exSZ3] (RuleNothing) English


