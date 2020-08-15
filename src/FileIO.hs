{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UnicodeSyntax              #-}

module FileIO where

import           Data.Maybe
import qualified Data.List as LST
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Languages
import           Dictionaries
import           Control.Exception

import           System.IO

readLingFile ∷ FilePath → IO (LangString l)
readLingFile p = do
                 handler ← openFile p ReadMode
                 instr ← hGetContents handler
                 let result = LangString $ T.pack instr
                 return result

translateWord ∷ Dict l l' → LangString l → Maybe (LangString l')
translateWord d s = fmap rTerm $ dictLookup d s

ipait ∷ Dict l 'Ipa → LangString 'Ipa → LangString l → [LangString 'Ipa]
ipait d f t = map (fromMaybe f . translateWord d) $ lWords t
