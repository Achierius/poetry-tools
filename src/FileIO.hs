{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FileIO where

import           Control.Exception
import qualified Data.List          as LST
import           Data.Maybe
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import           System.IO

import           Dictionaries
import           Languages


readLingFile ∷ FilePath → IO (LangString l)
readLingFile p = do
                 handler ← openFile p ReadMode
                 instr ← hGetContents handler
                 let result = LangString $ T.pack instr
                 return result

ipait ∷ Dict l 'Ipa → LangString 'Ipa → LangString l → [LangString 'Ipa]
ipait d f t = map (fromMaybe f . (`dictLookup` d)) $ lWords t
