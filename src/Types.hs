{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

--import Lens.Micro ((^.))
import Lens.Micro.TH

import Data.Text (unpack, Text)


type Barcode = Text
type Notes = Text


data Tape =
  Tape { _barcode :: Barcode
       , _notes :: Notes
       -- FIXME: starting to think it's a better idea to use an integer and id instead
       , _key :: Maybe KeySummary
       }
       deriving (Show)


-- | Not actually in the catalog, simply summarizes a TapeEncryptionKey
data KeySummary =
  KeySummary { _keySummaryId :: Int
             , _keySummaryName :: Text
             } deriving (Eq)


instance Show KeySummary where
  show k = "#" ++ (show $ _keySummaryId k) ++ ": " ++ (unpack $ _keySummaryName k)


data TapeEncryptionKey =
  TapeEncryptionKey { _keyId :: Int-- FIXME: rename to keyId
                    , _name :: Text
                    , _contents :: Text
                    , _keyNotes :: Maybe Notes
                    }


instance Show TapeEncryptionKey where
  show key = "Key #" ++ id' ++ " (\"" ++ name' ++ "\")" ++ notes'
   where
    id' = show $ _keyId key
    name' = unpack $ _name key

    notes' = case _keyNotes key of
      Nothing -> ""
      Just keyNotes -> ": " ++ (unpack keyNotes)


makeLenses ''Tape
makeLenses ''TapeEncryptionKey
