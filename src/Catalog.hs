-- | The sqlite stuff; the tape catalog data.
{-# LANGUAGE OverloadedStrings #-}
module Catalog
  ( setupDatabase
  , pullTape
  , updateTape
  , addKey
  , pullAllKeys
  , pullKey
  )
where

import Data.Text (Text)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

import Types (Tape(..), TapeEncryptionKey(..), KeySummary(..))


databaseFileName = "tarcat.db"

-- | Create the tape database and table.
setupDatabase :: IO ()
setupDatabase = do
  conn <- open databaseFileName
  -- FIXME: use ID for tapes too so you can replace barcodes?
  execute_ conn "CREATE TABLE tapes (barcode TEXT PRIMARY KEY, notes TEXT, encrypt_key INTEGER, FOREIGN KEY(encrypt_key) REFERENCES encrypt_keys(id))"
  execute_ conn "CREATE TABLE encrypt_keys (id INTEGER PRIMARY KEY, name TEXT NOT NULL UNIQUE, contents TEXT NOT NULL, notes TEXT)"
  pure ()


instance FromRow Tape where
  --                 barcode   notes                     key id    key name
  fromRow = Tape <$> field <*> field <*> (Just <$> (KeySummary <$> field <*> field))


pullTape :: Text -> IO Tape
pullTape barcode' = do
  conn <- open databaseFileName
  rows <- query conn "SELECT tapes.barcode, tapes.notes, tapes.encrypt_key, encrypt_keys.name FROM tapes LEFT OUTER JOIN encrypt_keys ON tapes.encrypt_key = encrypt_keys.id WHERE barcode = ?" (Only barcode') :: IO [Tape]
  case rows of
    tape:[] -> pure $ tape
    [] -> pure $ Tape barcode' "was empty!" Nothing
    _ -> error "too many matches! should be impossible!"


-- FIXME: return sucecss or failure idk
-- FIXME: shouldnt notes be "maybe?"
-- | If the entry for this barcode doesn't exist, create it!
updateTape :: Text -> Text -> Maybe Int -> IO ()
updateTape barcode' notes' keyId = do
  conn <- open databaseFileName
  execute conn "INSERT OR REPLACE INTO tapes (barcode, notes, encrypt_key) VALUES (?, ?, ?)" (barcode', notes', keyId)


-- | Add new encryption key.
addKey :: Text -> Text -> Maybe Text -> IO ()
addKey keyName keyContents maybeKeyNotes = do
  conn <- open databaseFileName
  case maybeKeyNotes of
    Just keyNotes -> execute conn "INSERT INTO encrypt_keys (name, contents, notes) VALUES (?, ?, ?)" (keyName, keyContents, keyNotes)
    Nothing -> execute conn "INSERT INTO encrypt_keys (name, contents) VALUES (?, ?)" (keyName, keyContents)
  pure ()


instance FromRow TapeEncryptionKey where
  fromRow = TapeEncryptionKey <$> field <*> field <*> field <*> field

-- FIXME: implement
pullAllKeys :: IO [TapeEncryptionKey]
pullAllKeys = do
  conn <- open databaseFileName
  -- shouldn't i be able to set the type here to IO [EncryptionKey] with the right class instance? FIXME
  rows <- query_ conn "SELECT id, name, contents, notes FROM encrypt_keys" :: IO [TapeEncryptionKey]
  pure rows


pullKey :: Int -> IO TapeEncryptionKey
pullKey id' = do
  conn <- open databaseFileName
  rows <- query conn "SELECT id, name, contents, notes FROM encrypt_keys WHERE id = ?" (Only id') :: IO [TapeEncryptionKey]
  case rows of
    key:[] -> pure $ key
    [] -> error "Key does not exist!"
    _ -> error "too many matches! should be impossible!"




-- TODO: update existing encryption key information based on ID

-- | Get the key associated with the provided title.
{-
pullKey :: Text -> IO Key
pullKey keyName = do
  conn <- open "tapecat.db"
  rows <- query conn "SELECT barcode, notes FROM tapes WHERE barcode = ?" (Only barcode') :: IO [(Text, Text)]
  case rows of
    [(_, notes')] -> pure $ Tape barcode' notes'
    [] -> pure $ Tape barcode' ""
    _ -> error "too many matches! should be impossible!"
-}
