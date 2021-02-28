{-# LANGUAGE OverloadedStrings #-}
module TUI.Types where

import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V
import Brick.Forms
  ( Form )

import qualified Brick.AttrMap as A

import Types

-- this is for list
customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

type ViewTapeState = Form Tape V.Event Name

type ViewKeyState = Form TapeEncryptionKey V.Event Name

type SearchState = Form Tape V.Event Name

-- FIXME: implement so you can see which tapes are using? KeyViewState... lets you save to location too.
-- | For viewing keys in list mode.
type ListKeysState = L.List Name String

-- FIXME: "Key" should be "KeyList" or "KeySearch"
-- | The screens within the app called "modes."
data AppMode = Search SearchState | ViewTape ViewTapeState | ListKeys ListKeysState | ViewKey ViewKeyState

-- NOTE: these are field names
data Name = BarcodeField
          | NotesField
          | KeyNotesField
          | KeyField
          | KeyBrickList -- FIXME: better name
          | IdField
          | NameField
          | ContentsField
          deriving (Eq, Ord, Show)
