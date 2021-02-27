-- | Handler/controller for search screen/mode.
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module TUI.Handle.Search where

import Control.Monad.IO.Class (liftIO)

import qualified Graphics.Vty as V
import Brick
import Brick.Forms (Form, formState, handleFormEvent)

import Types
import Catalog
import TUI.Types
import qualified TUI.ViewTape (mkForm)


-- | Switch modes and perform the search!
-- Switches to view tape mode by pulling up the tape
--viewTapeSwitch :: s
viewTapeSwitch :: Form Tape e n -> IO AppMode
viewTapeSwitch formState' = do
  let tape' = formState formState'
      barcodeInput = _barcode tape'
  tapeRecord <- pullTape barcodeInput
  form <- TUI.ViewTape.mkForm tapeRecord
  pure $ ViewTape form


-- | For the appHandleEvent in the brick App!
searchHandleEvent
  :: SearchState
  -> BrickEvent Name V.Event -> EventM Name (Next AppMode)
searchHandleEvent s ev = do
  case ev of
    VtyEvent (V.EvResize {}) -> continue (Search s)
    VtyEvent (V.EvKey V.KEsc []) -> halt (Search s)
    -- Enter quits only when we aren't in the multi-line editor.
    VtyEvent (V.EvKey V.KEnter []) -> liftIO (viewTapeSwitch s) >>= continue
    _ -> do
      s' <- handleFormEvent ev s
      continue (Search s')
