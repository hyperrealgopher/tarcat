-- FIXME/TODO: need to make KeyView mode and rename Key mode to KeyList or KeySearch mode
-- | Main brick stuff.
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module TUI where

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif

import           Control.Monad.IO.Class
import qualified Graphics.Vty as V
import Brick
import Brick.Forms
  ( formFocus
  , focusedFormInputAttr
  , invalidFormInputAttr
  )
import Brick.Focus
  ( focusRingCursor
  )
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Brick.AttrMap as A
import qualified Data.Vector as Vec

import Catalog
import Types
import TUI.Types
import TUI.Handle.ListKeys
import TUI.Draw.ListKeys
import TUI.Model.ListKeys
import TUI.ViewTape (viewTapeHandleEvent, viewTapeDraw)
import TUI.Types (Name, AppMode(..))
import TUI.Handle.Search (searchHandleEvent)
import TUI.Draw.Search (searchDraw)


appHandleEventDependingMode
  :: AppMode
  -> BrickEvent Name V.Event
  -> EventM Name (Next (AppMode))
appHandleEventDependingMode appMode ev
  -- Ctrl S to go to tape search/new mode
  | ev == VtyEvent (V.EvKey (V.KChar 's') [V.MCtrl]) =
    liftIO (switchToKeyState) >>= continue
  -- TODO/FIXME Ctrl K to go to key search/list mode
  -- | ev == VtyEvent (V.EvKey (V.KChar 'k') [V.MCtrl]) =
appHandleEventDependingMode (Search searchState) ev = searchHandleEvent searchState ev
appHandleEventDependingMode (ViewTape viewTapeState) ev = viewTapeHandleEvent viewTapeState ev
appHandleEventDependingMode (ListKeys keyState) ev = keyHandleEvent keyState ev


appDrawDependingMode :: AppMode -> [Widget Name]
appDrawDependingMode appMode =
  case appMode of
    (Search searchState) -> searchDraw searchState
    (ViewTape viewTapeState) -> viewTapeDraw viewTapeState
    (ListKeys keyState) -> keyDraw keyState


cursorHack
  :: AppMode
  -> [CursorLocation Name]
  -> Maybe (CursorLocation Name)
cursorHack appMode =
  case appMode of
    (Search searchState) -> focusRingCursor formFocus searchState
    (ViewTape viewTapeState) -> focusRingCursor formFocus viewTapeState
    -- FIXME
    (ListKeys keyState) -> showCursorNamed KeyBrickList


theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  -- List stuff
  , (L.listAttr,            V.white `on` V.blue)
  , (L.listSelectedAttr,    V.blue `on` V.white)
  , (customAttr,            fg V.cyan)
  ]


app :: App AppMode V.Event Name
app =
    App { appDraw = appDrawDependingMode
        , appHandleEvent = appHandleEventDependingMode
        , appChooseCursor = cursorHack
        , appStartEvent = return
        , appAttrMap = const theMap
        }
