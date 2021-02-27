-- | Handle events for the view which lists keys.
module TUI.Handle.ListKeys where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
-- #if !(MIN_VERSION_base(4,11,0))
-- import Data.Monoid
-- #endif
import qualified Graphics.Vty as V
import Lens.Micro ((^.))

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Brick.Types (Widget)
import qualified Brick.Types as T
import Brick.Util (fg, on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (hLimit, str, vBox, vLimit, withAttr, (<+>))
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec

import TUI.Types


keyHandleEvent :: ListKeysState -> T.BrickEvent Name e -> T.EventM Name (T.Next (AppMode))
keyHandleEvent l (T.VtyEvent e) =
    case e of
        V.EvKey (V.KChar '+') [] ->
            let el = nextElement (L.listElements l)
                pos = Vec.length $ l^.(L.listElementsL)
            in M.continue $ ListKeys $ L.listInsert pos el l

        V.EvKey (V.KChar '-') [] ->
            case l^.(L.listSelectedL) of
                Nothing -> M.continue $ ListKeys l
                Just i  -> M.continue $ ListKeys $ L.listRemove i l

        V.EvKey V.KEsc [] -> M.halt $ ListKeys l

        ev -> M.continue =<< (fmap ListKeys $ (L.handleListEventVi L.handleListEvent) ev l)
    where
      nextElement :: Vec.Vector String -> String
      nextElement v = fromMaybe "?" $ Vec.find (flip Vec.notElem v) (Vec.fromList ["wow", "gosh", "gee", "dang"])
keyHandleEvent l _ = M.continue $ ListKeys l
