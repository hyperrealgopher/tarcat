module TUI.Model.ListKeys where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Monoid
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
import Catalog


switchToKeyState :: IO AppMode
switchToKeyState = do
  -- FIXME: needs to be the AppMode version of KeyState since the keystate thingy is just a type synonym
  --let listOfKeys = ...
  --pure $ KeyState brickListCreated
  listOfKeys <- pullAllKeys
  let keysBrickList = L.list KeyBrickList (Vec.fromList $ map show listOfKeys) 1 :: ListKeysState
  pure $ ListKeys keysBrickList


-- FIXME
initialKeyState :: ListKeysState
initialKeyState = L.list KeyBrickList (Vec.fromList ["a","b","c"]) 1
