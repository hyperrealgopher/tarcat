{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module TUI.Draw.Search where

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif

import Brick.Forms
  ( renderForm
  )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick

import TUI.Types


-- | For the Draw in the brick App!
searchDraw :: SearchState -> [Widget Name]
searchDraw f = [C.vCenter $ C.hCenter form <=> C.hCenter help]
    where
        form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
        help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
        body = str $ "- You should scan barcode\n" <>
                     "- Search mode\n" <>
                     "- Enter/Esc quit, mouse interacts with fields"
