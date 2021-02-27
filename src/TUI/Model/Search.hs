-- | Models/types for search mode/screen.
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module TUI.Model.Search where

import Brick.Forms
  ( newForm
  , editTextField
  , (@@=)
  )
import Brick

import Types
import TUI.Types


-- | The starting state for the search screen. The "notes" field is hidden for now.
initialSearch :: Tape
initialSearch = Tape { _barcode = ""
                     , _notes = ""
                     }


initialSearchState :: SearchState
initialSearchState = mkForm initialSearch


mkForm :: Tape -> SearchState
mkForm =
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [ label "Barcode" @@=
                   editTextField barcode BarcodeField (Just 1)
               ]
