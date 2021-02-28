-- FIXME: maybe i should just use Name for the tape since list is beinga pain
-- | View a tape record in brick/the TUI.
-- TODO: barcode validation
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module TUI.ViewTape where

import Control.Arrow ((&&&))
import Data.Maybe
import Control.Monad.IO.Class (liftIO)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
--import Lens.Micro ((^.))
import qualified Data.Vector as Vector

import qualified Graphics.Vty as V
import Brick
import qualified Brick.Widgets.List as L
import Brick.Forms
  ( Form
  , newForm
  , formState
  , formFocus
  , renderForm
  , handleFormEvent
  , editTextField
  , listField
  , (@@=)
  )
import Brick.Focus
  ( focusGetCurrent
  )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C

import Types
import Catalog
import TUI.Types
import TUI.Model.Search (initialSearchState)


-- | The starting state for the view tape screen.
initialTapeInfo :: Tape
initialTapeInfo = Tape { _barcode = ""
                       , _notes = ""
                       }

--initialTapeState :: ViewTapeState
--initialTapeState = mkForm initialTapeInfo

--listItemRenderer :: Bool -> Int -> Widget Name
--listItemRenderer = L.renderList

listDrawElement :: (Show a) => Bool -> a -> Widget Name
listDrawElement sel a =
    let selStr s = if sel
                   then withAttr customAttr (str $ "<" <> s <> ">")
                   else str s
    in C.hCenter $ str "Item " <+> (selStr $ show a)

-- | The brick application state for view tape mode.
--type ViewTapeState = Form FormStateIguess EventIguess Name

-- This form is covered in the Brick User Guide; see the "Input Forms"
-- section.
mkForm :: Tape -> IO ViewTapeState
mkForm t = do
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
        -- The initial key position in the list
        initialKeySelectionState = initialTapeInfo-- should actually take Tape to select
    keys <- pullAllKeys
    let keysVectorFunc _ = Vector.fromList (map (\x -> KeySummary (_keyId x) (_name x)) keys)
        keySelectionField =
          listField keysVectorFunc key listDrawElement 4 KeyField
    let f = newForm [ label "Barcode" @@=
              editTextField barcode BarcodeField (Just 1)
            , label "Notes" @@=
              B.borderWithLabel (str "Notes") @@=
              editTextField notes NotesField (Just 3)
            -- FIXME: should use listField
            , label "Key" @@= keySelectionField
            ]
    pure (f t)


-- TODO: actually shouldn't let you edit barcode
-- | For the Draw in the brick App!
viewTapeDraw :: ViewTapeState -> [Widget Name]
viewTapeDraw f = [C.vCenter $ C.hCenter form <=> C.hCenter help]
    where
        form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
        help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
        body = str $ "- You should scan barcode\n" <>
                     "- Notes should include key\n" <>
                     "- Enter/Esc quit, mouse interacts with fields"


updateBasedOnFormState :: Form Tape e Name -> IO ()
updateBasedOnFormState formState' = do
  let tape' = formState formState'
  let barcodeInput = _barcode tape'
      notesInput = _notes tape'
      keyInput = _key tape'
      keySummary = _keySummaryId <$> keyInput
  updateTape barcodeInput notesInput keySummary


switchToSearch :: AppMode
switchToSearch = Search (initialSearchState)


-- | For the appHandleEvent in the brick App!
viewTapeHandleEvent
  :: ViewTapeState
  -> BrickEvent Name V.Event -> EventM Name (Next AppMode)
viewTapeHandleEvent s ev = do
  case ev of
    VtyEvent (V.EvResize {}) -> continue (ViewTape s)
    VtyEvent (V.EvKey V.KEsc []) -> continue switchToSearch
    -- Enter quits only when we aren't in the multi-line editor.
    VtyEvent (V.EvKey V.KEnter [])
        -- TODO: success or failure message (could just print response from sqlite to a popup? could have status bar at top... could bring back to original screen?)
        | focusGetCurrent (formFocus s) /= Just NotesField -> liftIO (updateBasedOnFormState s) >> continue (ViewTape s)
    _ -> do
      s' <- handleFormEvent ev s
      continue (ViewTape s')
