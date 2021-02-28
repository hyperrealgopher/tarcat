-- TODO: should i show which tapes use this key in a list?
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module TUI.ViewKey where

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
  , editShowableField
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


initialKey :: TapeEncryptionKey
initialKey =
  TapeEncryptionKey
    { _keyId = 0--FIXME _id needs to be renamed to keyId
    , _name = ""
    , _contents = ""
    , _keyNotes = Nothing
    }


listDrawElement :: (Show a) => Bool -> a -> Widget Name
listDrawElement sel a =
    let selStr s = if sel
                   then withAttr customAttr (str $ "<" <> s <> ">")
                   else str s
    in C.hCenter $ str "Item " <+> (selStr $ show a)


--mkKeyForm :: TapeEncryptionKey -> ViewKeyState
mkKeyForm t = do
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    newForm [ label "ID" @@=
              editShowableField keyId IdField
            , label "Name" @@=
              editTextField name NameField (Just 1)
            , label "Contents" @@=
              editTextField contents ContentsField (Just 1)
            , label "Notes" @@=
              B.borderWithLabel (str "Notes") @@=
              editShowableField keyNotes KeyNotesField
            ]


-- TODO: actually shouldn't let you edit barcode
-- | For the Draw in the brick App!
viewKeyDraw :: ViewKeyState -> [Widget Name]
viewKeyDraw f = [C.vCenter $ C.hCenter form <=> C.hCenter help]
    where
        form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
        help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
        body = str $ "- You should scan barcode\n" <>
                     "- Notes should include key\n" <>
                     "- Enter/Esc quit, mouse interacts with fields"


updateBasedOnFormState :: Form TapeEncryptionKey e Name -> IO ()
updateBasedOnFormState formState' = do
  let tapeEncryptionKey = formState formState'
      idInput = _keyId tapeEncryptionKey-- FIXME: what about ID?!
      nameInput = _name tapeEncryptionKey
      contentsInput = _contents tapeEncryptionKey
      notesInput = _keyNotes tapeEncryptionKey
  pure ()
  --updateKey idInput nameInput contentsInput notesInput


-- FIXME: should switch to listkeys
--switchToSearch :: AppMode
--switchToSearch = Search (initialSearchState)


-- | For the appHandleEvent in the brick App!
viewKeyHandleEvent
  :: ViewKeyState
  -> BrickEvent Name V.Event -> EventM Name (Next AppMode)
viewKeyHandleEvent s ev = do
  case ev of
    VtyEvent (V.EvResize {}) -> continue (ViewKey s)
    --VtyEvent (V.EvKey V.KEsc []) -> continue switchToSearch
    -- Enter quits only when we aren't in the multi-line editor.
    VtyEvent (V.EvKey V.KEnter [])
        -- TODO: success or failure message (could just print response from sqlite to a popup? could have status bar at top... could bring back to original screen?)
        | focusGetCurrent (formFocus s) /= Just KeyNotesField -> liftIO (updateBasedOnFormState s) >> continue (ViewKey s)
    _ -> do
      s' <- handleFormEvent ev s
      continue (ViewKey s')
