-- | Encryption key management.
module Keys (addKeyCommand) where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T

import Catalog (addKey)


-- TODO: echoKey


-- | This is used by the CLI for adding a key from file to the catalog/db.
addKeyCommand :: FilePath -> IO ()
addKeyCommand keyFilePath = do
  keyFileContents <- TIO.readFile keyFilePath
  -- FIXME: you've done this before, getting only the filename in burrow, isntead of full path passed
  _ <- addKey (T.pack keyFilePath) keyFileContents Nothing
  pure ()
