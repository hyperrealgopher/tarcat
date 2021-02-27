{-# LANGUAGE OverloadedStrings #-}
{- | Usage:

Use the TUI:
  tarcat tui

Load a key file:
  tarcat addkey somekey.key --title="some title" --comment="some comment optional"

honestly should just use the key file name as the title

Echo a key file to stdout:
  tarcat echokey "some title"

Add a barcode/tape to the catalog:
  tarcat addtar "barcodehere" "optional note" "some key title"

-}
module Main where

import Data.Semigroup ((<>))

import Options.Applicative hiding (ParseError)
import qualified Graphics.Vty as V
import Brick hiding (str)

import Catalog (setupDatabase)
import Keys
import TUI.Types (AppMode(..))
import TUI
import TUI.Model.Search


data MainOpts = MainOpts
  { optCommand :: MainCommand -- Should be a FilePath
  }


data AddKeyOpts = AddKeyOpts
  { addKeyOptFile :: !FilePath
  }


--data MainCommand = TUI | AddKey AddKeyOpts | Setup
data MainCommand = AddKey FilePath | Setup | TUI


doTheTUI :: IO ()
doTheTUI = do
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v

  initialVty <- buildVty
  _ <- customMain initialVty buildVty Nothing app (Search initialSearchState)
  pure ()


--fullDesc = "Manage a collection of (possibly encrypted) tapes using barcodes."

main :: IO ()
main = do
  opts <- execParser mainParserWrapped
  case optCommand opts of
    TUI -> doTheTUI
    Setup -> setupDatabase
    AddKey keyFilePath -> addKeyCommand keyFilePath
 where
  -- "Before we can run a `Parser` nwe need to wrap it into a ParserInfo structure,
  -- that specifies a number of properties that only apply to top level parsers,
  -- such as a header describing what the program does, to be displayed in the help
  -- screen."
  -- From https://www.stackage.org/package/optparse-applicative
  mainParserWrapped :: ParserInfo MainOpts
  mainParserWrapped = info (mainParser <**> helper)
    ( fullDesc
    <> progDesc "Tape catalog management."
    <> header "tarcat - tape archive catalog" )

  mainParser :: Parser MainOpts
  mainParser = MainOpts <$> hsubparser
    ( command "addkey" (info addKeyOptions ( progDesc "Add key file to catalog." ) )
    <> command "setup" (info (pure Setup) (progDesc "Initialize the catalog database."))
    <> command "tui" (info (pure TUI) (progDesc "Initialize the textual user interface."))
    )

  --addKeyCommand' :: Parser MainOpts
  --addKeyCommand' = AddKey <$> (argument (pure AddKeyOpts) (metavar "TARGET..."))
  addKeyOptions = AddKey <$> strArgument (metavar "FILENAME" <> help "Path to key")
{-
  optsParser :: ParserInfo Opts
  optsParser =
    info
      (helper <*> versionOption <*> programOptions)
      (fullDesc <> progDesc "tarcat " <>
       header "tarcat: Catalog magnetic tape archives and store their keys.")

  versionOption :: Parser (a -> a)
  versionOption = infoOption "0.0" (long "version" <> help "Show version")

  programOptions :: Parser Opts
  programOptions = Opts <$> hsubparser ( tuiCommand <> setupDatabaseCommand) <*> addKeyCommand'

  setupDatabaseCommand :: Mod CommandFields Command
  setupDatabaseCommand = command "setup" (info (pure Setup) (progDesc "Setup the catalog database."))

  --addKeyCommand' :: Parser Command
  --addKeyCommand' :: Parser Command
  --addKeyCommand' = command "addkey" (info (pure AddKey) (progDesc "Add tape encryption key to catalog."))
  addKeyCommand' = subparser
    (
      command "addKey" (info (pure AddKey) (progDesc "Add tape encryption key to catalog.") )
    )

  tuiCommand :: Mod CommandFields Command
  tuiCommand = command "tui" (info (pure TUI) (progDesc "Launch the textual user interface."))
-}
