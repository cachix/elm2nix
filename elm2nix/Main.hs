{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Main
  ( main,
  )
where

import Data.String.Here (hereLit)
import Data.Version (showVersion)
import qualified Elm2Nix
import Options.Applicative
import qualified Paths_elm2nix as This
import qualified Prettyprinter as PP
import System.IO

data Command
  = Init
  | Convert
  | Snapshot {elmJson :: FilePath, writeTo :: FilePath}

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  cmd <- getOpts
  case cmd of
    Convert -> Elm2Nix.convert
    Init -> Elm2Nix.initialize
    Snapshot {elmJson, writeTo} -> Elm2Nix.snapshot elmJson writeTo

getOpts :: IO Command
getOpts = customExecParser p (infoH opts rest)
  where
    infoH a = info (helper <*> a)
    rest =
      fullDesc
        <> progDesc "Convert Elm project to Nix expressions"
        <> header ("elm2nix " ++ showVersion This.version)
        <> footerDoc (Just $ PP.pretty exampleText)
    p = prefs showHelpOnEmpty

    exampleText :: String
    exampleText =
      [hereLit|

    Usage:

      $ elm2nix init > default.nix
      $ elm2nix convert > elm-srcs.nix
      $ elm2nix snapshot
      $ nix-build

      Note: You have to run elm2nix from top-level directory of an Elm project.
    |]

    snapshotOpts :: Parser Command
    snapshotOpts =
      Snapshot
        <$> (arg "elm-json" <|> pure "elm.json")
        <*> (arg "write-to" <|> pure "registry.dat")
      where
        arg name =
          strOption $
            long name <> metavar "FILENAME" <> completer (bashCompleter "file")

    opts :: Parser Command
    opts =
      subparser
        ( command "init" (infoH (pure Init) (progDesc "Generate default.nix (printed to stdout)"))
            <> command "convert" (infoH (pure Convert) (progDesc "Generate Nix expressions for elm.json using nix-prefetch-url"))
            <> command "snapshot" (infoH snapshotOpts (progDesc "Generate registry.dat"))
        )
