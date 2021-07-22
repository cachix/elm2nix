{-# LANGUAGE QuasiQuotes #-}
module Main
  ( main
  ) where

import Data.Semigroup ((<>))
import Data.Version (showVersion)
import Data.String.Here (hereLit)
import Options.Applicative
import System.IO
import System.Directory (getCurrentDirectory)
import qualified Elm2Nix
import qualified Paths_elm2nix as This
import qualified Text.PrettyPrint.ANSI.Leijen as PP


data Command
  = Init
  | Convert
  | Snapshot

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  cmd <- getOpts
  cwd <- getCurrentDirectory
  case cmd of
    Convert -> Elm2Nix.convert
    Init -> Elm2Nix.initialize
    Snapshot -> Elm2Nix.snapshot cwd

getOpts :: IO Command
getOpts = customExecParser p (infoH opts rest)
  where
    infoH a = info (helper <*> a)
    rest = fullDesc
      <> progDesc "Convert Elm project to Nix expressions"
      <> header ("elm2nix " ++ showVersion This.version)
      <> footerDoc (Just $ PP.string exampleText)
    p = prefs showHelpOnEmpty

    exampleText :: String
    exampleText = [hereLit|

    Usage:

      $ elm2nix init > default.nix
      $ elm2nix convert > elm-srcs.nix
      $ elm2nix snapshot
      $ nix-build

      Note: You have to run elm2nix from top-level directory of an Elm project.
    |]

    opts :: Parser Command
    opts = subparser
      ( command "init" (infoH (pure Init) (progDesc "Generate default.nix (printed to stdout)"))
     <> command "convert" (infoH (pure Convert) (progDesc "Generate Nix expressions for elm.json using nix-prefetch-url"))
     <> command "snapshot" (infoH (pure Snapshot) (progDesc "Generate registry.dat"))
      )
