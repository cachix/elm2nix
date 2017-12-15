{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Data.Semigroup ((<>))
import qualified Control.Monad (join)
import Data.Version (showVersion)
import Data.String.Here
import Options.Applicative
import qualified Lib
import qualified Paths_elm2nix as This
import qualified Text.PrettyPrint.ANSI.Leijen as PP

main :: IO ()
main = do
  cmd <- getOpts
  case cmd of
    Convert -> Lib.convert
    Init -> Lib.init'

getOpts :: IO Command
getOpts = customExecParser p (infoH opts rest)
  where
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
  $ nix-build

  Note: You have to run elm2nix from top-level directory of an Elm project.
|]

convertExample :: String
convertExample = [hereLit|

Steps:

1) resolves `elm-package.json` into pinned `elm-stuff/exact-dependencies.json`
2) converts `elm-stuff/exact-dependencies.json` into Nix expressions by using `nix-prefetch-url`

|]

opts :: Parser Command
opts = subparser
  ( command "init" (infoH (pure Init) (progDesc "Print default.nix to stdout"))
 <> command "convert"  (infoH (pure Convert) (progDesc "Generate Nix expressions for Elm sources" <> footerDoc (Just $ PP.string convertExample))) )

infoH a = info (helper <*> a)

data Command
  = Init
  | Convert
