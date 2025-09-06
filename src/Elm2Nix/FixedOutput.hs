{-# LANGUAGE OverloadedStrings #-}

module Elm2Nix.FixedOutput
  ( FixedDerivation (..),
    prefetch,
  )
where

import qualified Data.ByteString.Lazy.Char8 as BS
import System.Exit
import System.Process

-- fixed output derivation metadata
data FixedDerivation = FixedDerivation
  { -- | Computed sha256 hash
    drvHash :: String,
    -- | Nix store path of the derivation
    drvPath :: String,
    -- | URL to the tarball
    drvUrl :: String,
    drvName :: String,
    drvVersion :: String
  }
  deriving (Show, Eq)

-- | Use nix-prefetch-url to obtain resulting path and it's hash
-- | Partially taken from cabal2nix/src/Distribution/Nixpkgs/Fetch.hs
prefetch :: String -> String -> IO FixedDerivation
prefetch name version = do
  (Nothing, Just stdoutH, _, processH) <-
    createProcess
      (proc "nix-prefetch-url" args)
        { env = Nothing,
          std_in = Inherit,
          std_err = Inherit,
          std_out = CreatePipe
        }
  exitCode <- waitForProcess processH
  case exitCode of
    ExitFailure _ -> error "nix-prefetch-url exited with non-zero"
    ExitSuccess -> do
      buf <- BS.hGetContents stdoutH
      let ls = BS.lines buf
      case length ls of
        0 -> error "unknown nix-prefetch-url output"
        2 ->
          return $
            FixedDerivation
              (BS.unpack $ head ls)
              (BS.unpack $ head $ tail ls)
              url
              name
              version
        _ -> error "unknown nix-prefetch-url output"
  where
    url :: String
    url =
      "https://github.com/"
        ++ name
        ++ "/archive/"
        ++ version
        ++ ".tar.gz"
    args :: [String]
    args = ["--print-path", url]
