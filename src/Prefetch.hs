{-# LANGUAGE OverloadedStrings #-}
module Prefetch where


import System.Exit
import System.Process
import qualified Data.ByteString.Lazy.Char8 as BS

data DerivationSource = DerivationSource
  { drvHash :: String -- ^ Computed sha256 hash
  , drvPath :: String -- ^ Nix store path of the derivation
  , drvUrl :: String
  , drvName :: String
  , drvVersion :: String
  } deriving (Show, Eq)

-- | Use nix-prefetch-url to obtain resulting path and it's hash
-- | Partially taken from cabal2nix/src/Distribution/Nixpkgs/Fetch.hs
prefetchURL :: String -> String -> IO DerivationSource
prefetchURL name version =
  let url = toZipballUrl name version
      args :: [String]
      args = ["--unpack", "--print-path", url]
  in  do
        (Nothing, Just stdoutH, _, processH) <-
          createProcess (proc "nix-prefetch-url" args) { env     = Nothing
                                                       , std_in  = Inherit
                                                       , std_err = Inherit
                                                       , std_out = CreatePipe
                                                       }

        exitCode <- waitForProcess processH
        case exitCode of
          ExitFailure _ -> error "nix-prefetch-url exited with non-zero"
          ExitSuccess   -> do
            buf <- BS.hGetContents stdoutH
            let ls = BS.lines buf
            case length ls of
              0 -> error "unknown nix-prefetch-url output"
              2 -> return $ DerivationSource (BS.unpack $ head ls)
                                             (BS.unpack $ head $ tail ls)
                                             url
                                             name
                                             version
              _ -> error "unknown nix-prefetch-url output"


toZipballUrl :: String -> String -> String
toZipballUrl name version =
  "https://github.com/"
    ++ name
    ++ "/archive/"
    ++ version
    ++ ".zip"
