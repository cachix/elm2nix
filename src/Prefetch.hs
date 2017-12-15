module Prefetch where

import System.Environment
import System.Exit
import System.Process
import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Elm.Package as Package


data DerivationSource = DerivationSource
  { drvHash :: String -- ^ Computed sha256 hash
  , drvPath :: String -- ^ Nix store path of the derivation
  , drvUrl :: String
  , drvName :: Package.Name
  , drvVersion :: Package.Version
  } deriving (Show, Eq)

instance Show Package.Version where
  show = Package.versionToString

-- | Use nix-prefetch-url to obtain resulting path and it's hash
-- | Partially taken from cabal2nix/src/Distribution/Nixpkgs/Fetch.hs
prefetchURL :: (Package.Name, Package.Version) -> IO DerivationSource
prefetchURL (name, version) =
  let url = toZipballUrl name version
      args :: [String]
      args = ["--unpack", "--print-path", url]
  in  do
        envs <- getEnvironment
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


toZipballUrl :: Package.Name -> Package.Version -> String
toZipballUrl name version =
  "https://github.com/"
    ++ Package.toUrl name
    ++ "/archive/"
    ++ Package.versionToString version
    ++ ".zip"
