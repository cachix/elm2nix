{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}

module Elm2Nix
    ( convert
    , initialize
    , snapshot
    ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.List (intercalate)
import Data.String.Here
import Data.Text (Text)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import qualified Data.Text as Text

import Elm2Nix.ElmJson (Dep, Elm2NixError(..), readElmJson, toErrorMessage)
import Elm2Nix.FixedOutput (FixedDerivation(..), prefetch)
import Elm2Nix.PackagesSnapshot (snapshot)


newtype Elm2Nix a = Elm2Nix { runElm2Nix_ :: ExceptT Elm2NixError IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runElm2Nix :: Elm2Nix a -> IO (Either Elm2NixError a)
runElm2Nix = runExceptT . runElm2Nix_

throwErr :: Elm2NixError -> Elm2Nix a
throwErr e = Elm2Nix (throwE e)

-- CMDs

convert :: IO ()
convert = runCLI $ do
  liftIO (hPutStrLn stderr "Resolving elm.json dependencies into Nix ...")

  deps <- either throwErr return =<< liftIO readElmJson
  liftIO (hPutStrLn stderr "Prefetching tarballs and computing sha256 hashes ...")

  sources <- liftIO (mapConcurrently (uncurry prefetch) deps)
  liftIO (putStrLn (generateNixSources sources))

initialize :: IO ()
initialize = runCLI $
  liftIO (putStrLn [template|data/default.nix|])
  where
    -- | Converts Package.Name to Nix friendly name
    baseName :: Text
    baseName = "elm-app"
    version :: Text
    version = "0.1.0"
    toNixName :: Text -> Text
    toNixName = Text.replace "/" "-"
    name :: String
    name = Text.unpack (toNixName baseName <> "-" <> version)
    srcdir :: String
    srcdir = "./src" -- TODO: get from elm.json

-- Utils

runCLI :: Elm2Nix a -> IO a
runCLI m = do
  result <- runElm2Nix m
  case result of
        Right a ->
          return a
        Left err -> do
          depErrToStderr err
          exitFailure

depErrToStderr :: Elm2NixError -> IO ()
depErrToStderr = hPutStrLn stderr . toErrorMessage

generateNixSources :: [FixedDerivation] -> String
generateNixSources dss =
  [iTrim|
{
${intercalate "\n" (map f dss)}
}
  |]
  where
    f :: FixedDerivation -> String
    f ds =
      [i|
      "${drvName ds}" = {
        sha256 = "${drvHash ds}";
        version = "${drvVersion ds}";
      };|]
