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
import Control.Monad (liftM2)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Aeson (Value(..))
import Data.List (intercalate, nub)
import Data.HashMap.Strict (HashMap)
import Data.String.Here
import Data.Text (Text)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as HM
#else
import qualified Data.HashMap.Strict as HM
#endif

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as Json
import qualified Data.Text as Text

import Elm2Nix.ElmJson (Dep, Elm2NixError(..), parseElmJsonDeps)
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
  res <- liftIO (fmap Json.eitherDecode (LBS.readFile "elm.json"))
  elmJson <- either (throwErr . ElmJsonReadError) return res

  deps <- either throwErr return (parseElmJsonDeps "dependencies" elmJson)
  testDeps <- either throwErr return (parseElmJsonDeps "test-dependencies" elmJson)
  liftIO (hPutStrLn stderr "Prefetching tarballs and computing sha256 hashes ...")

  sources <- liftIO (mapConcurrently (uncurry prefetch) (nub $ deps ++ testDeps))
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
depErrToStderr err =
  hPutStrLn stderr $
      case err of
        UnexpectedValue v -> "Unexpected Value: \n" ++ show v
        ElmJsonReadError s -> "Error reading json: " ++ s
        KeyNotFound key -> "Key not found in json: " ++ Text.unpack key

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
