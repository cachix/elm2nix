{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Elm2Nix
    ( convert
    , initialize
    , snapshot
    ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (liftM2)
import Control.Monad.Except (liftIO, MonadIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Aeson (Value(..))
import Data.List (intercalate)
import Data.HashMap.Strict (HashMap)
import Data.String.Here
import Data.Text (Text)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as Json
import qualified Data.Text as Text
import qualified Data.Either as Either

import Elm2Nix.FixedOutput (FixedDerivation(..), prefetch)
import Elm2Nix.PackagesSnapshot (snapshot)
import qualified Elm2Nix.VersionConstraints as V


newtype Elm2Nix a = Elm2Nix { runElm2Nix_ :: ExceptT Elm2NixError IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

type Dep = (String, String)

data Elm2NixError =
    ElmJsonReadError String
  | UnexpectedValue Value
  | KeyNotFound Text
  deriving Show

runElm2Nix :: Elm2Nix a -> IO (Either Elm2NixError a)
runElm2Nix = runExceptT . runElm2Nix_

throwErr :: Elm2NixError -> Elm2Nix a
throwErr e = Elm2Nix (throwE e)

parseElmJsonDeps :: Value -> Either Elm2NixError [Dep]
parseElmJsonDeps obj =
  case obj of
    Object hm -> do
      deps <- tryLookup hm "dependencies"

      let libstyleDeps = parseDeps deps
      if Either.isRight libstyleDeps then
        libstyleDeps
      else
        case deps of
          Object dhm -> do
            direct   <- tryLookup dhm "direct"
            indirect <- tryLookup dhm "indirect"
            liftM2 (++) (parseDeps direct) (parseDeps indirect)
          v -> Left (UnexpectedValue v)
    v -> Left (UnexpectedValue v)
  where
    parseDep :: Text -> Value -> Either Elm2NixError Dep
    parseDep name (String ver) =
      let
        libVer = V.lowest <$> V.fromString (Text.unpack ver)
      in
      case libVer of
       Right ver -> Right (Text.unpack name, V.versionToString ver)
       Left _    ->
         case V.versionFromString (Text.unpack ver) of
           Just v   -> Right (Text.unpack name, V.versionToString v)
           Nothing  -> Left (ElmJsonReadError ("Error reading version constraint for '" <> Text.unpack name <> "' package."))
    parseDep _ v = Left (UnexpectedValue v)

    parseDeps :: Value -> Either Elm2NixError [Dep]
    parseDeps (Object hm) = mapM (uncurry parseDep) (HM.toList hm)
    parseDeps v           = Left (UnexpectedValue v)

    maybeToRight :: b -> Maybe a -> Either b a
    maybeToRight _ (Just x) = Right x
    maybeToRight y Nothing  = Left y

    tryLookup :: HashMap Text Value -> Text -> Either Elm2NixError Value
    tryLookup hm key =
      maybeToRight (KeyNotFound key) (HM.lookup key hm)

-- CMDs

convert :: IO ()
convert = runCLI $ do
  liftIO (hPutStrLn stderr "Resolving elm.json dependencies into Nix ...")
  res <- liftIO (fmap Json.eitherDecode (LBS.readFile "elm.json"))
  elmJson <- either (throwErr . ElmJsonReadError) return res

  deps <- either throwErr return (parseElmJsonDeps elmJson)
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
