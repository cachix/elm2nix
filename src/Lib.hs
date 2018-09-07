{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
    ( convert
    , init'
    ) where

import Control.Concurrent.Async
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad (liftM2)
import Control.Monad.Except (liftIO, MonadIO)
import Data.Aeson (Value(..))
import Data.List (intercalate)
import Data.HashMap.Strict (HashMap)
import Data.String.Here
import Data.Text (Text)
import System.Exit (exitFailure)
import System.IO ( hPutStrLn, stderr)

import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as Json
import qualified Data.Text as Text

import Prefetch

newtype Elm2Nix a = Elm2Nix { runElm2Nix_ :: ExceptT Elm2NixError IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

type Dep = (String, String)

data Elm2NixError =
    ElmJsonReadError String
  | UnexpectedValue Value
  | KeyNotFound Text
  deriving Show

runElm2Nix = runExceptT . runElm2Nix_

throwErr :: Elm2NixError -> Elm2Nix a
throwErr e = Elm2Nix (throwE e)

parseDep :: Text -> Value -> Either Elm2NixError Dep
parseDep name (String ver) = Right (Text.unpack name, Text.unpack ver)
parseDep _ v               = Left (UnexpectedValue v)

parseDeps :: Value -> Either Elm2NixError [Dep]
parseDeps (Object hm) = mapM (uncurry parseDep) (HM.toList hm)
parseDeps v           = Left (UnexpectedValue v)

maybeToRight :: b -> Maybe a -> Either b a
maybeToRight _ (Just x) = Right x
maybeToRight y Nothing  = Left y

tryLookup :: HashMap Text Value -> Text -> Either Elm2NixError Value
tryLookup hm key =
  maybeToRight (KeyNotFound key) (HM.lookup key hm)

parseElmJsonDeps :: Value -> Either Elm2NixError [Dep]
parseElmJsonDeps obj =
  case obj of
    Object hm ->
      do deps     <- tryLookup hm "dependencies"
         case deps of
           Object dhm -> do
             direct   <- tryLookup dhm "direct"
             indirect <- tryLookup dhm "indirect"
             liftM2 (++) (parseDeps direct) (parseDeps indirect)

           v -> Left (UnexpectedValue v)
    v ->
      Left (UnexpectedValue v)


-- CMDs

convert :: IO ()
convert = runCLI solveDependencies


init' :: IO ()
init' = runCLI (generateDefault "elm-app" "0.1.0")

-- Utils

depErrToStderr :: Elm2NixError -> IO ()
depErrToStderr err =
  let
    humanErr =
      case err of
        UnexpectedValue v -> "Unexpected Value: \n" ++ show v
        ElmJsonReadError s -> "Error reading json: " ++ s
        KeyNotFound key -> "Key not found in json: " ++ Text.unpack key
  in
    hPutStrLn stderr humanErr

runCLI :: Elm2Nix a -> IO a
runCLI m = do
  result <- runElm2Nix m
  case result of
        Right a ->
          return a

        Left err -> do
          depErrToStderr err
          exitFailure

generateDefault :: Text -> Text -> Elm2Nix ()
generateDefault baseName version = do
  let name = Text.unpack (toNixName baseName <> "-" <> version)
  let srcdir = "." :: String
  liftIO (putStrLn [template|data/default.nix|])

readElmJson :: Elm2Nix Value
readElmJson = do
  res <- liftIO (fmap Json.eitherDecode (LBS.readFile "elm.json"))
  either (throwErr . ElmJsonReadError) return res

solveDependencies :: Elm2Nix ()
solveDependencies = do
  liftIO (hPutStrLn stderr "Resolving elm.json dependencies into Nix ...")
  elmJson <- readElmJson

  deps <- either throwErr return (parseElmJsonDeps elmJson)
  liftIO (hPutStrLn stderr "Prefetching tarballs and computing sha256 hashes ...")

  sources <- liftIO (mapConcurrently (uncurry Prefetch.prefetchURL) deps)
  liftIO (putStrLn (generateNixSources sources))

generateNixSource :: DerivationSource -> String
generateNixSource ds =
  -- TODO: pass name to fetchzip
  [i|  "${drvName ds}" = {
    src = fetchzip {
      url = "${drvUrl ds}";
      sha256 = "${drvHash ds}";
    };
    version = "${drvVersion ds}";
  };|]

generateNixSources :: [DerivationSource] -> String
generateNixSources dss =
  [iTrim|
{ fetchzip }: {
${intercalate "\n" (map generateNixSource dss)}
}
  |]

-- | Converts Package.Name to Nix friendly name
toNixName :: Text -> Text
toNixName = Text.replace "/" "-"
