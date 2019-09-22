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
import Data.Vector (Vector)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import qualified System.Directory
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as Json
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Elm2Nix.FixedOutput (FixedDerivation(..), prefetch)
import Elm2Nix.PackagesSnapshot (snapshot)


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

parseElmJsonDeps :: Text -> Value -> Either Elm2NixError [Dep]
parseElmJsonDeps depsKey obj =
  case obj of
    Object hm -> do
      deps <- tryLookup hm depsKey
      case deps of
        Object dhm -> do
          direct   <- tryLookup dhm "direct"
          indirect <- tryLookup dhm "indirect"
          liftM2 (++) (parseDeps direct) (parseDeps indirect)
        v -> Left (UnexpectedValue v)
    v -> Left (UnexpectedValue v)
  where
    parseDep :: Text -> Value -> Either Elm2NixError Dep
    parseDep name (String ver) = Right (Text.unpack name, Text.unpack ver)
    parseDep _ v               = Left (UnexpectedValue v)

    parseDeps :: Value -> Either Elm2NixError [Dep]
    parseDeps (Object hm) = mapM (uncurry parseDep) (HM.toList hm)
    parseDeps v           = Left (UnexpectedValue v)

tryLookup :: HashMap Text Value -> Text -> Either Elm2NixError Value
tryLookup hm key = maybeToRight (KeyNotFound key) (HM.lookup key hm)
  where
    maybeToRight :: b -> Maybe a -> Either b a
    maybeToRight _ (Just x) = Right x
    maybeToRight y Nothing  = Left y

parseElmJsonSrcs :: Value -> Either Elm2NixError (Vector FilePath)
parseElmJsonSrcs obj =
  case obj of
    Object hm -> do
      case tryLookup hm "source-directories" of
          Left _            -> Right Vector.empty
          Right (Array vec) -> mapM extractSrcPath vec
          Right v           -> Left (UnexpectedValue v)
    v -> Left $ UnexpectedValue v
  where
    extractSrcPath :: Value -> Either Elm2NixError String
    extractSrcPath val =
      case val of
          String text -> Right (toNixPath (Text.unpack text))
          v -> Left (UnexpectedValue v)

    toNixPath :: FilePath -> FilePath
    toNixPath path =
      case path of
          "."       -> "./."
          p@('.':_) -> p
          p         -> "./" <> p

-- CMDs

convert :: IO ()
convert = runCLI $ do
  liftIO (hPutStrLn stderr "Resolving elm.json dependencies into Nix ...")
  res <- liftIO (fmap Json.eitherDecode (LBS.readFile "elm.json"))
  elmJson <- either (throwErr . ElmJsonReadError) return res

  deps <- either throwErr return (parseElmJsonDeps "dependencies" elmJson)
  testDeps <- either throwErr return (parseElmJsonDeps "test-dependencies" elmJson)
  liftIO (hPutStrLn stderr "Prefetching tarballs and computing sha256 hashes ...")

  sources <- liftIO (mapConcurrently (uncurry prefetch) (deps ++ testDeps))
  liftIO (putStrLn (generateNixSources sources))

initialize :: IO ()
initialize = runCLI $ do
  liftIO (hPutStrLn stderr "Resolving elm.json source directories into Nix paths...")
  res <- liftIO (fmap Json.eitherDecode (LBS.readFile "elm.json"))
  elmJson <- either (throwErr . ElmJsonReadError) return res

  srcs' <- either throwErr return (parseElmJsonSrcs elmJson)
  liftIO (hPutStrLn stderr $ "Using source directories:")
  liftIO (mapM (hPutStrLn stderr) srcs')
  let srcs = stringifySrcs srcs'
  srcdir <- liftIO (getSrcDir srcs')

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

    getSrcDir :: Vector FilePath -> IO FilePath
    getSrcDir dirs
      | Vector.null dirs = pure "./src"
      | "./." `Vector.elem` dirs || "."  `Vector.elem` dirs =
          -- Nix creates dir named after current directory if `srcs` contains `./.`
        lastDir <$> liftIO System.Directory.getCurrentDirectory
      | otherwise =
          -- Can't fail as there is case for Vec.null!
          pure (Vector.head dirs)

    lastDir :: FilePath -> FilePath
    lastDir = foldl (\path c -> if c == '/' then "" else path <> [c]) ""

    stringifySrcs :: Vector FilePath -> String
    stringifySrcs xs =
      "[\n"
      <> foldr (\i acc -> "    " <> i <> "\n" <> acc) "" xs
      <> "  ]"

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
