{-# LANGUAGE BangPatterns #-}
{- Writes a binary serialized package registry for the Elm compiler to consume.

  Takes Elm upstream code from:
  - https://github.com/elm/compiler/blob/master/builder/src/Deps/Registry.hs
  - https://github.com/elm/compiler/blob/master/compiler/src/Elm/Package.hs
-}
{-# LANGUAGE OverloadedStrings #-}

module Elm2Nix.PackagesSnapshot
  ( snapshot,
  )
where

import Control.Monad (liftM2, liftM3)
import Data.Binary (Binary, get, getWord8, put, putWord8)
import qualified Data.Binary as Binary
import Data.Binary.Get.Internal (readN)
import Data.Binary.Put (putBuilder)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word16)
import Elm2Nix.ElmJson (readElmJson, toErrorMessage)

data Name
  = Name
  { _author :: !Text,
    _project :: !Text
  }
  deriving (Eq, Ord)

parseName :: (MonadFail m) => Text -> m Name
parseName n =
  case Text.splitOn "/" n of
    [author, package] -> pure $ Name author package
    lst -> fail $ "wrong package name: " <> show lst

data Package
  = Package
  { _name :: !Name,
    _version :: !Version
  }
  deriving (Eq, Ord)

data Version
  = Version
  { _major :: {-# UNPACK #-} !Word16,
    _minor :: {-# UNPACK #-} !Word16,
    _patch :: {-# UNPACK #-} !Word16
  }
  deriving (Eq, Ord)

parseVersion :: (MonadFail m) => Text -> m Version
parseVersion x =
  case Text.splitOn "." x of
    [major, minor, patch] ->
      return $
        Version
          (read (Text.unpack major))
          (read (Text.unpack minor))
          (read (Text.unpack patch))
    _ ->
      fail "failure parsing version"

data KnownVersions
  = KnownVersions
  { _newest :: Version,
    _previous :: ![Version]
  }

data Registry
  = Registry
  { _count :: !Int,
    _versions :: !(Map Name KnownVersions)
  }

putUnder256 :: BS.ByteString -> Binary.Put
putUnder256 bs =
  do
    putWord8 (fromIntegral (BS.length bs))
    putBuilder (BS.byteString bs)

getUnder256 :: Binary.Get (BS.ByteString)
getUnder256 =
  do
    word <- getWord8
    let !n = fromIntegral word
    readN n id

instance Binary Name where
  get =
    liftM2
      Name
      (fmap Text.decodeUtf8 getUnder256)
      (fmap Text.decodeUtf8 getUnder256)

  put (Name author project) =
    do
      putUnder256 (Text.encodeUtf8 author)
      putUnder256 (Text.encodeUtf8 project)

instance Binary Package where
  get =
    liftM2 Package get get

  put (Package name version) =
    do
      put name
      put version

instance Binary Version where
  get =
    do
      word <- getWord8
      if word == 255
        then liftM3 Version get get get
        else do
          minor <- fmap fromIntegral getWord8
          patch <- fmap fromIntegral getWord8
          return (Version (fromIntegral word) minor patch)

  put (Version major minor patch) =
    if major < 256 && minor < 256 && patch < 256
      then do
        putWord8 (fromIntegral major)
        putWord8 (fromIntegral minor)
        putWord8 (fromIntegral patch)
      else do
        putWord8 255
        put major
        put minor
        put patch

instance Binary KnownVersions where
  get = liftM2 KnownVersions get get
  put (KnownVersions a b) = put a >> put b

instance Binary Registry where
  get = liftM2 Registry get get
  put (Registry a b) = put a >> put b

snapshot :: FilePath -> FilePath -> IO ()
snapshot elmJson writeTo = do
  deps <- either (error . toErrorMessage) id <$> readElmJson elmJson
  let parseDep (k, v) = do
        name <- parseName (Text.pack k)
        version <- parseVersion (Text.pack v)
        pure (name, [version])
  packages <- toKnownVersions . Map.fromListWith (<>) <$> mapM parseDep deps
  let size = Map.foldr' addEntry 0 packages
      registry = Registry size packages

      addEntry :: KnownVersions -> Int -> Int
      addEntry (KnownVersions _ vs) count =
        count + 1 + length vs

  Binary.encodeFile writeTo registry

toKnownVersions :: Map.Map Name [Version] -> Map.Map Name KnownVersions
toKnownVersions =
  fmap
    ( \versions ->
        case List.sortBy (flip compare) versions of
          v : vs -> KnownVersions v vs
          [] -> undefined
    )
