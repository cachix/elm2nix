{- Downloads binary serialized https://package.elm-lang.org/all-packages
   as Elm compiler expects it to parse.

  Takes Elm upstream code from:
  - https://github.com/elm/compiler/blob/master/builder/src/Deps/Cache.hs
  - https://github.com/elm/compiler/blob/master/builder/src/Deps/Website.hs
  - https://github.com/elm/compiler/blob/master/compiler/src/Elm/Package.hs

-}
{-# LANGUAGE OverloadedStrings #-}
module Elm2Nix.PackagesSnapshot
  ( snapshot
  ) where

import Control.Monad (liftM2, liftM3)
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import Data.Binary (Binary, put, get, putWord8, getWord8)
import qualified Data.Map as Map
import Data.Default (def)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word16)
import qualified Network.HTTP.Req as Req
import System.FilePath ((</>))


data Name =
  Name
    { _author :: !Text
    , _project :: !Text
    }
    deriving (Eq, Ord)

data Package =
  Package
    { _name :: !Name
    , _version :: !Version
    }
    deriving (Eq, Ord)

data Version =
  Version
    { _major :: {-# UNPACK #-} !Word16
    , _minor :: {-# UNPACK #-} !Word16
    , _patch :: {-# UNPACK #-} !Word16
    }
    deriving (Eq, Ord)

data PackageRegistry =
  PackageRegistry Int (Map Name [Version])

instance Binary Name where
  get =
    liftM2 Name get get

  put (Name author project) =
    do  put author
        put project

instance Binary Package where
  get =
    liftM2 Package get get

  put (Package name version) =
    do  put name
        put version

instance Binary Version where
  get =
    do  word <- getWord8
        if word == 0
          then liftM3 Version get get get
          else
            do  minor <- fmap fromIntegral getWord8
                patch <- fmap fromIntegral getWord8
                return (Version (fromIntegral word) minor patch)

  put (Version major minor patch) =
    if major < 256 && minor < 256 && patch < 256 then
      do  putWord8 (fromIntegral major)
          putWord8 (fromIntegral minor)
          putWord8 (fromIntegral patch)
    else
      do  putWord8 0
          put major
          put minor
          put patch

instance Binary PackageRegistry where
  get = liftM2 PackageRegistry get get
  put (PackageRegistry a b) = put a >> put b

snapshot :: String -> IO ()
snapshot dir = do
  r <- Req.runReq def $
    Req.req
    Req.POST
    (Req.https "package.elm-lang.org" Req./: "all-packages")
    Req.NoReqBody
    Req.jsonResponse
    mempty
  let packages = unwrap $ case Aeson.fromJSON (Req.responseBody r) of
         Aeson.Error s -> error s
         Aeson.Success val -> val
      size = Map.foldr ((+) . length) 0 packages
      registry = PackageRegistry size packages
  Binary.encodeFile (dir </> "versions.dat") registry

newtype Packages = Packages { unwrap :: Map.Map Name [Version] }

instance Aeson.FromJSON Packages where
  parseJSON v = Packages <$> Aeson.parseJSON v

instance Aeson.FromJSON Version where
  parseJSON = Aeson.withText "string" $ \x ->
    case Text.splitOn "." x of
      [major, minor, patch] ->
        return $ Version
                  (read (Text.unpack major))
                  (read (Text.unpack minor))
                  (read (Text.unpack patch))
      _ ->
        fail "failure parsing version"


instance Aeson.FromJSON Name where
  parseJSON = Aeson.withText "string" $ \x ->
    case Text.splitOn "/" x of
      [author, package] -> return $ Name author package
      lst -> fail $ "wrong package name: " <> show lst


instance Aeson.FromJSONKey Name where
  fromJSONKey = Aeson.FromJSONKeyTextParser $ \x ->
    case Text.splitOn "/" x of
      [author, package] -> return $ Name author package
      lst -> fail $ "wrong package name: " <> show lst
