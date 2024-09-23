{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm2Nix.ElmJson
  ( Dep
  , Elm2NixError(..), toErrorMessage
  , readElmJson
  ) where

import Control.Monad (liftM2)
import Data.Aeson (Value(..))
import Data.List (nub)
import Data.Text (Text)

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as HM
#else
import qualified Data.HashMap.Strict as HM
#endif

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as Json
import qualified Data.Text as Text


type Dep = (String, String)

data Elm2NixError =
    ElmJsonReadError String
  | UnexpectedValue Value
  | KeyNotFound Text
  deriving Show

toErrorMessage :: Elm2NixError -> String
toErrorMessage err =
  case err of
    UnexpectedValue v -> "Unexpected Value: \n" ++ show v
    ElmJsonReadError s -> "Error reading json: " ++ s
    KeyNotFound key -> "Key not found in json: " ++ Text.unpack key

readElmJson :: FilePath -> IO (Either Elm2NixError [Dep])
readElmJson path = do
  res <- Json.eitherDecode <$> LBS.readFile path
  pure $
    either
      (Left . ElmJsonReadError)
      parseElmJson
      res

parseElmJson :: Value -> Either Elm2NixError [Dep]
parseElmJson obj =
  nub <$> liftM2 (++)
    (parseElmJsonDeps "dependencies" obj)
    (parseElmJsonDeps "test-dependencies" obj)

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
#if MIN_VERSION_aeson(2,0,0)
    parseDep :: Json.Key -> Value -> Either Elm2NixError Dep
    parseDep name (String ver) = Right (Text.unpack (AK.toText name), Text.unpack ver)
#else
    parseDep :: Text -> Value -> Either Elm2NixError Dep
    parseDep name (String ver) = Right (Text.unpack name, Text.unpack ver)
#endif
    parseDep _ v               = Left (UnexpectedValue v)

    parseDeps :: Value -> Either Elm2NixError [Dep]
    parseDeps (Object hm) = mapM (uncurry parseDep) (HM.toList hm)
    parseDeps v           = Left (UnexpectedValue v)

    maybeToRight :: b -> Maybe a -> Either b a
    maybeToRight _ (Just x) = Right x
    maybeToRight y Nothing  = Left y

#if MIN_VERSION_aeson(2,0,0)
    tryLookup :: HM.KeyMap Value -> Text -> Either Elm2NixError Value
    tryLookup hm key =
      maybeToRight (KeyNotFound key) (HM.lookup (AK.fromText key) hm)
#else
    tryLookup :: HM.HashMap Text Value -> Text -> Either Elm2NixError Value
    tryLookup hm key =
      maybeToRight (KeyNotFound key) (HM.lookup key hm)
#endif
