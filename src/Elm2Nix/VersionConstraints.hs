{- Parses packages version

  This code is aproximation of but using builtin primitives
  - https://github.com/elm/compiler/blob/master/compiler/src/Elm/Version.hs
  - https://github.com/elm/compiler/blob/master/compiler/src/Elm/Constraint.hs
-}

module Elm2Nix.VersionConstraints
  ( Version
  , Constraint
  , fromString
  , lowest
  , versionToString
  ) where

import qualified Data.List.Split as Split


-- Version


data Version =
  Version
    { _major :: String
    , _minor :: String
    , _patch :: String
    }
    deriving (Eq, Ord)

versionFromString :: String -> (String -> Error) -> Either Error Version
versionFromString string toError =
  case Split.splitOn " " string of
    [major, minor, patch] ->
      Right (Version major minor patch)

    _ ->
      Left (toError string)


versionToString :: Version -> String
versionToString (Version major minor patch) =
  major <> "." <> minor <> "." <> patch


-- Constaraints


data Constraint
    = Range Version Op Op Version
    deriving (Eq)


data Op
  = Less
  | LessOrEqual
  deriving (Eq)


data Error
  = BadLower String
  | BadUpper String
  | BadFormat
  | InvalidOp
  | InvalidRange Version Version


fromString :: String -> Either Error Constraint
fromString string =
  case Split.splitOn " " string of
    [lower, lowerOp, "v", upperOp, upper] ->
      do  lo <- versionFromString lower BadLower
          lop <- opFromString lowerOp
          hop <- opFromString upperOp
          hi <- versionFromString upper BadUpper
          if lo < hi
            then Right (Range lo lop hop hi)
            else Left (InvalidRange lo hi)

    _ ->
      Left BadFormat


opFromString :: String -> Either Error Op
opFromString text =
  case text of
    "<=" -> Right LessOrEqual
    "<"  -> Right Less
    _    -> Left InvalidOp


lowest :: Constraint -> Version
lowest (Range lower _ _ _) =
  lower
