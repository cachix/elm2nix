{-# LANGUAGE QuasiQuotes #-}

module Lib
    ( convert
    , init'
    ) where

import Control.Monad (mapM)
import Control.Monad.Except (liftIO, throwError)
import Data.List (intercalate)
import qualified Data.Text as Text
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (exitFailure)
import System.IO ( hPutStrLn, stdout, stderr)
import qualified Data.Map as Map
import Data.String.Here

import qualified Install.Solver as Solver
import qualified Install.Plan as Plan
import qualified Reporting.Error as Error
import qualified Manager
import qualified Install
import qualified Elm.Package as Package
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Paths as Path
import qualified Elm.Package.Solution as Solution

import Prefetch

-- CMDs

convert :: IO ()
convert = runCLI solveDependencies


init' :: IO ()
init' = runCLI generateDefault

-- Utils

runCLI f = do
  result <- Manager.run f
  case result of
        Right () ->
          return ()

        Left err -> do
          Error.toStderr err
          exitFailure

generateDefault :: Manager.Manager ()
generateDefault = do
  desc <- readDescription
  let name = toNixName (Desc.name desc) ++ "-" ++ show (Desc.version desc)
  let srcdir = case Desc.sourceDirs desc of
                    []    -> "."
                    (a:_) -> a
  liftIO $ putStrLn [template|data/default.nix|]

solveDependencies :: Manager.Manager ()
solveDependencies = do
  liftIO $ hPutStrLn stderr "Resolving elm-package.json dependencies into elm-stuff/exact-dependencies.json ..."

  desc <- readDescription
  newSolution <- Solver.solve (Desc.elmVersion desc) (Desc.dependencies desc)
  liftIO (createDirectoryIfMissing True Path.stuffDirectory)
  liftIO (Solution.write Path.solvedDependencies newSolution)

  liftIO $ hPutStrLn stderr "Prefetching tarballs and computing sha256 hashes ..."

  let solL = Map.toList newSolution
  sources <- liftIO $ mapM Prefetch.prefetchURL solL

  liftIO $ putStrLn $ generateNixSources sources

readDescription :: Manager.Manager Desc.Description
readDescription = do
  exists <- liftIO (doesFileExist Path.description)

  if exists
  then Desc.read Error.CorruptDescription Path.description
  else Install.initialDescription

generateNixSource :: DerivationSource -> String
generateNixSource ds =
  -- TODO: pass name to fetchzip
  [i|  "${Package.toUrl (drvName ds)}" = {
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
toNixName :: Package.Name -> String
toNixName (Package.Name user project) =
  Text.unpack user ++ "-" ++ Text.unpack project
