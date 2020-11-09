{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Control.Exception as CE
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import Distribution.Types.CondTree
import Distribution.Types.Dependency (Dependency (..))
import Distribution.Types.ExeDependency (ExeDependency (..))
import Distribution.Types.LegacyExeDependency (LegacyExeDependency (..))
import Distribution.Types.Lens
import Distribution.Types.VersionRange (anyVersion)
import qualified Distribution.Verbosity as Verbosity
import Lens
import System.Environment (getArgs)

-----------------------------------------------------------------------------

main :: IO ()
main = CE.catch @CE.IOException
  ( do
      args <- getArgs
      case args of
        ["--help"] -> showHelp
        ["-help"] -> showHelp
        ["help"] -> showHelp
        [path] -> uusiCabal path
        _ -> showHelp
  )
  $ \e -> T.putStrLn $ "IOException: " <> (T.pack . show $ e)

showHelp :: IO ()
showHelp = putStrLn "uusi - remove all version constraints of dependencies in a .cabal file (replace inplace)\nUsage: uusi PATH_TO_TARGET"

uusiCabal :: FilePath -> IO ()
uusiCabal originPath = do
  T.putStrLn $ "Parsing cabal file from " <> T.pack originPath <> "..."
  cabal <- readGenericPackageDescription Verbosity.normal originPath
  let uusi = showGenericPackageDescription $ uusiGenericPackageDescription cabal
  writeFile originPath uusi
  T.putStrLn $ "Write file: " <> T.pack originPath

-----------------------------------------------------------------------------

type Uusi a = a -> a

uusiDependency :: Uusi Dependency
uusiDependency (Dependency name _ lib) = Dependency name anyVersion lib

uusiLegacyExeDependency :: Uusi LegacyExeDependency
uusiLegacyExeDependency (LegacyExeDependency name _) = LegacyExeDependency name anyVersion

uusiExeDependency :: Uusi ExeDependency
uusiExeDependency (ExeDependency name component _) = ExeDependency name component anyVersion

uusiBuildInfo :: Uusi BuildInfo
uusiBuildInfo i =
  i
    |> (targetBuildDepends %~ fmap uusiDependency)
    |> (buildToolDepends %~ fmap uusiExeDependency)
    |> (buildTools %~ fmap uusiLegacyExeDependency)

uusiCondTree :: (HasBuildInfo a) => Uusi (CondTree ConfVar [Dependency] a)
uusiCondTree = mapTreeData (buildInfo %~ uusiBuildInfo) . mapTreeConstrs (fmap uusiDependency)

uusiGenericPackageDescription :: Uusi GenericPackageDescription
uusiGenericPackageDescription cabal =
  cabal
    |> (condExecutables %~ uusiTrees)
    |> (condTestSuites %~ uusiTrees)
    |> (condBenchmarks %~ uusiTrees)
    |> (condSubLibraries %~ uusiTrees)
    |> (condLibrary . mapped %~ uusiCondTree)
  where
    uusiTrees trees = trees <&> _2 %~ uusiCondTree