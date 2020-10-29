{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Colourista as C
import qualified Control.Exception as CE
import qualified Data.Text as T
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import Distribution.Types.CondTree
import Distribution.Types.Dependency (Dependency (..))
import Distribution.Types.ExeDependency (ExeDependency (..))
import Distribution.Types.LegacyExeDependency (LegacyExeDependency (..))
import Distribution.Types.Lens
import Distribution.Types.VersionRange (anyVersion)
import qualified Distribution.Verbosity as Verbosity
import Lens.Micro
import Options.Applicative
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, hFlush, hPutStr, openTempFile)
import System.Process (readCreateProcessWithExitCode, shell)

newtype Options = Options
  { optPath :: FilePath
  }

cmdOptions :: Parser Options
cmdOptions = Options <$> strArgument (metavar "PATH" <> help "Path to .cabal file")

runArgsParser :: IO Options
runArgsParser =
  execParser $
    info
      (cmdOptions <**> helper)
      ( fullDesc
          <> progDesc "Try to reach the TARGET QAQ."
          <> header "uusi - a program removing all version constraints of dependencies in .cabal file"
      )

-----------------------------------------------------------------------------

main :: IO ()
main = CE.catch @CE.IOException
  ( do
      Options {..} <- runArgsParser
      C.infoMessage "Start running..."
      uusiCabal optPath >>= putStrLn
  )
  $ \e -> C.errorMessage $ "IOException: " <> (T.pack . show $ e)

genPatch :: FilePath -> FilePath -> IO String
genPatch a b = (^. _2) <$> readCreateProcessWithExitCode (shell $ "diff -u " <> a <> " " <> b) ""

uusiCabal :: FilePath -> IO String
uusiCabal originPath = do
  C.infoMessage $ "Parsing cabal file from " <> T.pack originPath <> "..."

  cabal <- readGenericPackageDescription Verbosity.normal originPath
  temp <- getTemporaryDirectory
  (oldPath, oldHandle) <- openTempFile temp "uusi"

  let old = showGenericPackageDescription cabal
      uusied = showGenericPackageDescription $ uusiGenericPackageDescription cabal

  hPutStr oldHandle old
  writeFile originPath uusied

  C.infoMessage $ "Write file: " <> T.pack originPath

  hFlush oldHandle
  hClose oldHandle

  result <- genPatch oldPath originPath
  removeFile oldPath

  return result

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
    & (targetBuildDepends %~ fmap uusiDependency)
    & (buildToolDepends %~ fmap uusiExeDependency)
    & (buildTools %~ fmap uusiLegacyExeDependency)

uusiCondTree :: (HasBuildInfo a) => Uusi (CondTree ConfVar [Dependency] a)
uusiCondTree = mapTreeData (buildInfo %~ uusiBuildInfo) . mapTreeConstrs (fmap uusiDependency)

uusiGenericPackageDescription :: Uusi GenericPackageDescription
uusiGenericPackageDescription cabal =
  cabal
    & (condExecutables %~ uusiTrees)
    & (condTestSuites %~ uusiTrees)
    & (condBenchmarks %~ uusiTrees)
    & (condSubLibraries %~ uusiTrees)
    & (condLibrary . mapped %~ uusiCondTree)
  where
    uusiTrees trees = trees <&> _2 %~ uusiCondTree