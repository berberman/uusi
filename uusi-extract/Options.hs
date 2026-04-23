{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Options
  ( Options (..),
    runOption,
    joinOptions,
  )
where

import qualified Data.Text as T
import Distribution.Pretty (pretty)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription, packageDescription)
import Distribution.Types.PackageDescription (package)
import Distribution.Types.PackageId (pkgVersion, pkgName)
import Distribution.Uusi.Utils (chain, (<|))
import System.Console.GetOpt
import Text.PrettyPrint (Mode (LeftMode), Style (..), renderStyle)

data Options = Options
  { optVersion :: Maybe (GenericPackageDescription -> T.Text),
    optName :: Maybe (GenericPackageDescription -> T.Text)
  }

style :: Style
style = Style { lineLength = 100, ribbonsPerLine = 1.0, mode = LeftMode }

defaultOptions :: Options
defaultOptions = Options Nothing Nothing

joinOptions :: Options -> [Maybe (GenericPackageDescription -> T.Text)]
joinOptions Options {..} = [optVersion] <> [optName]

cliOptions :: [OptDescr (Options -> Options)]
cliOptions =
  [ Option
      []
      ["print-version"]
      (NoArg (\opts -> opts {optVersion = Just (\desc -> T.pack (renderStyle style (pretty (pkgVersion (package (packageDescription desc))))))}))
      "print package version, e.g. --print-version",
    Option
      []
      ["print-name"]
      (NoArg (\opts -> opts {optName = Just (\desc -> T.pack (renderStyle style (pretty (pkgName (package (packageDescription desc))))))}))
      "print package name, e.g. --print-name"
  ]

runOption :: [String] -> IO (Options, [FilePath])
runOption argv = case getOpt Permute cliOptions argv of
  (o, n, []) -> return (chain o <| defaultOptions, n)
  (_, _, err) -> ioError <| userError <| concat err <> usageInfo help cliOptions
  where
    help = "uusi-extract - extract .cabal package information | usage: uusi-extract [OPTIONS] [TARGET] | find cabal file in cwd if target is not set"
