{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
#if MIN_VERSION_Cabal(3,14,0)
{-# LANGUAGE DataKinds #-}
#endif

module Main (main) where

import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as T
#if !MIN_VERSION_Cabal(3,8,0)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
#else
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
#endif
import Distribution.Simple.Utils (findPackageDesc)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
#if MIN_VERSION_Cabal(3,14,0)
import Distribution.Utils.Path (FileOrDir( File ), Pkg, SymbolicPath,
                                makeSymbolicPath, relativeSymbolicPath)
#endif
import Distribution.Uusi.Utils ((<|))
import qualified Distribution.Verbosity as Verbosity
import Options
import System.Environment (getArgs)

-----------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  (o, targets) <- runOption args

  target <- case targets of
#if !MIN_VERSION_Cabal(3,14,0)
    [x] -> pure x
#else
    [x] -> pure (makeSymbolicPath x)
#endif
    [] -> findCabal
    _ -> fail "Please specify at most one target to uusi-extract"

  extractAction <- case catMaybes (joinOptions o) of
    [x] -> pure x
    [] -> fail "Please specify an action to uusi-extract"
    _ -> fail "Please specify at most one action to uusi-extract"

  uusiExtractCabal extractAction target

-----------------------------------------------------------------------------

#if !MIN_VERSION_Cabal(3,14,0)
uusiExtractCabal :: (GenericPackageDescription -> T.Text) -> FilePath -> IO ()
uusiExtractCabal extractAction originPath = do
  cabal <- readGenericPackageDescription Verbosity.normal originPath
  T.putStrLn <| extractAction cabal
#else
uusiExtractCabal :: (GenericPackageDescription -> T.Text) -> SymbolicPath Pkg 'File -> IO ()
uusiExtractCabal extractAction originPath = do
  cabal <- readGenericPackageDescription Verbosity.normal Nothing originPath
  T.putStrLn <| extractAction cabal
#endif

#if !MIN_VERSION_Cabal(3,14,0)
findCabal :: IO FilePath
findCabal =
  findPackageDesc "." >>= \case
    Left err -> fail $ show err
    Right x -> pure x
#else
findCabal :: IO (SymbolicPath Pkg 'File)
findCabal =
  findPackageDesc (Just (makeSymbolicPath ".")) >>= \case
    Left err -> fail $ show err
    Right x -> pure $ relativeSymbolicPath x
#endif
