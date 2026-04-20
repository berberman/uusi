{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
#if MIN_VERSION_Cabal(3,14,0)
{-# LANGUAGE DataKinds #-}
#endif

module Main (main) where

import Control.Monad (unless, when)
import qualified Data.Text as T
import qualified Data.Text.IO as T
#if !MIN_VERSION_Cabal(3,8,0)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
#else
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
#endif
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import Distribution.Simple.Utils (findPackageDesc)
#if MIN_VERSION_Cabal(3,14,0)
import Distribution.Utils.Path (FileOrDir( File ), Pkg, SymbolicPath,
                                getSymbolicPath, interpretSymbolicPathCWD, makeSymbolicPath, relativeSymbolicPath)
#endif
import Distribution.Uusi.Core
import Distribution.Uusi.Types
import Distribution.Uusi.Utils
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
    _ -> fail "Please specify at most one target to uusi"

  let actions = joinOptions o
      useDefaultAction = null actions
      actions' = if useDefaultAction then [allToAnyVersion] else actions

  when useDefaultAction $
    T.putStrLn "You didn't pass any option, use --all."

  unless useDefaultAction $ do
    T.putStrLn "Pending action(s):"
    ("  " <>) . show <$> actions' |> unlines |> init |> putStrLn
  uusiCabal actions' target

-----------------------------------------------------------------------------

#if !MIN_VERSION_Cabal(3,14,0)
uusiCabal :: Uusis -> FilePath -> IO ()
uusiCabal actions originPath = do
  T.putStrLn <| "Parsing cabal file from " <> T.pack originPath <> "..."
  cabal <- readGenericPackageDescription Verbosity.normal originPath
  let uusi = showGenericPackageDescription <| uusiGenericPackageDescription actions cabal
  writeFile originPath uusi
  T.putStrLn <| "Write file: " <> T.pack originPath
#else
uusiCabal :: Uusis -> SymbolicPath Pkg 'File -> IO ()
uusiCabal actions originPath = do
  T.putStrLn <| "Parsing cabal file from " <> T.pack (getSymbolicPath originPath) <> "..."
  cabal <- readGenericPackageDescription Verbosity.normal Nothing originPath
  let uusi = showGenericPackageDescription <| uusiGenericPackageDescription actions cabal
  writeFile (interpretSymbolicPathCWD originPath) uusi
  T.putStrLn <| "Write file: " <> T.pack (getSymbolicPath originPath)
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
