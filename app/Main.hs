{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Main (main) where

import Control.Monad (unless, when)
import qualified Data.Text as T
import qualified Data.Text.IO as T
#if !MIN_VERSION_Cabal(3,6,0)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
#else
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
#endif
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import Distribution.Simple.Utils (findPackageDesc)
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
    [x] -> pure x
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

uusiCabal :: Uusis -> FilePath -> IO ()
uusiCabal actions originPath = do
  T.putStrLn <| "Parsing cabal file from " <> T.pack originPath <> "..."
  cabal <- readGenericPackageDescription Verbosity.normal originPath
  let uusi = showGenericPackageDescription <| uusiGenericPackageDescription actions cabal
  writeFile originPath uusi
  T.putStrLn <| "Write file: " <> T.pack originPath

findCabal :: IO FilePath
findCabal =
  findPackageDesc "." >>= \case
    Left err -> fail err
    Right x -> pure x
