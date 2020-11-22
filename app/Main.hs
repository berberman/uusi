{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad (unless, when)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import Distribution.Uusi.Core
import Distribution.Uusi.Types
import Distribution.Uusi.Utils
import qualified Distribution.Verbosity as Verbosity
import Options
import System.Environment (getArgs)
import System.FilePath (takeDirectory, (</>))

-----------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  (o@Options {..}, target) <- runOption args
  let actions = joinOptions o
      useDefaultAction = null actions
      actions' = if useDefaultAction then [allToAnyVersion] else actions

  when optGenSetup $ do
    let path = takeDirectory target </> "Setup.hs"
    writeFile path $
      unlines
        [ "import Distribution.Simple",
          "main = defaultMain"
        ]
    T.putStrLn $ "Write file: " <> T.pack path

  when useDefaultAction $
    T.putStrLn "You didn't pass any option, use --all."

  unless useDefaultAction $ do
    T.putStrLn "Pending action(s):"
    ("  " <>) . show <$> actions' |> unlines |> init |> putStrLn
  uusiCabal actions' target

-----------------------------------------------------------------------------

uusiCabal :: SomeUusi -> FilePath -> IO ()
uusiCabal actions originPath = do
  T.putStrLn <| "Parsing cabal file from " <> T.pack originPath <> "..."
  cabal <- readGenericPackageDescription Verbosity.normal originPath
  let uusi = showGenericPackageDescription <| uusiGenericPackageDescription actions cabal
  writeFile originPath uusi
  T.putStrLn <| "Write file: " <> T.pack originPath
