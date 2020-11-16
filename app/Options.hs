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

import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Distribution.Parsec (simpleParsec)
import Distribution.Types.PackageName
import Distribution.Types.VersionRange
import Distribution.Uusi.Core
import Distribution.Uusi.Types
import Distribution.Uusi.Utils
import System.Console.GetOpt

data Options = Options
  { optAll :: Bool,
    optOverwrite :: SomeUusi,
    optRemove :: SomeUusi,
    optReplace :: SomeUusi,
    optGenSetup :: Bool
  }
  deriving stock (Show)

defaultOptions :: Options
defaultOptions = Options False [] [] [] False

joinOptions :: Options -> SomeUusi
joinOptions Options {..} = [allToAnyVersion | optAll] <> optOverwrite <> optRemove <> optReplace

cliOptions :: [OptDescr (Options -> Options)]
cliOptions =
  [ Option
      []
      ["all"]
      (NoArg (\opts -> opts {optAll = True}))
      "remove all version constraints | e.g. -all",
    Option
      ['u']
      ["update"]
      ( ReqArg
          ( \arg opts -> case parsePkg arg of
              Just action -> opts {optOverwrite = (action |> uncurry overwriteByName) : optOverwrite opts}
              _ -> error $ "faild to parse overwrite: " <> arg
          )
          "PACKAGE(:VERSION_RANGE)"
      )
      "overwrite the version range of PACKAGE (empty version range means `-any`) | e.g. -ubase: >= 4.14 && < 4.15",
    Option
      ['d']
      ["delete"]
      ( ReqArg
          ( \arg opts -> case simpleParsec arg of
              Just name -> opts {optRemove = removeByName name : optRemove opts}
              _ -> error $ "faild to parse remove: " <> arg
          )
          "PACKAGE"
      )
      "remove PACKAGE | e.g. -dbase",
    Option
      ['r']
      ["replace"]
      ( ReqArg
          ( \arg opts -> case parseReplace arg of
              Just action -> opts {optReplace = action : optReplace opts}
              _ -> error $ "faild to parse replace: " <> arg
          )
          "SOURCE_PACKAGE:DEST1_PACKAGE(:DEST1_VERSION),DEST2_PACKAGE(:DEST2_VERSION)..."
      )
      "replace PACKAGE with a set of packages (empty version range means `-any`) | e.g. -rbase:text,bytestring",
    Option
      []
      ["gen-setup"]
      (NoArg (\opts -> opts {optGenSetup = True}))
      "generate Setup.hs"
  ]

parsePkg :: String -> Maybe (PackageName, VersionRange)
parsePkg s = case T.splitOn ":" (T.pack s) of
  [name, v] -> case v |> T.unpack |> simpleParsec of
    Just ver -> (,ver) <$> (name |> T.unpack |> simpleParsec)
    _ -> Nothing
  [name] -> (,anyVersion) <$> (name |> T.unpack |> simpleParsec)
  _ -> Nothing

parseReplace :: String -> Maybe Uusi
parseReplace (T.pack -> s)
  | (Just source) <- s |> T.takeWhile (/= ':') |> T.unpack |> simpleParsec,
    k <- s |> T.dropWhile (/= ':'),
    not <| T.null k,
    rest <- k |> T.tail |> T.splitOn ",",
    action <- [r | x <- rest, r <- catMaybes [x |> T.unpack |> parsePkg]],
    not <| null action =
    action |> replaceByName source |> Just
  | otherwise = Nothing

runOption :: [String] -> IO (Options, FilePath)
runOption argv = case getOpt Permute cliOptions argv of
  (o, [n], []) -> return (chain o <| defaultOptions, n)
  (_, _, err) -> ioError <| userError <| concat err <> usageInfo help cliOptions
  where
    help = "uusi - tweak .cabal file (replace inplace) | useage: uusi [OPTIONS] TARGET"