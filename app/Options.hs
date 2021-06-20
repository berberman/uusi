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
    optOverwrite :: Uusis,
    optRemove :: Uusis,
    optReplace :: Uusis,
    optBuild :: Uusis,
    optNoBuild :: Uusis,
    optOptions :: Uusis
  }
  deriving stock (Show)

defaultOptions :: Options
defaultOptions = Options False [] [] [] [] [] []

joinOptions :: Options -> Uusis
joinOptions Options {..} = [allToAnyVersion | optAll] <> optOverwrite <> optRemove <> optReplace <> optBuild <> optNoBuild <> optOptions

cliOptions :: [OptDescr (Options -> Options)]
cliOptions =
  [ Option
      []
      ["all"]
      (NoArg (\opts -> opts {optAll = True}))
      "remove all version constraints | e.g. --all",
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
      "overwrite the version range of PACKAGE (empty version range means any version) | e.g. -ubase: >= 4.14 && < 4.15",
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
      "replace PACKAGE with a set of packages (empty version range means anyversion) | e.g. -rbase:text,bytestring",
    Option
      ['b']
      ["buildable"]
      ( ReqArg
          ( \arg opts -> case parseBuildable arg True of
              Just action -> opts {optBuild = action : optBuild opts}
              _ -> error $ "failed to parse build: " <> arg
          )
          "COMPONENT"
      )
      "set the buildable of a component to true | e.g. -b foo-test",
    Option
      []
      ["no-buildable", "nb"]
      ( ReqArg
          ( \arg opts -> case parseBuildable arg False of
              Just action -> opts {optNoBuild = action : optNoBuild opts}
              _ -> error $ "failed to parse no build:" <> arg
          )
          "COMPONENT"
      )
      "set the buildable of a component to false | e.g. --nb foo-test",
    Option
      []
      ["add-options", "add-opt"]
      ( ReqArg
          ( \arg opts -> case parseComponentOpt arg of
              Just action -> opts {optNoBuild = action : optNoBuild opts}
              _ -> error $ "failed to parse compoennt opt: " <> arg
          )
          "COMPONENT:OPTION_1,OPTION_2,..."
      )
      "append ghc-options to a component | e.g. --add-options foo-test:-Wall,--Wpartial-fields",
    Option
      []
      ["add-options-all", "add-opt-all"]
      ( ReqArg
          ( \arg opts -> case parseOpt arg of
              Just x -> opts {optOptions = addOptionsForAll x : optOptions opts}
              _ -> error $ "failed to parse all opt: " <> arg
          )
          "OPTION_1,OPTION_2,..."
      )
      "append ghc-options to all components | e.g. --add-options-all -Wall,--Wpartial-fields",
    Option
      []
      ["remove-options-all", "remove-opt-all"]
      ( ReqArg
          ( \arg opts -> case parseOpt arg of
              Just x -> opts {optOptions = removeOptionsForAll x : optOptions opts}
              _ -> error $ "failed to parse all opt: " <> arg
          )
          "OPTION_1,OPTION_2,..."
      )
      "remove ghc-options from all components | e.g. --remove-options-all -Wall,--Wpartial-fields"
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

parseBuildable :: String -> Bool -> Maybe Uusi
parseBuildable s b = flip buildableByName b <$> simpleParsec s

parseOpt :: String -> Maybe [String]
parseOpt (T.pack -> s) =
  let s' = T.splitOn "," s
   in if all ((== '-') . T.head) s' then Just (fmap (T.unpack . T.strip) s') else Nothing

parseComponentOpt :: String -> Maybe Uusi
parseComponentOpt (T.pack -> s)
  | (Just name) <- s |> T.takeWhile (/= ':') |> T.unpack |> simpleParsec,
    k <- s |> T.dropWhile (/= ':') |> T.tail,
    not <| T.null k,
    Just opts <- parseOpt $ T.unpack k =
    Just $ optionsByName name (<> opts)
  | otherwise = Nothing

runOption :: [String] -> IO (Options, FilePath)
runOption argv = case getOpt Permute cliOptions argv of
  (o, [n], []) -> return (chain o <| defaultOptions, n)
  (_, _, err) -> ioError <| userError <| concat err <> usageInfo help cliOptions
  where
    help = "uusi - tweak .cabal file (replace inplace) | usage: uusi [OPTIONS] TARGET"
