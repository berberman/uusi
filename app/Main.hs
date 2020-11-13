{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.Monad (unless, when)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Types.CondTree
import Distribution.Types.Dependency (Dependency (..))
import Distribution.Types.Lens
import Distribution.Types.PackageName
import Distribution.Types.VersionRange
import qualified Distribution.Verbosity as Verbosity
import Lens
import System.Console.GetOpt
import System.Environment (getArgs)

-----------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  (joinOptions -> actions, target) <- runOption args
  let useDefaultAction = null actions
      actions' = if useDefaultAction then [allToAnyVersion] else actions

  when useDefaultAction $
    T.putStrLn "You didn't pass any option, use -all."

  unless useDefaultAction $ do
    T.putStrLn "Pending action(s):"
    ("  " <>) . show <$> actions' |> unlines |> init |> putStrLn
  uusiCabal actions' target

runOption :: [String] -> IO (Options, FilePath)
runOption argv = case getOpt Permute cliOptions argv of
  (o, [n], []) -> return (chain o <| defaultOptions, n)
  (_, _, err) -> ioError <| userError <| concat err <> usageInfo help cliOptions
  where
    help = "uusi - tweak .cabal file (replace inplace) | useage: uusi [OPTIONS] TARGET"

-----------------------------------------------------------------------------

data Options = Options
  { optAll :: Bool,
    optOverwrite :: SomeUusi,
    optRemove :: SomeUusi,
    optReplace :: SomeUusi
  }
  deriving stock (Show)

defaultOptions :: Options
defaultOptions = Options False [] [] []

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
      "overwrite the version range of PACKAGE | e.g. -ubase: >= 4.14 && < 4.15",
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
      "replace PACKAGE with a set of packages | e.g. -rbase:text,bytestring"
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

-----------------------------------------------------------------------------

allToAnyVersion :: Uusi
allToAnyVersion = SetVersion "All dependencies" (const True) anyVersion

removeByName :: PackageName -> Uusi
removeByName name = Remove (unPackageName name |> T.pack) (== name)

overwriteByName :: PackageName -> VersionRange -> Uusi
overwriteByName name = SetVersion (unPackageName name |> T.pack) (== name)

replaceByName :: PackageName -> [(PackageName, VersionRange)] -> Uusi
replaceByName name t = Replace (unPackageName name |> T.pack) (== name) (uncurry VersionedPackage <$> t)

uusiCabal :: SomeUusi -> FilePath -> IO ()
uusiCabal actions originPath = do
  T.putStrLn <| "Parsing cabal file from " <> T.pack originPath <> "..."
  cabal <- readGenericPackageDescription Verbosity.normal originPath
  let uusi = showGenericPackageDescription <| uusiGenericPackageDescription actions cabal
  writeFile originPath uusi
  T.putStrLn <| "Write file: " <> T.pack originPath

-----------------------------------------------------------------------------

data Action tag p = Remove tag p | SetVersion tag p VersionRange | Replace tag p [VersionedPackage]

instance (Show tag) => Show (Action tag p) where
  show (Remove tag _) = "Remove[" <> show tag <> "]"
  show (SetVersion tag _ range) = "SetVersion[" <> show tag <> ", " <> prettyShow range <> "]"
  show (Replace tag _ targets) = "Replace[" <> show tag <> " |-> " <> T.unpack (T.intercalate ", " $ T.pack . show <$> targets) <> "]"

type Uusi = Action T.Text (PackageName -> Bool)

type SomeUusi = [Uusi]

type Op a = a -> a

uusiRange' :: HasVersionedPackage a => Uusi -> Op a
uusiRange' (SetVersion _ p range) x
  | p $ x ^. myPkgName = x & myVersionRange .~ range
  | otherwise = x
uusiRange' _ x = x

uusiRange :: HasVersionedPackage a => SomeUusi -> Op a
uusiRange actions = chain <| uusiRange' <$> actions

uusiReplace' :: HasVersionedPackage a => Uusi -> a -> [a]
uusiReplace' (Replace _ p targets) x
  | p $ x ^. myPkgName =
    ( \(a, b) ->
        a
          & myPkgName .~ b ^. myPkgName
          & myVersionRange .~ b ^. myVersionRange
    )
      <$> zip (repeat x) targets
  | otherwise = []
uusiReplace' _ _ = []

uusiReplace :: HasVersionedPackage a => SomeUusi -> Op [a]
uusiReplace actions t
  | k <- [r | x <- t, a <- actions, r <- uusiReplace' a x],
    not <| null k =
    k
  | otherwise = t

uusiRemove :: HasVersionedPackage a => SomeUusi -> Op [a]
uusiRemove actions t = let ps = [p | (Remove _ p) <- actions] in filter (\x -> and <| fmap (not . (<| (x ^. myPkgName))) ps) t

chain :: [Op a] -> Op a
chain = foldr (.) id

-----------------------------------------------------------------------------

uusiBuildInfo :: SomeUusi -> Op BuildInfo
uusiBuildInfo actions i =
  i
    |> (targetBuildDepends %~ fmap (uusiRange actions) . uusiReplace actions . uusiRemove actions)
    |> (buildToolDepends %~ fmap (uusiRange actions) . uusiReplace actions . uusiRemove actions)
    |> (buildTools %~ fmap (uusiRange actions) . uusiReplace actions . uusiRemove actions)

uusiCondTree :: (HasBuildInfo a) => SomeUusi -> Op (CondTree ConfVar [Dependency] a)
uusiCondTree actions = mapTreeData (buildInfo %~ uusiBuildInfo actions) . mapTreeConstrs (fmap (uusiRange actions) . uusiReplace actions . uusiRemove actions)

uusiGenericPackageDescription :: SomeUusi -> Op GenericPackageDescription
uusiGenericPackageDescription actions cabal =
  cabal
    |> (condExecutables %~ uusiTrees)
    |> (condTestSuites %~ uusiTrees)
    |> (condBenchmarks %~ uusiTrees)
    |> (condSubLibraries %~ uusiTrees)
    |> (condLibrary . mapped %~ uusiCondTree actions)
  where
    uusiTrees trees = trees <&> _2 %~ uusiCondTree actions