{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.Monad (unless, when)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Types.CondTree
import Distribution.Types.Dependency (Dependency (..))
import Distribution.Types.ExeDependency (ExeDependency (..))
import Distribution.Types.LegacyExeDependency (LegacyExeDependency (..))
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
  (AOption actions, target) <- runOption args
  let useDefaultAction = null actions
      actions' = if useDefaultAction then [allToAnyVersion] else actions

  when useDefaultAction $
    T.putStrLn "You didn't pass -a, remove all version constraints of dependencies."

  unless useDefaultAction $ do
    T.putStrLn "Pending action(s):"
    show <$> actions' |> unlines |> putStrLn
  uusiCabal actions' target

runOption :: [String] -> IO (AOption, FilePath)
runOption argv = case getOpt Permute [option] argv of
  (o, [n], []) -> return (chain o <| defaultOption, n)
  (_, _, err) -> ioError <| userError <| concat err <> usageInfo help [option]
  where
    help = "uusi - tweak .cabal file (replace inplace)"

newtype AOption = AOption
  { optActions :: SomeUusi
  }
  deriving stock (Show)

defaultOption :: AOption
defaultOption = AOption []

option :: OptDescr (AOption -> AOption)
option =
  Option
    ['a']
    ["action"]
    ( OptArg
        ( \arg opt -> case arg >>= parseAction of
            Just action -> opt {optActions = action : optActions opt}
            _ -> opt
        )
        "ACTION"
    )
    <| unlines
      [ "There are three kinds of actions:",
        "PACKAGE:remove        -- remove PACKAGE from dependencies              | e.g. base:remove",
        "PACKAGE:VERSION_RANGE -- overwrite the version range of PACKAGE        | e.g. base: >= 4.14 && < 4.15 ",
        "...                                                                    | e.g. text:-any",
        "all                   -- remove all version constraints of dependencies",
        "",
        "They can be used together: uusi -aACTION1 -aACTION2 ... TARGET",
        "default: all"
      ]

parseAction :: String -> Maybe Uusi
parseAction s = case T.splitOn ":" (T.pack s) of
  [T.unpack . T.strip -> name, t] -> case t of
    "remove" -> name |> mkPackageName |> removeByName |> Just
    _ -> case t |> T.unpack |> simpleParsec of
      Just ver -> overwriteByName (name |> mkPackageName) ver |> Just
      _ -> Nothing
  ["all"] -> Just allToAnyVersion
  _ -> Nothing

allToAnyVersion :: Uusi
allToAnyVersion = SetVersion "Remove all version constraints" (const True) anyVersion

removeByName :: PackageName -> Uusi
removeByName name = Remove (unPackageName name |> T.pack) (== name)

overwriteByName :: PackageName -> VersionRange -> Uusi
overwriteByName name = SetVersion (unPackageName name |> T.pack) (== name)

uusiCabal :: SomeUusi -> FilePath -> IO ()
uusiCabal actions originPath = do
  T.putStrLn <| "Parsing cabal file from " <> T.pack originPath <> "..."
  cabal <- readGenericPackageDescription Verbosity.normal originPath
  let uusi = showGenericPackageDescription <| uusiGenericPackageDescription actions cabal
  writeFile originPath uusi
  T.putStrLn <| "Write file: " <> T.pack originPath

-----------------------------------------------------------------------------

data Action tag p = Remove tag p | SetVersion tag p VersionRange

instance (Show tag) => Show (Action tag p) where
  show (Remove tag _) = "Remove[" <> show tag <> "]"
  show (SetVersion tag _ range) = "SetVersion[" <> show tag <> ", " <> prettyShow range <> "]"

type Uusi = Action T.Text (PackageName -> Bool)

type SomeUusi = [Uusi]

type Op a = a -> a

class HasNameAndRange a where
  getName :: a -> PackageName
  setRange :: VersionRange -> Op a
  apply :: (PackageName -> Bool) -> VersionRange -> Op a
  apply p range x
    | p <| getName x = setRange range x
  apply _ _ x = x

instance HasNameAndRange Dependency where
  getName (Dependency name _ _) = name
  setRange range (Dependency name _ lib) = Dependency name range lib

instance HasNameAndRange ExeDependency where
  getName (ExeDependency name _ _) = name
  setRange range (ExeDependency name component _) = ExeDependency name component range

instance HasNameAndRange LegacyExeDependency where
  getName (LegacyExeDependency name _) = mkPackageName name
  setRange range (LegacyExeDependency name _) = LegacyExeDependency name range

uusiRange' :: HasNameAndRange a => Uusi -> Op a
uusiRange' (SetVersion _ p range) = apply p range
uusiRange' _ = id

uusiRemove :: HasNameAndRange a => SomeUusi -> Op [a]
uusiRemove actions t = let ps = [p | a <- [actions], (Remove _ p) <- a] in filter (\x -> and <| fmap (not . (<| getName x)) ps) t

uusiRange :: HasNameAndRange a => SomeUusi -> Op a
uusiRange actions = chain <| uusiRange' <$> actions

chain :: [Op a] -> Op a
chain = foldr (.) id

-----------------------------------------------------------------------------

uusiBuildInfo :: SomeUusi -> Op BuildInfo
uusiBuildInfo actions i =
  i
    |> (targetBuildDepends %~ fmap (uusiRange actions) . uusiRemove actions)
    |> (buildToolDepends %~ fmap (uusiRange actions) . uusiRemove actions)
    |> (buildTools %~ fmap (uusiRange actions) . uusiRemove actions)

uusiCondTree :: (HasBuildInfo a) => [Uusi] -> Op (CondTree ConfVar [Dependency] a)
uusiCondTree actions = mapTreeData (buildInfo %~ uusiBuildInfo actions) . mapTreeConstrs (fmap (uusiRange actions) . uusiRemove actions)

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