{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import Distribution.Types.CondTree
import Distribution.Types.Dependency (Dependency (..))
import Distribution.Types.ExeDependency (ExeDependency (..))
import Distribution.Types.LegacyExeDependency (LegacyExeDependency (..))
import Distribution.Types.Lens
import Distribution.Types.PackageName
import Distribution.Types.VersionRange
import qualified Distribution.Verbosity as Verbosity
import Lens
import System.Environment (getArgs)

-----------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> showHelp
    ["-help"] -> showHelp
    ["help"] -> showHelp
    [path] -> uusiCabal path
    _ -> showHelp

showHelp :: IO ()
showHelp = putStrLn "uusi - remove all version constraints of dependencies in a .cabal file (replace inplace)\nUsage: uusi PATH_TO_TARGET"

allToAnyVersion :: Uusi
allToAnyVersion = SetVersion "Remove all version constraints" (const True) anyVersion

uusiCabal :: FilePath -> IO ()
uusiCabal originPath = do
  T.putStrLn $ "Parsing cabal file from " <> T.pack originPath <> "..."
  cabal <- readGenericPackageDescription Verbosity.normal originPath
  let uusi = showGenericPackageDescription $ uusiGenericPackageDescription [allToAnyVersion] cabal
  writeFile originPath uusi
  T.putStrLn $ "Write file: " <> T.pack originPath

-----------------------------------------------------------------------------

data Action tag p = Remove tag p | SetVersion tag p VersionRange deriving stock (Show)

type Uusi = Action T.Text (PackageName -> Bool)

type SomeUusi = [Uusi]

type Op a = a -> a

class HasNameAndRange a where
  getName :: a -> PackageName
  setRange :: VersionRange -> Op a
  apply :: (PackageName -> Bool) -> VersionRange -> Op a
  apply p range x
    | p $ getName x = setRange range x
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
uusiRemove actions t = let ps = [p | a <- [actions], (Remove _ p) <- a] in filter (\x -> and $ fmap (not . ($ getName x)) ps) t

uusiRange :: HasNameAndRange a => SomeUusi -> Op a
uusiRange actions = chain $ uusiRange' <$> actions

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