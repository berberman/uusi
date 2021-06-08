{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Copyright: (c) 2020-2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
-- This module provides core functionality of @uusi@.
-- It exports the core function 'uusiGenericPackageDescription', and some functions to create 'Uusi'.
module Distribution.Uusi.Core
  ( uusiGenericPackageDescription,
    allToAnyVersion,
    removeByName,
    overwriteByName,
    replaceByName,
    buildableByName,
    optionsByName,
    addOptionsForAll,
    removeOptionsForAll,
  )
where

import Data.List ((\\))
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Distribution.Compiler (PerCompilerFlavor (..))
import Distribution.Types.CondTree (CondTree, mapTreeConstrs, mapTreeData)
import Distribution.Types.Dependency (Dependency)
import Distribution.Types.Lens
import Distribution.Types.PackageName (PackageName, unPackageName)
import Distribution.Types.UnqualComponentName
import Distribution.Types.VersionRange (VersionRange, anyVersion)
import Distribution.Uusi.Types
import Distribution.Uusi.Utils
import Distribution.Uusi.Lens

-----------------------------------------------------------------------------

-- | Create 'Action' that removes all version constraints
allToAnyVersion :: Uusi
allToAnyVersion = SetVersion "All dependencies" (const True) anyVersion

-- | Create 'Action' that removes a dependency by given its name
removeByName :: PackageName -> Uusi
removeByName name = Remove (unPackageName name |> T.pack) (== name)

-- | Create 'Action' that overwrites a dependency's version range
overwriteByName :: PackageName -> VersionRange -> Uusi
overwriteByName name = SetVersion (unPackageName name |> T.pack) (== name)

-- | Create 'Action' that replaces a dependency with a set of packages
replaceByName :: PackageName -> [(PackageName, VersionRange)] -> Uusi
replaceByName name t = Replace (unPackageName name |> T.pack) (== name) (uncurry VersionedPackage <$> t)

-- | Create 'Action' that sets the buildable of a component (not library)
buildableByName :: UnqualComponentName -> Bool -> Uusi
buildableByName name = SetBuildable (unUnqualComponentName name |> T.pack) (== name)

-- | Create 'Action' that modifies ghc-options of a component (or the library, if @name == "library"@)
optionsByName :: UnqualComponentName -> Op [String] -> Uusi
optionsByName name = ModifyBuiltOptions (unUnqualComponentName name |> T.pack) (== name)

-- | Create 'Action' that appends @opts@ to ghc-options of all components and the library
addOptionsForAll :: [String] -> Uusi
addOptionsForAll opts =
  ModifyBuiltOptions
    ("Add " <> T.pack (show opts) <> " to all components and the library")
    (const True)
    (<> opts)

-- | Create 'Action' that removes @opts@ from ghc-options of all components and the library
removeOptionsForAll :: [String] -> Uusi
removeOptionsForAll opts =
  ModifyBuiltOptions
    ("Remove " <> T.pack (show opts) <> " from all components and the library")
    (const True)
    (filter (`notElem` opts))

-----------------------------------------------------------------------------

uusiRange' :: HasVersionedPackage a => Uusi -> Op a
uusiRange' (SetVersion _ p range) x
  | p $ x ^. myPkgName = x & myVersionRange .~ range
  | otherwise = x
uusiRange' _ x = x

uusiRange :: HasVersionedPackage a => Uusis -> Op a
uusiRange actions = chain <| uusiRange' <$> actions

uusiReplace' :: HasVersionedPackage a => Uusi -> a -> [a]
uusiReplace' (Replace _ p targets) x
  | p $ x ^. myPkgName =
    ( \t ->
        x
          & myPkgName .~ t ^. myPkgName
          & myVersionRange .~ t ^. myVersionRange
    )
      <$> targets
  | otherwise = []
uusiReplace' _ _ = []

uusiReplace :: (HasVersionedPackage a, Eq a) => Uusis -> Op [a]
uusiReplace actions t =
  let k = [(r', if success then Just x else Nothing) | x <- t, a <- actions, let r = uusiReplace' a x, let success = not $ null r, r' <- r]
      kf = fst <$> k
      ks = catMaybes $ snd <$> k
   in -- TODO: this is ugly
      kf <> (t \\ ks)

uusiRemove :: HasVersionedPackage a => Uusis -> Op [a]
uusiRemove actions t = let ps = [p | (Remove _ p) <- actions] in filter (\x -> and <| fmap (not . (<| (x ^. myPkgName))) ps) t

uusiBuildable :: HasBuildInfo a => Uusis -> Op (UnqualComponentName, CondTree ConfVar [Dependency] a)
uusiBuildable actions t
  | (name, tree) <- t,
    (b : _) <- [b | (SetBuildable _ p b) <- actions, p name] =
    (name, mapTreeData (buildInfo %~ buildable .~ b) tree)
  | otherwise = t

uusiOptions' :: HasBuildInfo a => Action tag -> Op (UnqualComponentName, CondTree ConfVar [Dependency] a)
uusiOptions' (ModifyBuiltOptions _ p f) x
  | (name, tree) <- x,
    p name =
    (name, mapTreeData (buildInfo %~ options %~ \(PerCompilerFlavor ghc ghcjs) -> PerCompilerFlavor (f ghc) ghcjs) tree)
uusiOptions' _ x = x

uusiOptions :: HasBuildInfo a => [Action tag] -> Op (UnqualComponentName, CondTree ConfVar [Dependency] a)
uusiOptions actions = chain <| uusiOptions' <$> actions

-----------------------------------------------------------------------------

uusiBuildInfo :: Uusis -> Op BuildInfo
uusiBuildInfo actions i =
  i
    |> (targetBuildDepends %~ fmap (uusiRange actions) . uusiReplace actions . uusiRemove actions)
    |> (buildToolDepends %~ fmap (uusiRange actions) . uusiReplace actions . uusiRemove actions)
    |> (buildTools %~ fmap (uusiRange actions) . uusiReplace actions . uusiRemove actions)

uusiCondTree :: (HasBuildInfo a) => Uusis -> Op (CondTree ConfVar [Dependency] a)
uusiCondTree actions = mapTreeData (buildInfo %~ uusiBuildInfo actions) . mapTreeConstrs (fmap (uusiRange actions) . uusiReplace actions . uusiRemove actions)

-- | The core function of @uusi@.
uusiGenericPackageDescription ::
  -- | A list of 'Action' to apply
  Uusis ->
  Op GenericPackageDescription
uusiGenericPackageDescription actions cabal =
  cabal
    |> (condExecutables %~ uusiTrees)
    |> (condTestSuites %~ uusiTrees)
    |> (condBenchmarks %~ uusiTrees)
    |> (condSubLibraries %~ uusiTrees)
    |> (condLibrary . mapped %~ uusiCondTree actions . (snd . uusiOptions actions . ("library",)))
  where
    uusiTrees trees = trees <&> _2 %~ uusiCondTree actions <&> uusiBuildable actions <&> uusiOptions actions
