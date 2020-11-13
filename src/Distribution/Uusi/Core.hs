{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Uusi.Core
  ( uusiGenericPackageDescription,
    allToAnyVersion,
    removeByName,
    overwriteByName,
    replaceByName,
  )
where

import Data.List ((\\))
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Distribution.Types.CondTree (CondTree, mapTreeConstrs, mapTreeData)
import Distribution.Types.Dependency (Dependency)
import Distribution.Types.Lens
import Distribution.Types.PackageName (PackageName, unPackageName)
import Distribution.Types.VersionRange (VersionRange, anyVersion)
import Distribution.Uusi.Types
import Distribution.Uusi.Utils
import Lens.Micro

-----------------------------------------------------------------------------

allToAnyVersion :: Uusi
allToAnyVersion = SetVersion "All dependencies" (const True) anyVersion

removeByName :: PackageName -> Uusi
removeByName name = Remove (unPackageName name |> T.pack) (== name)

overwriteByName :: PackageName -> VersionRange -> Uusi
overwriteByName name = SetVersion (unPackageName name |> T.pack) (== name)

replaceByName :: PackageName -> [(PackageName, VersionRange)] -> Uusi
replaceByName name t = Replace (unPackageName name |> T.pack) (== name) (uncurry VersionedPackage <$> t)

-----------------------------------------------------------------------------

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
    ( \t ->
        x
          & myPkgName .~ t ^. myPkgName
          & myVersionRange .~ t ^. myVersionRange
    )
      <$> targets
  | otherwise = []
uusiReplace' _ _ = []

uusiReplace :: (HasVersionedPackage a, Eq a) => SomeUusi -> Op [a]
uusiReplace actions t =
  let k = [(r', if success then Just x else Nothing) | x <- t, a <- actions, let r = uusiReplace' a x, let success = not $ null r, r' <- r]
      kf = fst <$> k
      ks = catMaybes $ snd <$> k
   in -- TODO: this is ugly
      kf <> (t \\ ks)

uusiRemove :: HasVersionedPackage a => SomeUusi -> Op [a]
uusiRemove actions t = let ps = [p | (Remove _ p) <- actions] in filter (\x -> and <| fmap (not . (<| (x ^. myPkgName))) ps) t

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