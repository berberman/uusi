{-# LANGUAGE RankNTypes #-}

-- | Copyright: (c) 2020-2021 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <berberman@yandex.com>
-- Stability: experimental
-- Portability: portable
-- Lenses from "Distribution.Compat.Lens"
module Distribution.Uusi.Lens
  ( module Distribution.Compat.Lens,
    lens,
    (<&>),
    mapped,
    HasVersionedPackage (..),
    HasComponentialPackage (..),
  )
where

import Data.Functor ((<&>))
import Data.Functor.Identity
import Distribution.Compat.Lens
import Distribution.Types.Dependency
import Distribution.Types.ExeDependency
import Distribution.Types.LegacyExeDependency
import Distribution.Types.PackageName
import Distribution.Types.UnqualComponentName
import Distribution.Types.VersionRange
import Distribution.Uusi.Types

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE lens #-}

mapped :: Functor f => ASetter (f a) (f b) a b
mapped f = Identity . fmap (runIdentity . f)
{-# INLINE mapped #-}

class HasVersionedPackage a where
  versionedPackage :: Lens' a VersionedPackage
  myPkgName :: Lens' a PackageName
  myVersionRange :: Lens' a VersionRange

  myPkgName = versionedPackage . myPkgName
  {-# INLINE myPkgName #-}
  myVersionRange = versionedPackage . myVersionRange
  {-# INLINE myVersionRange #-}

instance HasVersionedPackage VersionedPackage where
  versionedPackage = id
  myPkgName = lens _myPkgName (\(VersionedPackage _ range) name -> VersionedPackage name range)
  {-# INLINE myPkgName #-}
  myVersionRange = lens _myVersionRange (\(VersionedPackage name _) range -> VersionedPackage name range)
  {-# INLINE myVersionRange #-}

class HasComponentialPackage a where
  componentialPackage :: Lens' a ComponentialPackage
  myComponentName :: Lens' a UnqualComponentName
  myCorePackage :: Lens' a VersionedPackage
  myComponentName = componentialPackage . myComponentName
  {-# INLINE myComponentName #-}
  myCorePackage = componentialPackage . myCorePackage
  {-# INLINE myCorePackage #-}

instance HasComponentialPackage ComponentialPackage where
  componentialPackage = id
  myComponentName = lens _myComponentName (\(ComponentialPackage core _) name -> ComponentialPackage core name)
  {-# INLINE myComponentName #-}
  myCorePackage = lens _myCorePackage (\(ComponentialPackage _ name) core -> ComponentialPackage core name)
  {-# INLINE myCorePackage #-}

instance HasVersionedPackage Dependency where
  versionedPackage =
    lens
      (\(Dependency name range _) -> VersionedPackage name range)
      (\(Dependency _ _ lib) (VersionedPackage name range) -> Dependency name range lib)
  {-# INLINE versionedPackage #-}

instance HasVersionedPackage ExeDependency where
  versionedPackage =
    lens
      (\(ExeDependency name _ range) -> VersionedPackage name range)
      (\(ExeDependency _ component _) (VersionedPackage name range) -> ExeDependency name component range)
  {-# INLINE versionedPackage #-}

instance HasComponentialPackage ExeDependency where
  componentialPackage =
    lens
      (\x@(ExeDependency _ component _) -> ComponentialPackage (x ^. versionedPackage) component)
      ( \x (ComponentialPackage core component) ->
          x & myCorePackage .~ core
            & myComponentName .~ component
      )
  {-# INLINE componentialPackage #-}

instance HasVersionedPackage LegacyExeDependency where
  versionedPackage =
    lens
      (\(LegacyExeDependency name range) -> VersionedPackage (mkPackageName name) range)
      (\_ (VersionedPackage name range) -> LegacyExeDependency (unPackageName name) range)
  {-# INLINE versionedPackage #-}
