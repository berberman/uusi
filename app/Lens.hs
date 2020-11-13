{-# LANGUAGE TemplateHaskell #-}

module Lens
  ( module Lens,
    module Lens.Micro,
  )
where

import Distribution.Pretty (prettyShow)
import Distribution.Types.Dependency (Dependency (..))
import Distribution.Types.ExeDependency (ExeDependency (..))
import Distribution.Types.LegacyExeDependency
  ( LegacyExeDependency (..),
  )
import Distribution.Types.PackageName
  ( PackageName,
    mkPackageName,
    unPackageName,
  )
import Distribution.Types.UnqualComponentName
  ( UnqualComponentName,
  )
import Distribution.Types.VersionRange (VersionRange)
import Lens.Micro
import Lens.Micro.TH (makeClassy)

data VersionedPackage = VersionedPackage
  { _myPkgName :: PackageName,
    _myVersionRange :: VersionRange
  }

instance Show VersionedPackage where
  show (VersionedPackage name range) = show (unPackageName name) <> ":" <> prettyShow range

data ComponentialPackage = ComponentialPackage
  { _myCorePackage :: VersionedPackage,
    _myComponentName :: UnqualComponentName
  }

makeClassy ''VersionedPackage
makeClassy ''ComponentialPackage

instance HasVersionedPackage Dependency where
  versionedPackage =
    lens
      (\(Dependency name range _) -> VersionedPackage name range)
      (\(Dependency _ _ lib) (VersionedPackage name range) -> Dependency name range lib)

instance HasVersionedPackage ExeDependency where
  versionedPackage =
    lens
      (\(ExeDependency name _ range) -> VersionedPackage name range)
      (\(ExeDependency _ component _) (VersionedPackage name range) -> ExeDependency name component range)

instance HasComponentialPackage ExeDependency where
  componentialPackage =
    lens
      (\x@(ExeDependency _ component _) -> ComponentialPackage (x ^. versionedPackage) component)
      ( \x (ComponentialPackage core component) ->
          x & myCorePackage .~ core
            & myComponentName .~ component
      )

instance HasVersionedPackage LegacyExeDependency where
  versionedPackage =
    lens
      (\(LegacyExeDependency name range) -> VersionedPackage (mkPackageName name) range)
      (\_ (VersionedPackage name range) -> LegacyExeDependency (unPackageName name) range)

infixl 1 |>

infixr 0 <|

-- qwq
(|>) :: a -> (a -> b) -> b
(|>) = (&)

(<|) :: (a -> b) -> a -> b
(<|) = ($)