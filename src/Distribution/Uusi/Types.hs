{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Copyright: (c) 2020 berberman
-- SPDX-License-Identifier: MIT
-- Maintainer: berberman <1793913507@qq.com>
-- Stability: experimental
-- Portability: portable
-- Types and lenses used by the library.
module Distribution.Uusi.Types (module Distribution.Uusi.Types) where

import Data.Text (Text)
import qualified Data.Text as T
import Distribution.Pretty (prettyShow)
import Distribution.Types.Dependency (Dependency (..))
import Distribution.Types.ExeDependency (ExeDependency (..))
import Distribution.Types.LegacyExeDependency (LegacyExeDependency (..))
import Distribution.Types.PackageName (PackageName, mkPackageName, unPackageName)
import Distribution.Types.UnqualComponentName (UnqualComponentName)
import Distribution.Types.VersionRange (VersionRange)
import Lens.Micro
import Lens.Micro.TH (makeClassy)

-----------------------------------------------------------------------------

-- | Action acting on cabal dependencies.
data Action tag
  = -- | For a dependency x, if P(x) then remove x
    Remove tag (PackageName -> Bool)
  | -- | For a dependency x, if P(x) then set x's version range
    SetVersion tag (PackageName -> Bool) VersionRange
  | -- | For a dependency x, if P(x) then replace x with a set of packages
    Replace tag (PackageName -> Bool) [VersionedPackage]
  | -- | For a component x, if P(x) then set the buildable of x
    SetBuildable tag (UnqualComponentName -> Bool) Bool
  | -- | For a component or library x, if P(x) then modify build options of x
    ModifyBuiltOptions tag (UnqualComponentName -> Bool) (Op [String])

instance (Show tag) => Show (Action tag) where
  show (Remove tag _) = "Remove[" <> show tag <> "]"
  show (SetVersion tag _ range) = "SetVersion[" <> show tag <> ", " <> prettyShow range <> "]"
  show (Replace tag _ targets) = "Replace[" <> show tag <> " |-> " <> T.unpack (T.intercalate ", " $ T.pack . show <$> targets) <> "]"
  show (SetBuildable tag _ buildable) = "SetBuildable[" <> show tag <> ", " <> show buildable <> "]"
  show (ModifyBuiltOptions tag _ _) = "ModifyBuiltOptions[" <> show tag <> "]"

-- | Common 'Action', where the tag 'Text'.
type Uusi = Action Text

-- | A list of 'Uusi'
type Uusis = [Uusi]

-- | An endo operation
type Op a = a -> a

-----------------------------------------------------------------------------

-- | Super type of three kinds of dependency.
-- Because cabal doesn't define lenses of 'Dependency', 'ExeDependency', and 'LegacyExeDependency',
-- here comes out a general data type to define overloaded lenses.
-- See 'HasVersionedPackage'.
data VersionedPackage = VersionedPackage
  { _myPkgName :: PackageName,
    _myVersionRange :: VersionRange
  }

instance Show VersionedPackage where
  show (VersionedPackage name range) = show (unPackageName name) <> ":" <> prettyShow range

-- | Sub type of 'VersionedPackage', with 'UnqualComponentName'.
-- Similar to 'VersionedPackage', for defining lenses.
-- See 'HasComponentialPackage'.
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
