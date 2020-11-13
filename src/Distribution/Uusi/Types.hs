{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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

data Action tag p = Remove tag p | SetVersion tag p VersionRange | Replace tag p [VersionedPackage]

instance (Show tag) => Show (Action tag p) where
  show (Remove tag _) = "Remove[" <> show tag <> "]"
  show (SetVersion tag _ range) = "SetVersion[" <> show tag <> ", " <> prettyShow range <> "]"
  show (Replace tag _ targets) = "Replace[" <> show tag <> " |-> " <> T.unpack (T.intercalate ", " $ T.pack . show <$> targets) <> "]"

type Uusi = Action Text (PackageName -> Bool)

type SomeUusi = [Uusi]

type Op a = a -> a

-----------------------------------------------------------------------------

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