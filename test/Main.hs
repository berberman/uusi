{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parsec
  ( parseGenericPackageDescription,
    runParseResult,
  )
import Distribution.Parsec (simpleParsec)
import Distribution.Simple.Utils (toUTF8BS)
import Distribution.Types.Dependency (Dependency)
import Distribution.Types.VersionRange
  ( VersionRange,
    anyVersion,
  )
import Distribution.Uusi.Core
import Distribution.Uusi.Lens
import Distribution.Uusi.Types
import Test.HUnit

main :: IO ()
main = runTestTTAndExit tests

tests :: Test
tests =
  test
    [ testRemoveAllConstraints,
      testOverwrite,
      testRemove,
      testReplace,
      testReplaceAndRemove
    ]

testRemoveAllConstraints :: Test
testRemoveAllConstraints =
  createTest "remove all version constraints" [allToAnyVersion] $ fmap (myVersionRange .~ anyVersion)

testOverwrite :: Test
testOverwrite =
  createTest "overwrite" [overwriteByName name ver] $ fmap (\x -> x & myVersionRange %~ if x ^. myPkgName == name then const ver else id)
  where
    name = "b"
    ver = versionRange "> 666"

testRemove :: Test
testRemove =
  let name = "d"
   in createTest "remove" [removeByName name] $ filter (\x -> x ^. myPkgName /= name)

testReplace :: Test
testReplace =
  let name = "base"
      targets = zip ["apple", "banana"] (repeat $ versionRange "-any")
   in createTest
        "replace"
        [replaceByName name targets]
        (\k -> [r | x <- k, r <- if x ^. myPkgName == name then fmap (\(n, v) -> x & myPkgName .~ n & myVersionRange .~ v) targets else [x]])

testReplaceAndRemove :: Test
testReplaceAndRemove =
  let name1 = "d"
      name2 = "base"
      targets = zip ["apple", "banana"] (repeat $ versionRange "-any")
   in createTest
        "replace and remove"
        [removeByName name1, replaceByName name2 targets]
        (\k -> [r | x <- k, r <- if x ^. myPkgName == name2 then fmap (\(n, v) -> x & myPkgName .~ n & myVersionRange .~ v) targets else [x], r ^. myPkgName /= name1])

createTest :: String -> Uusis -> Op [Dependency] -> Test
createTest label actions f = label ~: expectedDeps ~=? afterDeps
  where
    origin = parseCabalFile cabalFile
    originDeps = flattenDependencies origin
    after = uusiGenericPackageDescription actions origin
    afterDeps = flattenDependencies after
    expectedDeps = f originDeps

versionRange :: String -> VersionRange
versionRange s = case simpleParsec s of
  Just x -> x
  _ -> error $ "failed to parse version range: " <> s

cabalFile :: String
cabalFile =
  unlines
    [ "name: test",
      "version: 1.0.0",
      "build-type: Simple",
      "cabal-version: 2.0",
      "library",
      "  build-depends: base, a, b > 233, c, d ^>= 2999.20.1.0"
    ]

parseCabalFile :: String -> GenericPackageDescription
parseCabalFile s = case snd . runParseResult . parseGenericPackageDescription . toUTF8BS $ s of
  Left (_, err) -> error $ show err
  Right x -> x

flattenDependencies :: GenericPackageDescription -> [Dependency]
flattenDependencies = allBuildDepends . flattenPackageDescription
