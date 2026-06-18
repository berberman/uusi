# Changelog

`uusi` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

## 0.4.4.0

* Support `Cabal` 3.12, 3.14, and 3.16.
* Support GHC 9.8, 9.10, 9.12, and 9.14.
* Add `uusi-extract` executable to print package name and version.
* Update Arch Linux package link from `community` to `extra`.

## 0.4.3.0

* Use .cabal file in cwd if the target is not set
* Support `Cabal` 3.6, 3.8, and 3.10.

## 0.4.2.0

* Support `Cabal-3.4`

## 0.4.1.0

* Remove `microlens` and `microlens-th` - now `uusi` only depends on boot libs

## 0.4.0.0

* Support modifying `ghc-options` 

## 0.3.1.0

* Remove `--gen-setup` from options of `uusi`

* Add `gen-setup` executable

* Clearify cabal version constraint

## 0.3.0.0

* Add `--buildable` and `--no-buildable`

* Fix a typo in prompt [#5](https://github.com/berberman/uusi/pull/5)

## 0.2.1.0

* Add `--gen-setup`

* Fix typo

## 0.2.0.0

* Provide functionalities other than removing all version constraints

* Refactor project structure, `uusi` now becomes a library

* Add some unit tests

## 0.1.0.0

* Remove diff

* Reduce dependencies as much as possible

## 0.0.1.0

* Support sub-libraries and legacy build tools.

## 0.0.0.0

* Split from [arch-hs](https://github.com/berberman/arch-hs).

[1]: https://pvp.haskell.org
[2]: https://github.com/berberman/uusi/releases
