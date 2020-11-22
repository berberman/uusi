# uusi

[![GitHub CI](https://github.com/berberman/uusi/workflows/CI/badge.svg)](https://github.com/berberman/uusi/actions)
[![Build Status](https://travis-ci.com/berberman/uusi.svg?branch=master)](https://travis-ci.com/berberman/uusi)
[![Hackage](https://img.shields.io/hackage/v/uusi.svg?logo=haskell)](https://hackage.haskell.org/package/uusi)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

`uusi` is a simple command-line program to tweak [Package Description](https://cabal.readthedocs.io/en/latest/cabal-package.html#package-description).
In distribution packaging, sometimes we need modify the version range of dependencies. Usually, this can be done by some *nix text processing utilities, such as awk, sed, etc.
However, these tools process cabal files as plain text, not considering the semantics. Thus, it might be pesky or elusive to conver the entire cases.
`uusi` has almost no extra dependencies, so it's safe to introduce `uusi` as build-depends of the target, which packagers can use to manipulate target's dependencies.
Currently, `uusi` works in Arch Linux packaging, and you may find out that it has been [required by](https://www.archlinux.org/packages/community/x86_64/uusi/) many haskell packages.
In most cases, it is used to loose the version constraint of a dependency when building a package, because the dependent haskell package provided by system may be too new to fit the constraints,
whereas this package can be built against the newer dependency successfully.

## Installation

```
# pacman -S uusi
```

Install it via `pacman`.

## Build

```
$ git clone https://github.com/berberman/uusi
```

If you choose to use dynamic haskell packages provided by Arch Linux, run to configure the project locally:

```
$ cabal configure --disable-library-vanilla --enable-shared --enable-executable-dynamic --ghc-options=-dynamic
```

Then build it:

```
$ cabal build
```

## Usage

`uusi` will replace the text in .cabal file inplace. `uusi` supports four kinds of actions:

* remove all version constraints of dependencies

* overwrite a version range of a specific dependency

* remove a dependency

* replace a dependency with given packages

* generate a boilerplate `Setup.hs`

If no option is specified, `uusi` will use `-all`, removing all version constraints.

### Examples

* Set all dependencies' version ranges to `-any` and generate `Setup.hs`:

```
$ uusi --gen-setup foo.cabal
```

* Replace `old-time` with `time`:

```
$ uusi -rold-time:time foo.cabal
```

* Remove `semigroup`:

```
$ uusi -dsemigroup foo.cabal
```

* Overwrite `base`:

```
$ uusi -ubase: >=4.14 foo.cabal
```

Packages have pattern `<pkgname>` or `<pkgname>:<version>`. Run this program, and see help message for details.

* Together:

```
$ uusi -ubar:2999.20.1.0 -dbase -all foo.cabal
```


## Contributing

Issues and PRs are always welcome. **_\(:з」∠)\_**