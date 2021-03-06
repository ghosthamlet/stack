## Unreleased

* Remove GHC uncompressed tar file after installation [#376](https://github.com/commercialhaskell/stack/issues/376)
* Put stackage snapshots JSON on S3 [#380](https://github.com/commercialhaskell/stack/issues/380)
* Specifying flags for multiple packages [#335](https://github.com/commercialhaskell/stack/issues/335)
* single test suite failure should show entire log [#388](https://github.com/commercialhaskell/stack/issues/388)
* valid-wanted is a confusing option name [#386](https://github.com/commercialhaskell/stack/issues/386)
* stack init in multi-package project should use local packages for dependency checking [#384](https://github.com/commercialhaskell/stack/issues/384)
* Display information on why a snapshot was rejected [#381](https://github.com/commercialhaskell/stack/issues/381)
* Give a reason for unregistering packages [#389](https://github.com/commercialhaskell/stack/issues/389)

## 0.1.0.0

* Fall back to cabal dependency solver when a snapshot can't be found
* Basic implementation of `stack new` [#137](https://github.com/commercialhaskell/stack/issues/137)
* `stack solver` command [#364](https://github.com/commercialhaskell/stack/issues/364)
* `stack path` command [#95](https://github.com/commercialhaskell/stack/issues/95)
* Haddocks [#143](https://github.com/commercialhaskell/stack/issues/143):
    * Build for dependencies
    * Use relative links
    * Generate module contents and index for all packages in project

## 0.0.3

* `--prefetch` [#297](https://github.com/commercialhaskell/stack/issues/297)
* `upload` command ported from stackage-upload [#225](https://github.com/commercialhaskell/stack/issues/225)
* `--only-snapshot` [#310](https://github.com/commercialhaskell/stack/issues/310)
* `--resolver` [#224](https://github.com/commercialhaskell/stack/issues/224)
* `stack init` [#253](https://github.com/commercialhaskell/stack/issues/253)
* `--extra-include-dirs` and `--extra-lib-dirs` [#333](https://github.com/commercialhaskell/stack/issues/333)
* Specify intra-package target [#201](https://github.com/commercialhaskell/stack/issues/201)

## 0.0.2

* Fix some Windows specific bugs [#216](https://github.com/commercialhaskell/stack/issues/216)
* Improve output for package index updates [#227](https://github.com/commercialhaskell/stack/issues/227)
* Automatically update indices as necessary [#227](https://github.com/commercialhaskell/stack/issues/227)
* --verbose flag [#217](https://github.com/commercialhaskell/stack/issues/217)
* Remove packages (HTTPS and Git) [#199](https://github.com/commercialhaskell/stack/issues/199)
* Config values for system-ghc and install-ghc
* Merge `stack deps` functionality into `stack build`
* `install` command [#153](https://github.com/commercialhaskell/stack/issues/153) and [#272](https://github.com/commercialhaskell/stack/issues/272)
* overriding architecture value (useful to force 64-bit GHC on Windows, for example)
* Overhauled test running (allows cycles, avoids unnecessary recompilation, etc)

## 0.0.1

* First public release, beta quality
