# Changelog for [`os-string` package](http://hackage.haskell.org/package/os-string)

## 2.0.7 *Nov 2024*

* don't catch asynchronous exceptions during encoding/decoding wrt [#22](https://github.com/haskell/os-string/issues/22)

## 2.0.6 *Jun 2024*

* add `fromString` on windows
* add `fromBytestring` on unix

## 2.0.5 *Jun 2024*

* Add `decodeLE`/`encodeLE` and deprecate `decodeFS`/`encodeFS` (pointing users to `System.OsPath` instead), fixes [#19](https://github.com/haskell/os-string/issues/19)

## 2.0.4 *Jun 2024*

* Use TemplateHaskellQuotes rather than TemplateHaskell extension wrt [#21](https://github.com/haskell/os-string/issues/21)

## 2.0.3 *May 2024*

* Fix `length` function wrt [#17](https://github.com/haskell/os-string/issues/17)

## 2.0.2.2 *May 2024*

* Fix compilation on big-endian arches, by Andrew Lelechenko

## 2.0.2.1 *Apr 2024*

* Fix compabitiliby with GHC 9.10

## 2.0.2 *Dec 2023*

* Implement coercionToPlatformTypes, fixes #4

## 2.0.1 *Dec 2023*

* add `unsafeEncodeUtf`, fixes #5

## 2.0.0 *Nov 2023*

* Split out `OsString` modules from filepath library
* add more bytestring like functions (index/search/etc.)

## 1.0.0 *Nov 2023*

* dummy release to avoid name clashes with filepath <1.5

