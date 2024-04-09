# Revision history for `paths`

## 0.2.0.0

* Make `Path` abstract by default and move type-unsafe operations into new `System.Path.Unsafe` module
* Add wrappers for `Data.Text(.Lazy).IO` now that `text` is bundled with GHC
* Add `appendByteString` & `appendLazyByteString` wrappers
* Add `{has,drop,add}TrailingPathSeparator` operations
* Add new `takeBaseName` and `normalise` operations
* Introduce `FileExt` type for representing file extensions in the API and add more file-extension related operations.
* Change types of `joinFragments` and `splitFragments`, and add new `fragments` smart-constructor
* Change type-signature of `takeFileName`
* Add new `System.Path.QQ` module providing QuasiQuoters
* Add new `System.Path.Lens` module
* Rename `Relative` to `CWD`
* Synchronize operator fixities of `<.>`, `-<.>` and `</>` to match the ones from the `filepath` library

## 0.1

* First version. Mostly derived from `hackage-security`'s `Hackage.Security.Util.Path`
