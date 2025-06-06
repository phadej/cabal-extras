# 0.0.0.20250606

- Pass `rts` package include directories to C-preprocessor.

# 0.0.0.20240720

- Accept @pkgname:libname@ target syntax

# 0.0.0.20240703

- Fix "Project Unit Id" bug

# 0.0.0.20240702

- Support `cabal-install-3.12` changed store directory logic
- Update dependencies, in particular use `Cabal-3.12.1.0`

# 0.0.0.20240414

- `--extra-package` accepts sublibraries, i.e. `mypkg:sublib` syntax.
- `cabal-docspec` tests all library components, also internal (visible and invisible) components.

# 0.0.0.20231219

- Pass `default-language` flag to GHC
- Fix issue with CPP defines without an explicit value (i.e. `-DFOO`, not `-DBAR=42`)
- Include `include-dirs` in build directory (for Configure generated headers)

# 0.0.0.20230406

- Change failing docspec output to be proper diff.
  Additionally print actual output for easy copying.
- Preprocess source files also if `default-extension` contains `CPP`
- Update dependencies, in particular use `Cabal-3.10.1.0`
- Build with GHC-9.2.7

# 0.0.0.20211114

- Add `--module` flag
- static binary release

# 0.0.0.20210111

- `--check-properties`
- CPP includes
- ghci RTS options

# 0.0.0.20210110

- Set datadir environment variables

# 0.0.0.20210108

- Named chunks handling

# 0.0.0.20201230.1

- Second alpha release

# 0.0.0.20201230

- First alpha release
