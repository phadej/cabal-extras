module CabalDocspec.Phase1 (
    phase1,
) where

import Peura

import qualified Distribution.ModuleName      as C
import qualified Distribution.Types.BuildInfo as C
import qualified Distribution.Types.Version   as C

import CabalDocspec.Cpp
import CabalDocspec.Doctest.Extract
import CabalDocspec.Lexer
import CabalDocspec.Located
import CabalDocspec.Trace

-- | First phase.
--
-- Read file, extract doctest blocks.
--
-- In particular, this runs CPP preprocessing if needed.
phase1
    :: TracerPeu r Tr
    -> GhcInfo
    -> Maybe (Path Absolute)  -- ^ builddir
    -> PackageName            -- ^ package name
    -> Version                -- ^ package version
    -> Path Absolute          -- ^ package directory
    -> Bool                   -- ^ cpp extension
    -> [Path Absolute]        -- ^ additional include directories
    -> [PackageIdentifier]    -- ^ dependencies
    -> C.BuildInfo
    -> C.ModuleName
    -> Path Absolute
    -> Peu r (Module (Located String))
phase1 tracer ghcInfo mbuildDir pkgName_ pkgVer pkgDir cppEnabled cppDirs pkgIds bi modname modpath = do
    traceApp tracer $ TracePhase1 modname modpath

    contents <- fromUTF8BS <$> readByteString modpath

    -- lex the input.
    -- If it includes {-# LANGUAGE CPP #-}, then cpphs and re-lex.
    comments <- case needsCppPass cppEnabled contents of
        Just tokens -> do
            -- putDebug tracer $ unlines $ map show tokens
            return $ extractComments tokens
        Nothing -> do
            contents' <- cpphs tracer pkgVer pkgIds cppIncludes cppDefines modpath contents
            evaluate $ force $ extractComments $ stubbornPass0 contents'

    -- putDebug tracer $ unlines $ map show comments

    -- extract docstrings from all comments
    let docs = extractDocstrings modname comments

    return docs
  where
    cppIncludes :: [Path Absolute]
    cppIncludes =
        -- if there are absolute dirs, they are converted to relative,
        -- so may break
        [ pkgDir </> fromUnrootedFilePath dir
        | dir <- C.includeDirs bi
        ] ++
        [ buildDir </>
          fromUnrootedFilePath "build" </>
          componentDir ghcInfo (PackageIdentifier pkgName_ pkgVer) </>
          fromUnrootedFilePath "build" </>
          fromUnrootedFilePath dir
        | buildDir <- toList mbuildDir
        , dir <- C.includeDirs bi
        ] ++
        cppDirs

    cppDefines :: [(String, String)]
    cppDefines =
        [ ("__GLASGOW_HASKELL__", cppGhcVersion (ghcVersion ghcInfo))
        ] ++
        [ d'
        | d <- C.cppOptions bi
        , Just d' <- return (parseDefineFlag d)
        ]

-- x86_64-linux/ghc-9.8.1/streamly-0.10.0
componentDir :: GhcInfo -> PackageIdentifier -> Path Unrooted
componentDir ghcInfo pid = 
    fromUnrootedFilePath(ghcPlatform ghcInfo) </>
    fromUnrootedFilePath ("ghc-" ++ prettyShow (ghcVersion ghcInfo)) </>
    fromUnrootedFilePath (prettyShow pid)

cppGhcVersion :: Version -> String
cppGhcVersion v = case C.versionNumbers v of
    []      -> "0"
    (x:[])  -> show (x * 100)
    (x:y:_) -> show (x * 100 + y)

parseDefineFlag :: String -> Maybe (String, String)
parseDefineFlag ('-' : 'D' : rest) =
    case after of
        []        -> Just (before, "1")
        '=':value -> Just (before, value)
        _         -> Nothing -- shouldn't happen
  where
    (before, after) = span (/= '=') rest

parseDefineFlag _ = Nothing
