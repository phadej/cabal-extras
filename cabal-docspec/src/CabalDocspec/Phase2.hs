{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
module CabalDocspec.Phase2 (
    phase2,
) where

import Peura

import qualified Cabal.Config           as Cabal
import qualified Language.Haskell.Lexer as L

import CabalDocspec.Doctest.Example
import CabalDocspec.Doctest.Extract
import CabalDocspec.Doctest.Parse
import CabalDocspec.GHCi
import CabalDocspec.Located
import CabalDocspec.Opts
import CabalDocspec.Summary
import CabalDocspec.Trace

phase2
    :: TracerPeu r Tr
    -> DynOpts
    -> [UnitId]
    -> GhcInfo
    -> Maybe (Path Absolute) -- ^ Build directory, @builddir@
    -> Cabal.Config Identity
    -> Path Absolute
    -> [Module [Located DocTest]]
    -> Peu r Summary
phase2 tracer dynOpts unitIds ghcInfo mbuildDir cabalCfg cwd parsed = do
    let preserveIt = case optPreserveIt dynOpts of
            PreserveIt     -> True
            DontPreserveIt -> False

    let timeout :: Int
        timeout = max fastTimeout $ truncate $ optTimeout dynOpts * 1e6

    -- second phase: fire up the ghci, and execute stuff
    storeDir <- makeAbsoluteFilePath $ runIdentity $ Cabal.cfgStoreDir cabalCfg
    let storeDir' = storeDir </> fromUnrootedFilePath ("ghc-" ++ prettyShow (ghcVersion ghcInfo))
    let storeDb = storeDir' </> fromUnrootedFilePath "package.db"
    storeExists <- doesDirectoryExist storeDb

    let GhcFlags {..} = getGhcFlags ghcInfo

    let ghciArgs :: [String]
        ghciArgs =
            [ "-i" -- so we don't explode on hs-source-dirs: . packages
            ] ++
            [ "-X" ++ ext
            | ext <- optExts dynOpts
            ] ++
            [ "-hide-all-packages"
            , ghcFlagNoUserPackageDb
            -- , "-package-db=" ++ toFilePath (ghcGlobalDb ghcInfo)
            ] ++
            [ ghcFlagPackageDb ++ "=" ++ toFilePath storeDb
            | storeExists
            ] ++
            [ ghcFlagPackageDb ++ "=" ++ toFilePath localDb
            | buildDir <- toList mbuildDir
            , let localDb = buildDir </> fromUnrootedFilePath "packagedb" </> fromUnrootedFilePath ("ghc-" ++ prettyShow (ghcVersion ghcInfo))
            ] ++
            [ ghcFlagPackageId ++ "=" ++ u
            | u <- map prettyShow unitIds
            ]

    withInteractiveGhc tracer ghcInfo cwd ghciArgs $ \ghci -> do
        fmap mconcat $ for parsed $ \m -> do
            traceApp tracer $ TracePhase2 (moduleName m)

            let reset = do
                    void $ eval tracer ghci False fastTimeout ":r"
                    when preserveIt $ void $ eval tracer ghci False fastTimeout "()"

            -- load module in question
            reset
            _ <- eval tracer ghci False fastTimeout $ ":m " ++ prettyShow (moduleName m)

            let runSetup = do
                    -- TODO: --fast
                    reset

                    -- command line --setup
                    for_ (optSetup dynOpts) $ \expr -> do
                        result <- eval tracer ghci preserveIt timeout expr
                        case mkResult [] (lines result) of
                                Equal -> return ()
                                NotEqual diff -> do
                                    putError tracer expr
                                    putError tracer $ unlines ("" : diff)

                    -- in file $setup
                    for_ (moduleSetup m) $ \setups -> for_ setups $ \(L pos setup) -> case setup of
                        Property expr -> putError tracer $ "properties are not supported in setup, skipping: " ++ expr
                        Example expr expected -> do
                            result <- eval tracer ghci preserveIt timeout expr
                            case mkResult expected (lines result) of
                                Equal -> return ()
                                NotEqual diff -> do
                                    putError tracer expr
                                    putError tracer $ prettyPos pos ++ unlines ("" : diff)

            let runExampleGroup !acc [] = return acc
                runExampleGroup !acc (L pos doctest : next) = case doctest of
                    Property expr -> do
                        putError tracer $ "properties not implemented, skipping " ++ expr
                        runExampleGroup (acc <> Summary 1 0 1 0 1 0) next

                    Example expr expected -> do
                        result <- eval tracer ghci preserveIt timeout expr
                        case mkResult expected (lines result) of
                            Equal -> do
                                runExampleGroup (acc <> Summary 1 1 1 0 0 0) next
                            NotEqual diff -> do
                                putError tracer expr
                                putError tracer $ prettyPos pos ++ unlines ("" : diff)
                                let lnext = length next
                                return (acc <> Summary (1 + lnext) 1 0 1 0 lnext)

            fmap mconcat $ for (moduleContent m) $ \contents -> do
                runSetup
                runExampleGroup mempty contents

fastTimeout :: Int
fastTimeout = 1000000

eval :: TracerPeu r Tr -> GHCi -> Bool -> Int -> String -> Peu r String
eval tracer ghci preserveIt timeout expr = do
    traceApp tracer $ TraceGHCiInput expr
    res <- sendExpressions tracer ghci preserveIt timeout [expr]
    case res of
        Timeout -> return "* Hangs forever *\n"
        Exited ec -> die tracer $ "ghci exited " ++ show ec
        Result out err -> do
            -- putDebug tracer (fromUTF8BS out)
            -- putDebug tracer (fromUTF8BS err)

            -- first the "importand" error output, then standard output
            return (fromUTF8BS err ++ fromUTF8BS out)

prettyPos :: L.Pos -> String
prettyPos pos = "in comment at " ++ show (L.line pos) ++ ":" ++ show (L.column pos)
