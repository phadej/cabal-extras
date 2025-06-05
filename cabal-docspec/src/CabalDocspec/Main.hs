{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Copyright: Oleg Grenrus
-- License: GPL-3.0-or-later
module CabalDocspec.Main (main) where

import Peura

import Control.Applicative ((<**>))

import qualified Cabal.Config                                 as Cabal
import qualified Cabal.Plan                                   as Plan
import qualified Data.Map.Strict                              as Map
import qualified Data.Set                                     as Set
import qualified Data.Text                                    as T
import qualified Distribution.Compat.NonEmptySet              as NES
import qualified Distribution.Compiler                        as C
import qualified Distribution.ModuleName                      as C
import qualified Distribution.Package                         as C
import qualified Distribution.System                          as C
import qualified Distribution.Types.BuildInfo                 as C
import qualified Distribution.Types.ComponentName             as C
import qualified Distribution.Types.CondTree                  as C
import qualified Distribution.Types.ConfVar                   as C
import qualified Distribution.Types.Flag                      as C
import qualified Distribution.Types.GenericPackageDescription as C
import qualified Distribution.Types.InstalledPackageInfo      as IPI
import qualified Distribution.Types.Library                   as C
import qualified Distribution.Types.PackageDescription        as C
import qualified Distribution.Types.UnqualComponentName       as C
import qualified Distribution.Utils.Path                      as C
import qualified Distribution.Version                         as C
import qualified Language.Haskell.Extension                   as Ext
import qualified Options.Applicative                          as O
import qualified System.FilePath                              as FP

import CabalDocspec.Doctest.Extract
import CabalDocspec.Doctest.Parse
import CabalDocspec.Lexer
import CabalDocspec.Library
import CabalDocspec.Located
import CabalDocspec.Man
import CabalDocspec.Opts
import CabalDocspec.Package
import CabalDocspec.Phase1
import CabalDocspec.Phase2
import CabalDocspec.Summary
import CabalDocspec.Trace
import CabalDocspec.Warning

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    opts <- O.execParser optsP'
    tracer0 <- makeTracerPeu (optTracer opts defaultTracerOptions)

    let dynOpts :: DynOpts
        dynOpts = optGhci opts defaultDynOpts

    -- modify tracer verbosity
    let tracer :: TracerPeu () Tr
        tracer = adjustTracer (optVerbosity dynOpts) tracer0

    -- main action
    runPeu tracer () $ do
        -- display manual
        when (optPhase dynOpts == Manual) $ liftIO man

        -- check if we know these extensions
        for_ (optExts (optGhci opts defaultDynOpts)) $ \ext -> case Ext.classifyExtension ext of
            Ext.UnknownExtension _ -> putWarning tracer WUnknownExtension $
                ext ++ " is unknown extension. GHCi may fail to start."
            _ -> pure ()

        -- gather info
        ghcInfo <- getGhcInfo tracer (optCompiler opts)
        cabalCfg  <- liftIO Cabal.readConfig

        res <- case optCabalPlan opts of
            CabalPlan -> do
                buildDir <- makeAbsolute (optBuildDir opts)
                plan <- liftIO $ Plan.findAndDecodePlanJson $ Plan.InBuildDir $ toFilePath buildDir

                -- checks
                checkGhcVersion tracer ghcInfo plan

                -- Elaborate plan by reading local package definitions
                pkgs0 <- readLocalCabalFiles tracer plan
                pkgs <-
                    if null (optTargets opts)
                    then return pkgs0
                    else fmap concat $ for (optTargets opts) $ \(Target pn _) -> do
                        let match =
                                [ pkg
                                | pkg <- pkgs0
                                , C.packageName (pkgGpd pkg) == pn
                                ]

                        case match of
                            []  -> die tracer $ "No package " ++ prettyShow pn ++ " in the plan"
                            _   -> return match

                -- collect environment of datadirs
                let env :: [(String, String)]
                    env =
                        [ (manglePackageName pn ++ "_datadir", toFilePath $ pkgDir pkg </> fromUnrootedFilePath (C.dataDir pd))
                        | pkg <- pkgs0
                        , let pd = C.packageDescription (pkgGpd pkg)
                        , let pn = C.packageName pd
                        , not (null (C.dataFiles pd))
                        ]

                let checkComponent :: C.PackageName -> C.ComponentName -> Bool
                    checkComponent pn cn
                        | null (optTargets opts) = True
                        | otherwise = any p $ optTargets opts
                      where
                        p (Target pn' Nothing)   = pn == pn'
                        p (Target pn' (Just ln)) = pn == pn' && cn == C.CLibName ln

                -- process components
                res <- for pkgs $ \pkg -> do
                    for (pkgUnits pkg) $ \unit ->
                        ifor (Plan.uComps unit) $ \cn ci -> do
                            if checkComponent (C.packageName (pkgGpd pkg)) (toCabal cn)
                            then testComponent tracer0 tracer (optGhci opts)
                                    ghcInfo buildDir cabalCfg plan env pkg unit cn ci
                            else return mempty

                -- summarize Summary's
                return $ foldMap (foldMap (foldMap id)) res

            NoCabalPlan -> do
                dbG <- readPackageDb tracer (ghcGlobalDb ghcInfo)
                pkgs <- readDirectCabalFiles tracer
                    [ prettyShow pn
                    | Target pn _ <- optTargets opts
                    ]

                -- process components
                res <- for pkgs $ \pkg -> do
                    testComponentNo tracer0 tracer (optGhci opts)
                        ghcInfo cabalCfg dbG pkg

                return $ foldMap id res

        -- if there are errors, exit
        traceApp tracer $ TraceSummary res
        let res' = sumSummary res
        unless (isOk res') $ do
            putError tracer "there were errors or property failures"
            exitFailure
  where
    optsP' = O.info (optsP <**> O.helper <**> versionP) $ mconcat
        [ O.fullDesc
        , O.header "cabal-docspec - another doctest for Haskell"
        ]

    versionP = O.infoOption VERSION_cabal_docspec
        $ O.long "version" <> O.help "Show version"

-------------------------------------------------------------------------------
-- Tracer adjustment
-------------------------------------------------------------------------------

adjustTracer :: Applicative m => Verbosity -> Tracer m (Trace Tr) -> Tracer m (Trace Tr)
adjustTracer (Verbosity verbosity) tracer0 = Tracer $ \cs t -> case t of
    TraceApp tr
        | verbosity >= trLevel tr
        ->traceWithCallStack tracer0 cs t

        | otherwise
        -> pure ()

    _ -> traceWithCallStack tracer0 cs t
  where
    trLevel TraceSummary {}   = minBound
    trLevel TraceComponent {} = 0
    trLevel TracePhase1 {}    = 0
    trLevel TracePhase2 {}    = 0
    trLevel TraceGHCi {}      = 1
    trLevel TraceGHCiInput {} = 1

-------------------------------------------------------------------------------
-- Checks
-------------------------------------------------------------------------------

checkGhcVersion :: TracerPeu r w -> GhcInfo -> Plan.PlanJson -> Peu r ()
checkGhcVersion tracer ghcInfo plan
    | ghcId == planId = return ()
    | otherwise = die tracer $ unwords
        [ ghcPath ghcInfo
        , "(" ++ prettyShow ghcId ++ ")"
        , "and plan compiler version"
        , "(" ++ prettyShow planId ++ ")"
        , "are not the same"
        ]
  where
    ghcId = PackageIdentifier "ghc" (ghcVersion ghcInfo)
    planId = toCabal (Plan.pjCompilerId plan)

-------------------------------------------------------------------------------
-- With plan.json
-------------------------------------------------------------------------------

testComponent
    :: TracerPeu r Tr
    -> TracerPeu r Tr
    -> (DynOpts -> DynOpts)
    -> GhcInfo
    -> Path Absolute -- ^ buildDir
    -> Cabal.Config Identity
    -> Plan.PlanJson
    -> [(String, String)]
    -> Package
    -> Plan.Unit
    -> Plan.CompName
    -> Plan.CompInfo
    -> Peu r Summary
testComponent tracer0 tracerTop dynOptsCli ghcInfo buildDir cabalCfg plan env pkg unit cn ci = do
    case cn of
        Plan.CompNameLib -> do
            traceApp tracerTop $ TraceComponent (C.packageId (pkgGpd pkg)) cn

            lib0 <- maybe (die tracerTop "no library component in GPD") return
                $ C.condLibrary $ pkgGpd pkg
            aux lib0

        Plan.CompNameSubLib ln -> do
            traceApp tracerTop $ TraceComponent (C.packageId (pkgGpd pkg)) cn
            let qn = C.mkUnqualComponentName (T.unpack ln)

            lib0 <- maybe (die tracerTop $ "no sublibrary component in GPD" <> prettyShow qn) return
                $ Map.lookup qn sublibs
            aux lib0

        -- Skip other components
        _ -> return mempty
  where
    mcabalVer :: Maybe Version
    mcabalVer = Just $ toCabal $ Plan.pjCabalVersion plan

    sublibs :: Map C.UnqualComponentName (C.CondTree C.ConfVar [C.Dependency] C.Library)
    sublibs = Map.fromList (C.condSubLibraries $ pkgGpd pkg)

    aux lib0 = do
        -- "configure"
        let (_, lib) = simplifyCondTree ghcInfo (Map.mapKeys toCabal $ Plan.uFlags unit) lib0
        let bi = C.libBuildInfo lib
        let cppEnabled = Ext.EnableExtension Ext.CPP `elem` C.defaultExtensions bi

        -- append options in .cabal file
        dynOptsBi <- dynOptsFromBuildInfo tracerTop bi
        let dynOpts = dynOptsCli (dynOptsBi defaultDynOpts)

        -- Once dynOpts is read we can read adjust verbosity of our tracer
        let tracer = adjustTracer (optVerbosity dynOpts) tracer0

        -- find extra units
        extraUnitIds <- findExtraPackages tracer plan $ Set.toList $ propPkgs dynOpts <> optExtraPkgs dynOpts

        -- find library module paths
        modulePaths <- findModules
            tracer
            (pkgDir pkg)
            (C.hsSourceDirs bi)
            (C.exposedModules lib)

        -- direct package dependencies
        let pkgIds :: [PackageIdentifier]
            pkgIds =
                [ toCabal $ Plan.uPId unit'
                | unitId <- toList (Plan.ciLibDeps ci)
                , Just unit' <- return $ Plan.pjUnits plan ^? ix unitId
                ]

        let unitIds :: [UnitId]
            unitIds = ordNub $
                toCabal (Plan.uId unit) :
                map toCabal (toList (Plan.ciLibDeps ci)) ++
                extraUnitIds

        -- cpp include dirs
        cppDirs <- traverse makeAbsolute (optCppIncludeDirs dynOpts)

        -- first phase: read modules and extract the comments
        let pkgVer = C.packageVersion (pkgGpd pkg)
        modules <- for modulePaths $ \(modname, modpath) ->
            phase1 tracer ghcInfo (Just buildDir) (C.packageName (pkgGpd pkg)) pkgVer (pkgDir pkg) cppEnabled cppDirs pkgIds bi modname modpath

        -- extract doctests from the modules.
        let parsed :: [Module [Located DocTest]]
            parsed = fmap4 (doctestStripComments (optStripComs dynOpts))
                $ parseModules modules where

        validated <- validate tracer parsed
        if optPhase dynOpts > Phase1
        then do
            phase2 tracer dynOpts unitIds ghcInfo mcabalVer (Just buildDir) cabalCfg (pkgDir pkg) env validated
        else
            return $ foldMap skipModule parsed

-------------------------------------------------------------------------------
-- Test component without
-------------------------------------------------------------------------------

testComponentNo
    :: forall r. TracerPeu r Tr
    -> TracerPeu r Tr
    -> (DynOpts -> DynOpts)
    -> GhcInfo
    -> Cabal.Config Identity
    -> Map UnitId IPI.InstalledPackageInfo
    -> Package
    -> Peu r Summary
testComponentNo tracer0 tracerTop dynOptsCli ghcInfo cabalCfg dbG pkg = do
    traceApp tracerTop $ TraceComponent (C.packageId (pkgGpd pkg)) Plan.CompNameLib

    -- use default values for flags
    let flags :: Map C.FlagName Bool
        flags = Map.fromList
            [ (C.flagName flag, C.flagDefault flag)
            | flag <- C.genPackageFlags (pkgGpd pkg)
            ]

    -- "configure"
    lib0 <- maybe (die tracerTop "no library component in GPD") return
        $ C.condLibrary $ pkgGpd pkg
    let (_, lib) = simplifyCondTree ghcInfo flags lib0
    let bi = C.libBuildInfo lib
    let cppEnabled = Ext.EnableExtension Ext.CPP `elem` C.defaultExtensions bi

    -- append options in .cabal file
    dynOptsBi <- dynOptsFromBuildInfo tracerTop bi
    let dynOpts = dynOptsCli (dynOptsBi defaultDynOpts)

    -- Once dynOpts is read we can read adjust verbosity of our tracer
    let tracer = adjustTracer (optVerbosity dynOpts) tracer0

    let findUnit :: Library -> Peu r (UnitId, PackageIdentifier, LibraryName)
        findUnit lib'@(Library pn ln) = do
            let units =
                    [ (unitId, IPI.sourcePackageId ipi, ln)
                    | (unitId, ipi) <- itoList dbG
                    , C.packageName (IPI.sourcePackageId ipi) == pn
                    , IPI.sourceLibName ipi == ln
                    ]

            case units of
                [u] -> return u
                []  -> die tracer $ "Cannot find unit for " ++ prettyShow lib'
                _   -> die tracer $ "Found multiple units for " ++ prettyShow lib' ++ ": " ++
                    unwords (map (prettyShow . fstOf3) units)

    -- we don't have install plan, so we look for packages in IPI
    depends <- fmap concat $ for (C.targetBuildDepends bi) $ \dep -> traverse findUnit (toList (depLib dep))
    thisUnitId <- findUnit (Library (C.packageName (pkgGpd pkg)) LMainLibName) -- TODO: libraryname
    extraUnitIds <- traverse findUnit $ Set.toList $ propPkgs dynOpts <> optExtraPkgs dynOpts

    let pkgIds :: [PackageIdentifier]
        pkgIds = map sndOf3 depends

    let unitIds :: [UnitId]
        unitIds = ordNub $ map fstOf3 $
            thisUnitId : depends ++ extraUnitIds

    -- find library module paths
    modulePaths <- findModules
        tracer
        (pkgDir pkg)
        (C.hsSourceDirs bi)
        (C.exposedModules lib)

    -- cpp include dirs
    cppDirs <- traverse makeAbsolute (optCppIncludeDirs dynOpts)

    -- first phase: read modules and extract the comments
    let pkgVer = C.packageVersion (pkgGpd pkg)
    modules <- for modulePaths $ \(modname, modpath) ->
        phase1 tracer ghcInfo Nothing (C.packageName (pkgGpd pkg)) pkgVer (pkgDir pkg) cppEnabled cppDirs pkgIds bi modname modpath

    -- extract doctests from the modules.
    let parsed :: [Module [Located DocTest]]
        parsed = fmap4 (doctestStripComments (optStripComs dynOpts))
            $ parseModules modules where

    validated <- validate tracer parsed

    -- without plan.json we cannot know which cabal-install is used.
    -- assume latest.
    let mcabalVer = Nothing

    if optPhase dynOpts > Phase1
    then do
        -- Note: we don't pass additional environment
        -- For non-cabal-plan setup we simply don't support data-files.
        phase2 tracer dynOpts unitIds ghcInfo mcabalVer Nothing cabalCfg (pkgDir pkg) [] validated
    else
        return $ foldMap skipModule parsed

-------------------------------------------------------------------------------
-- Extra packages
-------------------------------------------------------------------------------

findExtraPackages
    :: TracerPeu r Tr
    -> Plan.PlanJson
    -> [Library]
    -> Peu r [UnitId]
findExtraPackages tracer plan = traverse $ \lib@(Library pn ln) -> do
    let units =
          [ toCabal uid
          | (uid, unit) <- itoList (Plan.pjUnits plan)
          , let PackageIdentifier pn' _ = toCabal (Plan.uPId unit)
          , pn == pn'
          , libNameToCompName ln `Map.member` Plan.uComps unit
          ]

    case units of
        [u] -> return u
        []  -> die tracer $ "Cannot find unit for " ++ prettyShow lib
        _   -> die tracer $ "Found multiple units for " ++ prettyShow lib ++ ": " ++
            unwords (map prettyShow units)

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

propPkgs :: DynOpts -> Set Library
propPkgs dynOpts = case optProperties dynOpts of
    SkipProperties  -> mempty
    CheckProperties -> Set.singleton (Library (mkPackageName "QuickCheck") LMainLibName)

manglePackageName :: C.PackageName -> String
manglePackageName = map fixchar . prettyShow where
    fixchar :: Char -> Char
    fixchar '-' = '_'
    fixchar c   = c

-- | Return name per module.
findModules
    :: TracerPeu r Tr
    -> Path Absolute                              -- ^ package root directory
    -> [C.SymbolicPath C.PackageDir C.SourceDir]  -- ^ @hs-source-dirs
    -> [C.ModuleName]
    -> Peu r [(C.ModuleName, Path Absolute)]
findModules tracer pkgRoot srcDirs' modules = do
    let srcDirs
            | null srcDirs' = [C.sameDirectory]
            | otherwise     = srcDirs'

    fmap concat $ for modules $ \modname -> do
        matches <- fmap concat $ for srcDirs $ \srcDir ->
            globDir1 (C.getSymbolicPath srcDir FP.</> C.toFilePath modname FP.-<.> ".hs") pkgRoot

        case matches of
            [m] -> return [(modname, m)]
            [] -> do
                putWarning tracer WMissingModuleFile $
                    "No .hs file for " ++ prettyShow modname
                return []
            (m:_) -> do
                putWarning tracer WMultipleModuleFiles $
                    "Multiple .hs files for " ++ prettyShow modname ++ ": " ++
                    unwords (map toFilePath matches)

                return [(modname, m)]

simplifyCondTree
    :: (Semigroup a, Semigroup d)
    => GhcInfo
    -> Map C.FlagName Bool
    -> C.CondTree C.ConfVar d a
    -> (d, a)
simplifyCondTree ghcInfo flags = C.simplifyCondTree $ \cv -> Right $ case cv of
    C.OS os         -> os == C.buildOS
    C.Arch arch     -> arch == C.buildArch
    C.Impl c vr     -> c == C.GHC && C.withinRange (ghcVersion ghcInfo) vr
    C.PackageFlag n -> Map.findWithDefault False n flags

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

doctestStripComments :: StripComments -> DocTest -> DocTest
doctestStripComments StripComments    (Example expr res) = Example (dropComments expr) (map f res) where
    f (ExpectedLine xs) = ExpectedLine (fmap g xs)
    f WildCardLine      = WildCardLine
    g (LineChunk s)     = LineChunk (dropComments s)
    g WildCardChunk     = WildCardChunk
doctestStripComments StripComments     (Property expr) = Property (dropComments expr)
doctestStripComments DontStripComments dt = dt

fmap4
    :: (Functor f1, Functor f2, Functor f3, Functor f4)
    => (a -> b) -> f1 (f2 (f3 (f4 a))) -> f1 (f2 (f3 (f4 b)))
fmap4 f = fmap (fmap (fmap (fmap f)))

-------------------------------------------------------------------------------
-- No hanging :{
-------------------------------------------------------------------------------

validate
    :: TracerPeu r Tr
    -> [Module [Located DocTest]] -> Peu r [Module [Located DocTest]]
validate tracer parsed = for parsed $ \m -> do
    case traverse (traverse filterExpressionD) m of
        Right xs -> do
            return xs
        Left (L l err) -> do
            die tracer $ prettyShow (moduleName m) ++ " " ++ prettyPos l ++ " " ++ err


filterExpressionD :: Located DocTest -> Either (Located String) (Located DocTest)
filterExpressionD orig@(L l (Example s _)) =
    orig <$ first (L l) (filterExpression s)
filterExpressionD orig@(L l (Property s)) =
    orig <$ first (L l) (filterExpression s)

-- | Simple check that :{ and :} are there.
-- From doctest.
--
-- Otherwise
--
-- @
-- ghci> :{
-- x = 1
--
-- y = 2
--
-- x + y
-- :}
-- @
--
-- like things cause timeout.
--
filterExpression :: String -> Either String String
filterExpression e =
  case lines e of
    []     -> Right e
    (l:ls) -> if firstLine == ":{" && lastLine /= ":}" then fail_ else Right e
      where
        firstLine = strip l
        lastLine  = strip $ last (l:|ls)
        fail_ = Left "unterminated multiline command"
  where
    strip :: String -> String
    strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

depLib :: C.Dependency -> NonEmpty Library
depLib (C.Dependency pn _vr lns) = Library pn <$> NES.toNonEmpty lns

libNameToCompName :: LibraryName -> Plan.CompName
libNameToCompName LMainLibName = Plan.CompNameLib
libNameToCompName (LSubLibName n) = Plan.CompNameSubLib (T.pack (C.unUnqualComponentName  n))
