{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Copyright: Oleg Grenrus
-- License: GPL-3.0-or-later
module CabalEnv.Main (main) where

import Peura

import Control.Applicative           ((<**>))
import Data.List                     (lines, sort, stripPrefix)
import Data.Version                  (showVersion)
import Distribution.Parsec           (eitherParsec)
import Distribution.Types.Dependency (Dependency (..))
import Distribution.Version          (intersectVersionRanges)
import System.FilePath.Glob          (glob)

import qualified Crypto.Hash.SHA256             as SHA256
import qualified Data.ByteString.Base16         as Base16
import qualified Data.Map.Strict                as Map
import qualified Data.Set                       as Set
import qualified Data.Text                      as T
import qualified Distribution.Pretty            as C
import qualified Distribution.Types.LibraryName as C
import qualified Distribution.Version           as C
import qualified Options.Applicative            as O
import qualified System.FilePath                as FP

import qualified Cabal.Config as Config
import qualified Cabal.Plan   as Plan

import CabalEnv.Environment
import CabalEnv.Warning

import Paths_cabal_env (version)

main :: IO ()
main = runPeu () $ \tracer -> do
    opts <- liftIO $ O.execParser optsP'
    ghcInfo <- getGhcInfo tracer (optCompiler opts)

    case optAction opts of
        ActionInstall -> installAction tracer opts ghcInfo
        ActionShow    -> showAction    tracer opts ghcInfo
        ActionList    -> listAction    tracer opts ghcInfo
        ActionHide    -> hideAction    tracer opts ghcInfo
  where
    optsP' = O.info (optsP <**> O.helper <**> versionP) $ mconcat
        [ O.fullDesc
        , O.progDesc "Manage package-enviroments"
        , O.header "cabal-env - a better cabal-install install --lib"
        ]

    versionP = O.infoOption (showVersion version)
        $ O.long "version" <> O.help "Show version"

-------------------------------------------------------------------------------
-- List
-------------------------------------------------------------------------------

listAction :: TracerPeu r W -> Opts -> GhcInfo -> Peu r ()
listAction tracer Opts {..} ghcInfo = do
    let envDir = ghcEnvDir ghcInfo
    when optVerbose $ putInfo tracer $ "Environments available in " ++ toFilePath envDir
    fs <- listDirectory envDir
    for_ (sort fs) $ \f ->
        putInfo tracer $ toUnrootedFilePath f ++ if optVerbose then " " ++ toFilePath (envDir </> f) else ""

-------------------------------------------------------------------------------
-- Show
-------------------------------------------------------------------------------

showAction :: TracerPeu r W -> Opts -> GhcInfo -> Peu r ()
showAction tracer Opts {..} ghcInfo = do
    let envPath = ghcEnvDir ghcInfo </> fromUnrootedFilePath optEnvName
    withEnvironment tracer envPath $ \env -> do
        when optVerbose $ putInfo tracer $ "Packages in " ++ optEnvName ++ " environment " ++ toFilePath envPath

        let depsPkgNames :: Set PackageName
            depsPkgNames = Set.fromList
                [ pn
                | Dependency pn _ _ <- envPackages env
                ]

        -- we don't need to consider reachableUnits,
        -- as we are interested only in package identifiers
        for_ (planPkgIds $ envPlan env) $ \pkgId@(PackageIdentifier pkgName _) -> do
            let shown = (envTransitive env || Set.member pkgName depsPkgNames || pkgName == mkPackageName "base")
                    && pkgName `notElem` envHidden env

            output tracer $ prettyShow pkgId ++ if shown then "" else " (hidden)"

-------------------------------------------------------------------------------
-- Install
-------------------------------------------------------------------------------

installAction :: TracerPeu r W -> Opts -> GhcInfo -> Peu r ()
installAction tracer opts@Opts {..} ghcInfo = unless (null optDeps) $ do
    let envDir = ghcEnvDir ghcInfo
    putDebug tracer $ "GHC environment directory: " ++ toFilePath envDir

    createDirectoryIfMissing True envDir
    withEnvironmentMaybe tracer (envDir </> fromUnrootedFilePath optEnvName) $ \menv ->
        case menv of
            Nothing  -> installActionDo tracer opts
                (Environment optDeps [] (fromMaybe defaultTransitive optTransitive) Map.empty ())
                ghcInfo
            Just env -> do
                let (planBS, plan) = envPlan env
                let oldEnv :: Environment ()
                    oldEnv = Environment
                        { envPackages   = nubDeps $ optDeps ++ envPackages env
                        , envTransitive = fromMaybe (envTransitive env) optTransitive
                        , envHidden     = envHidden env
                        , envLocalPkgs  = envLocalPkgs env
                        , envPlan       = ()
                        }
                if all (inThePlan plan) (envPackages oldEnv)
                then do
                    putDebug tracer "Everything in plan, regenerating environment file"
                    generateEnvironment tracer opts oldEnv ghcInfo plan planBS
                else installActionDo tracer opts oldEnv ghcInfo

inThePlan :: Plan.PlanJson -> Dependency -> Bool
inThePlan plan (Dependency pn range _) = case Map.lookup pn pkgIds of
    Nothing  -> False
    Just ver -> ver `C.withinRange` range
  where
    pkgIds = planPkgIdsMap plan

-------------------------------------------------------------------------------
-- Local packages
-------------------------------------------------------------------------------

getLocalPackages :: TracerPeu r W -> Opts -> Peu r (Map PackageName (Path Absolute))
getLocalPackages tracer Opts {..}
    | optLocal = do
        putInfo tracer "SDisting local packages"
        cwd <- getCurrentDirectory

        withSystemTempDirectory "cabal-env" $ \dir -> do
            -- local packages are sdist'd, and we'll copy them to
            -- a directory ~/.cabal/local-pkgs if ~/.cabal/store is storeDir,
            -- i.e. next to store.
            --
            cblCfg  <- liftIO Config.readConfig
            storeDir <- makeAbsoluteFilePath $ runIdentity $ Config.cfgStoreDir cblCfg
            let localPkgDir = takeDirectory storeDir </> fromUnrootedFilePath "local-pkgs"
            createDirectoryIfMissing True localPkgDir

            -- 1. we sdist all, using empty --builddir
            _ <- runProcessCheck tracer cwd "cabal"
                [ "sdist"
                , "--builddir=" ++ toFilePath (dir </> fromUnrootedFilePath "dist-newstyle")
                , "all"
                ]

            -- 2. then we look for all packages in that directory.
            res  <- liftIO $ glob (toFilePath dir ++ "/dist-newstyle/sdist/*.tar.gz")
            tarballs <- elaborateSdistLocation tracer res

            -- 3. We copy files to ~/.cabal/local-pkgs
            -- Naming them as pkgid-contenthash.tar.gz
            fmap Map.fromList $ for (Map.toList tarballs) $ \(pkgId@(PackageIdentifier pkgName _), path) -> do
                hash <- calculateHash1 path
                let newPath = localPkgDir </> fromUnrootedFilePath
                        (prettyShow pkgId ++ "-" ++ hash ++ ".tar.gz")

                copyFile path newPath

                return (pkgName, newPath)

    | otherwise =
        return Map.empty

elaborateSdistLocation :: forall r. TracerPeu r W -> [FilePath] -> Peu r (Map PackageIdentifier (Path Absolute))
elaborateSdistLocation tracer = fmap Map.fromList . traverse go where
    go :: FilePath -> Peu r (PackageIdentifier, Path Absolute)
    go fp = do
        fp' <- makeAbsoluteFilePath fp
        let fn = toUnrootedFilePath (takeFileName fp')
        pid <- elaboratePkgId fn
        return (pid, fp')

    elaboratePkgId :: String -> Peu r PackageIdentifier
    elaboratePkgId str = case stripSuffix ".tar.gz" str of
        Nothing -> do
            putError tracer $ "tarball path doesn't end with .tar.gz -- " ++ str
            exitFailure

        Just pfx -> case eitherParsec pfx of
            Right pkgId -> return pkgId
            Left  err   -> die tracer err

    stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
    stripSuffix sfx str = fmap reverse (stripPrefix (reverse sfx) (reverse str))

calculateHash1 :: Path Absolute -> Peu r String
calculateHash1 fp = do
    content <- readLazyByteString fp
    return $ fromUTF8BS $ Base16.encode $ SHA256.hashlazy content

-------------------------------------------------------------------------------
--  Hide
-------------------------------------------------------------------------------

hideAction :: TracerPeu r W -> Opts -> GhcInfo -> Peu r ()
hideAction tracer opts@Opts {..} ghcInfo = unless (null optDeps) $ do
    let envDir = ghcEnvDir ghcInfo
    putDebug tracer $ "GHC environment directory: " ++ toFilePath envDir

    createDirectoryIfMissing True envDir
    withEnvironmentMaybe tracer (envDir </> fromUnrootedFilePath optEnvName) $ \menv ->
        case menv of
            Nothing  -> return ()
            Just env -> do
                let (planBS, plan) = envPlan env
                let oldEnv :: Environment ()
                    oldEnv = Environment
                        { envPackages   = nubDeps $ envPackages env
                        , envTransitive = fromMaybe (envTransitive env) optTransitive
                        , envHidden     = sortNub $ envHidden env ++ [ pn | Dependency pn _ _ <- optDeps ]
                        , envLocalPkgs  = envLocalPkgs env
                        , envPlan       = ()
                        }

                if all (inThePlan plan) (envPackages oldEnv)
                then do
                    putDebug tracer "Everything in plan, regenerating environment file"
                    generateEnvironment tracer opts oldEnv ghcInfo plan planBS
                else installActionDo tracer opts oldEnv ghcInfo

-------------------------------------------------------------------------------
-- Environment generation
-------------------------------------------------------------------------------

installActionDo
    :: TracerPeu r W
    -> Opts
    -> Environment () -- ^ old environment
    -> GhcInfo
    -> Peu r ()
installActionDo tracer opts@Opts {..} oldEnv ghcInfo = do
    -- new environment with added local packages
    locals <- getLocalPackages tracer opts
    let env = oldEnv { envLocalPkgs = Map.union locals (envLocalPkgs oldEnv) }

    let planInput :: PlanInput
        planInput = emptyPlanInput
            { piLibraries = Map.fromListWith intersectVersionRanges $
                [ (pn, vr)
                | Dependency pn vr _ <- envPackages env
                ]
            , piDryRun   = optDryRun
            , piCompiler = Just optCompiler
            , piTarballs = Map.elems $ envLocalPkgs env
            }

    res <- ephemeralPlanJson' tracer planInput
    case res of
        Nothing -> do
            putError tracer "ERROR: cabal v2-build failed"
            putError tracer "This is might be due inconsistent dependencies (delete package env file, or try -a) or something else"
            exitFailure

        Just _ | optDryRun ->
            return ()

        Just (planBS, plan) ->
                generateEnvironment tracer opts env ghcInfo plan planBS

generateEnvironment
    :: TracerPeu r W
    -> Opts
    -> Environment ()
    -> GhcInfo
    -> Plan.PlanJson
    -> ByteString
    -> Peu r ()
generateEnvironment tracer Opts {..} env ghcInfo plan planBS = do
    let units = Plan.pjUnits plan

    -- https://github.com/haskell/cabal/issues/6407
    -- collect units we can reach from fake-package
    let runits = reachableUnits plan

    -- Construct environment

    let environment = env
          { envPlan = planBS
          }

    config <- liftIO Config.readConfig

    let depsPkgNames :: Set PackageName
        depsPkgNames = Set.fromList
            [ pn
            | Dependency pn _ _ <- envPackages env
            ]

    let envFileLines :: [String]
        envFileLines =
            [ "-- This is GHC environment file written by cabal-env"
            , "--"
            , "clear-package-db"
            , "global-package-db"
            , "package-db " ++ runIdentity (Config.cfgStoreDir config)
                FP.</> "ghc-" ++ C.prettyShow (ghcVersion ghcInfo)
                FP.</>"package.db"
            ] ++
            [ "package-id " ++ T.unpack uidText
            | (uid@(Plan.UnitId uidText), unit) <- Map.toList units
            , uid `Set.member` runits
            , Plan.uType unit `notElem` [ Plan.UnitTypeLocal, Plan.UnitTypeInplace ]

            -- internal libraries are all non-visible now.
            , (cName, _ci) <- Map.toList (Plan.uComps unit)
            , cName == Plan.CompNameLib

            , let PackageIdentifier pkgName _ = toCabal (Plan.uPId unit)
            , envTransitive env || Set.member pkgName depsPkgNames || pkgName == mkPackageName "base"
            , pkgName `notElem` envHidden env
            ] ++
            [ "-- cabal-env " ++ x
            | x <- lines $ encodeEnvironment environment
            ]

    -- (over)write ghc environment file
    let newEnvFileContents  :: String
        newEnvFileContents = unlines envFileLines

    putDebug tracer "writing environment file"
    when optVerbose $ outputErr tracer newEnvFileContents

    writeByteString (ghcEnvDir ghcInfo </> fromUnrootedFilePath optEnvName) (toUTF8BS newEnvFileContents)

-------------------------------------------------------------------------------
-- BFS
-------------------------------------------------------------------------------

bfs :: forall a. Ord a => Set a -> (a -> Set a) -> Set a
bfs xs edge = go Set.empty xs where
    go :: Set a -> Set a -> Set a
    go acc next
        | Set.null next = acc
        | otherwise     = go acc' next'
      where
        acc'  = acc <> next
        next' = foldMap edge next `Set.difference` acc'

-------------------------------------------------------------------------------
-- Dependency extras
-------------------------------------------------------------------------------

nubDeps :: [Dependency] -> [Dependency]
nubDeps deps =
    [ Dependency pn (C.simplifyVersionRange vr) (Set.singleton C.LMainLibName)
    | (pn, vr) <- Map.toList m
    ]
  where
    m = Map.fromListWith intersectVersionRanges
        [ (pn, vr)
        | Dependency pn vr _ <- deps
        ]

sortNub :: Ord a => [a] -> [a]
sortNub = Set.toList . Set.fromList

-------------------------------------------------------------------------------
-- Plan extras
-------------------------------------------------------------------------------

planPkgIds :: Plan.PlanJson -> Set PackageIdentifier
planPkgIds plan = Set.fromList
    [ toCabal $ Plan.uPId unit
    | unit <- Map.elems (Plan.pjUnits plan)
    ]

planPkgIdsMap :: Plan.PlanJson -> Map PackageName Version
planPkgIdsMap plan = Map.fromList
    [ case toCabal $ Plan.uPId unit of
        PackageIdentifier pn ver -> (pn, ver)
    | unit <- Map.elems (Plan.pjUnits plan)
    ]

reachableUnits :: Plan.PlanJson -> Set Plan.UnitId
reachableUnits plan = bfs fakePackageUnitId $ \uid -> Set.unions
    [ Plan.ciLibDeps ci
    | unit <- toList (Map.lookup uid units)
    , ci <- Map.elems (Plan.uComps unit)
    ]
  where
    units = Plan.pjUnits plan

    fakePackageUnitId :: Set Plan.UnitId
    fakePackageUnitId = Set.fromList
        [ uid
        | (uid, unit) <- Map.toList units
        , Plan.uPId unit == Plan.PkgId (Plan.PkgName "fake-package") (Plan.Ver [0])
        ]

-------------------------------------------------------------------------------
-- Defaults
-------------------------------------------------------------------------------

defaultTransitive :: Bool
defaultTransitive = False

-------------------------------------------------------------------------------
-- Options parser
-------------------------------------------------------------------------------

-- TODO: commands to list package environments, their contents, delete, copy.
-- TODO: special . name for "package environment in this directory"
data Opts = Opts
    { optCompiler   :: FilePath
    , optEnvName    :: String
    , optAnyVer     :: Bool -- Todo unused
    , optVerbose    :: Bool
    , optDryRun     :: Bool
    , optTransitive :: Maybe Bool
    , optDeps       :: [Dependency]
    , optLocal      :: Bool
    , optAction     :: Action
    }

data Action
    = ActionShow     -- ^ show package environment contents
    | ActionList     -- ^ list package environments
    | ActionInstall
    | ActionHide     -- ^ hide a package

optsP :: O.Parser Opts
optsP = Opts
    <$> O.strOption (O.short 'w' <> O.long "with-compiler" <> O.value "ghc" <> O.showDefault <> O.help "Specify compiler to use")
    <*> O.strOption (O.short 'n' <> O.long "name" <> O.value "default" <> O.showDefault <> O.help "Environment name")
    <*> O.switch (O.short 'a' <> O.long "any" <> O.help "Allow any version of existing packages")
    <*> O.switch (O.short 'v' <> O.long "verbose" <> O.help "Print stuff...")
    <*> O.switch (O.short 'd' <> O.long "dry-run" <> O.help "Dry run, don't install anything")
    <*> optional transitiveP
    <*> many (O.argument (O.eitherReader eitherParsec) (O.metavar "PKG..." <> O.help "packages (with possible bounds)"))
    <*> O.switch (O.long "local" <> O.help "Include local packages")
    -- behaviour flags
    <*> actionP
  where
    transitiveP =
        O.flag' True (O.short 't' <> O.long "transitive" <> O.help "Expose transitive dependencies")
        <|>
        O.flag' False (O.long "no-transitive" <> O.help "Don't expose transitive dependencies")

actionP :: O.Parser Action
actionP = installP <|> showP <|> listP <|> hideP <|> pure ActionInstall where
    installP = O.flag' ActionInstall (O.short 'i' <> O.long "install" <> O.help "Install / add packages to the environment")
    showP = O.flag' ActionShow (O.short 's' <> O.long "show" <> O.help "Shows the contents of the environment")
    listP = O.flag' ActionList (O.short 'l' <> O.long "list" <> O.help "List package environments")
    hideP = O.flag' ActionHide (O.long "hide" <> O.help "Hide packages from an environment")
