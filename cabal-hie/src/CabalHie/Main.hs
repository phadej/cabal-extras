{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
-- |
-- Copyright: Oleg Grenrus
-- License: GPL-2.0-or-later
module CabalHie.Main (main) where

import Peura
import Prelude ()

import Control.Applicative ((<**>))
import Data.Version        (showVersion)

import qualified Cabal.Plan               as Plan
import qualified Data.Aeson               as A
import qualified Data.Map.Strict          as Map
import qualified Data.Text                as T
import qualified Data.YAML.Aeson          as YAML.Aeson
import qualified Distribution.Compat.Lens as L
import qualified Distribution.Pretty      as C
import qualified Options.Applicative      as O
import qualified System.FilePath          as FP

import qualified Distribution.Compiler                        as C
import qualified Distribution.Package                         as C
import qualified Distribution.PackageDescription.Parsec       as C
import qualified Distribution.System                          as C
import qualified Distribution.Types.CondTree                  as C
import qualified Distribution.Types.ConfVar                   as C
import qualified Distribution.Types.Flag                      as C
import qualified Distribution.Types.GenericPackageDescription as C
import qualified Distribution.Version                         as C

import qualified Distribution.Types.BuildInfo.Lens as CL

import Paths_cabal_hie (version)

main :: IO ()
main = do
    opts <- O.execParser optsP'
    tracer <- makeTracerPeu (optTracer opts defaultTracerOptions)
    runPeu tracer () $ generateHie tracer opts
  where
    optsP' = O.info (optsP <**> O.helper <**> versionP) $ mconcat
        [ O.fullDesc
        , O.progDesc "Check project or package deps"
        , O.header "cabal-hie"
        ]

    versionP = O.infoOption (showVersion version)
        $ O.long "version" <> O.help "Show version"

-------------------------------------------------------------------------------
-- Options parser
-------------------------------------------------------------------------------

data Opts = Opts
    { optBuildDir :: FsPath
    , optCompiler  :: FilePath
    , optTracer   :: TracerOptions Void -> TracerOptions Void
    }

optsP :: O.Parser Opts
optsP = pure Opts
    <*> O.option fspath (O.long "builddir" <> O.value (fromFilePath "dist-newstyle") <> O.metavar "BUILDDIR")
    <*> O.strOption (O.short 'w' <> O.long "with-compiler" <> O.metavar "PATH" <> O.value "ghc" <> O.showDefault <> O.help "Specify compiler to use")
    <*> tracerOptionsParser

fspath :: O.ReadM FsPath
fspath = O.eitherReader $ return . fromFilePath

-------------------------------------------------------------------------------
-- Generator
-------------------------------------------------------------------------------

generateHie :: forall r. TracerPeu r Void -> Opts -> Peu r ()
generateHie tracer opts = do
    -- gather info
    cwd     <- getCurrentDirectory
    ghcInfo <- getGhcInfo tracer (optCompiler opts)

    buildDir <- makeAbsolute (optBuildDir opts)
    plan <- liftIO $ Plan.findAndDecodePlanJson $ Plan.InBuildDir $ toFilePath buildDir

    -- checks
    checkGhcVersion tracer ghcInfo plan

    -- Elaborate plan by reading local package definitions
    pkgs0 <- readLocalCabalFiles tracer plan

    allDirs <- fmap concat $ for pkgs0 $ \pkg -> do
        let gpd :: C.GenericPackageDescription
            gpd = pkgGpd pkg

        -- convert package directory to absolute directory
        let absDir :: FilePath -> Path Absolute
            absDir fp = pkgDir pkg </> fromUnrootedFilePath fp

        -- componetns for this package, keyed by selector
        let components :: Map Text (Plan.Unit, Plan.CompName, Plan.CompInfo)
            components = Map.fromList
                [ (pn' <> ":" <> Plan.dispCompNameTarget pn cn, (unit, cn, ci))
                | unit <- pkgUnits pkg
                , let Plan.PkgId pn _ = Plan.uPId unit
                , let Plan.PkgName pn' = pn
                , (cn, ci) <- Map.toList $ Plan.uComps unit
                ]

        -- next we collect (directory, selector) pairs
        let componentPaths :: (Semigroup a, CL.HasBuildInfo a) => String -> C.CondTree C.ConfVar [d] a -> Peu r [(Path Absolute, Text)]
            componentPaths selector comp0  = do
                (unit, _cn, _ci) <- maybe (die tracer $ "Cannot find unit for " ++ selector) return $
                    Map.lookup (T.pack selector) components

                let (_, comp) = simplifyCondTree ghcInfo (Map.mapKeys toCabal $ Plan.uFlags unit) comp0

                return $ map (\dir -> (absDir dir, T.pack selector)) (L.toListOf (CL.hsSourceDirs . traverse) comp)

        -- libraries
        libDirs <- for (toList $ C.condLibrary gpd) $ \comp0 -> do
            let selector = prettyShow (C.packageName gpd) <> ":lib:" <> C.prettyShow (C.packageName gpd)
            componentPaths selector comp0

        -- exe
        exeDirs <- for (C.condExecutables gpd) $ \(name, comp0) -> do
            let selector = prettyShow (C.packageName gpd) <> ":exe:" <> C.prettyShow name
            componentPaths selector comp0

        -- sub libraries
        sublibDirs <- for (C.condSubLibraries gpd) $ \(name, comp0) -> do
            let selector = prettyShow (C.packageName gpd) <> ":lib:" <> C.prettyShow name
            componentPaths selector comp0

        -- tests
        testDirs <- for (C.condTestSuites gpd) $ \(name, comp0) -> do
            let selector = prettyShow (C.packageName gpd) <> ":test:" <> C.prettyShow name
            componentPaths selector comp0

        -- bench
        benchDirs <- for (C.condBenchmarks gpd) $ \(name, comp0) -> do
            let selector = prettyShow (C.packageName gpd) <> ":bench:" <> C.prettyShow name
            componentPaths selector comp0

        let allDirs :: [(Path Absolute, Text)]
            allDirs = concatMap concat [ libDirs, exeDirs, sublibDirs, testDirs, benchDirs ]

        return allDirs

    let allDirs' :: [(FilePath, Text)]
        allDirs' =
            [ (FP.makeRelative (toFilePath cwd) (toFilePath fp), selector)
            | (fp, selector) <- allDirs
            ]

    writeByteString (cwd </> fromUnrootedFilePath "hie.yaml") $ YAML.Aeson.encode1Strict $ A.object
        [ "cradle" A..= A.object [ "cabal" A..=
            [ A.object
                [ "path"      A..= fp
                , "component" A..= selector
                ] 
            | (fp, selector) <- allDirs'
            ]
        ]]

-------------------------------------------------------------------------------
-- hie cradle
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- From cabal-docspec
-------------------------------------------------------------------------------

data Package = Package
    { pkgGpd   :: C.GenericPackageDescription
    , pkgDir   :: Path Absolute
    , pkgUnits :: [Plan.Unit]
    }
  deriving Show

readLocalCabalFiles
    :: TracerPeu r w
    -> Plan.PlanJson
    -> Peu r [Package]
readLocalCabalFiles tracer plan =
    for (itoList units0) $ \(path, units) -> do
        path' <- makeAbsoluteFilePath path
        cabalPath <- globDir1First "*.cabal" path'
        cabalBS <- readByteString cabalPath
        gpd <- maybe (die tracer $ "cannot parse " ++ toFilePath cabalPath) return
            $ C.parseGenericPackageDescriptionMaybe cabalBS

        return Package
            { pkgGpd   = gpd
            , pkgDir   = path'
            , pkgUnits = toList units
            }
  where
    units0 :: Map FilePath (NonEmpty Plan.Unit)
    units0 = group
        [ (path, unit)
        | unit <- toList (Plan.pjUnits plan)
        , Plan.uType unit == Plan.UnitTypeLocal
        , Just (Plan.LocalUnpackedPackage path) <- return (Plan.uPkgSrc unit)
        ]

group :: (Ord a) => [(a,b)] -> Map a (NonEmpty b)
group = Map.fromListWith (<>) . map (fmap pure)

-------------------------------------------------------------------------------
-- cabal-docspec main
-------------------------------------------------------------------------------

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
