{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
-- |
-- Copyright: Oleg Grenrus
-- License: GPL-2.0-or-later
module CabalDeps.Main (main) where

import Peura
import Prelude ()

import Control.Applicative ((<**>))
import Data.Version        (showVersion)

import qualified Cabal.Index              as I
import qualified Cabal.Package            as Pkg
import qualified Cabal.Plan               as P
import qualified Data.Map.Strict          as Map
import qualified Data.Set                 as Set
import qualified Distribution.Compat.Lens as L
import qualified Distribution.Pretty      as C
import qualified Options.Applicative      as O

import qualified Distribution.Package                         as C
import qualified Distribution.Version                         as C
import qualified Distribution.Types.BuildInfo                 as C
import qualified Distribution.Types.BuildInfo.Lens            as L
import qualified Distribution.Types.ComponentName             as C
import qualified Distribution.Types.ExeDependency             as C
import qualified Distribution.Types.CondTree                  as C
import qualified Distribution.Types.Condition                  as C
import qualified Distribution.Types.PackageDescription as C
import qualified Distribution.Types.GenericPackageDescription as C
import qualified Distribution.Types.LibraryName               as C
import qualified Distribution.Types.SetupBuildInfo            as C

import Paths_cabal_deps (version)

main :: IO ()
main = do
    opts <- O.execParser optsP'
    runPeu () $ doDeps opts
  where
    optsP' = O.info (optsP <**> O.helper <**> versionP) $ mconcat
        [ O.fullDesc
        , O.progDesc "Check project or package deps"
        , O.header "cabal-diff"
        ]

    versionP = O.infoOption (showVersion version)
        $ O.long "version" <> O.help "Show version"

-------------------------------------------------------------------------------
-- Options parser
-------------------------------------------------------------------------------

data Opts = Opts
    { optCabal :: [FsPath]
    }

optsP :: O.Parser Opts
optsP = Opts
    <$> many (O.argument (O.eitherReader $ return . fromFilePath) (O.metavar "CABALFILE..." <> O.help "Cabal files"))

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

doDeps :: Opts -> Peu r ()
doDeps opts = do
    putInfo "Reading Hackage metadata"
    meta <- liftIO I.cachedHackageMetadata

    case optCabal opts of
        []     -> doPlanDeps meta
        (x:xs) -> doGpdDeps meta (x :| xs)

-------------------------------------------------------------------------------
-- Check GPD
-------------------------------------------------------------------------------

doGpdDeps :: Map PackageName I.PackageInfo -> NonEmpty FsPath -> Peu r ()
doGpdDeps meta fps = do
    gpds <- for fps $ \fp -> do
        fp' <- makeAbsolute fp
        liftIO $ Pkg.readPackage $ toFilePath fp'

    for_ gpds $ \gpd ->
        checkGpd meta gpd

    -- TODO: use exitCode to indicate

-- TODO: return True or False if fine or not.

checkGpd :: Map PackageName I.PackageInfo -> C.GenericPackageDescription -> Peu r ()
checkGpd meta gpd = do
    let PackageIdentifier packageName _ = C.package (C.packageDescription gpd)

    let comps :: Map C.ComponentName (C.CondTree C.ConfVar () C.BuildInfo)
        comps = Map.fromList $
            [ (C.CLibName C.LMainLibName, buildInfoTree lib)
            | lib <- toList (C.condLibrary gpd)
            ] ++
            [ (C.CLibName $ C.LSubLibName name, buildInfoTree lib)
            | (name, lib) <- C.condSubLibraries gpd
            ] ++
            [ (C.CFLibName name, buildInfoTree flib)
            | (name, flib) <- C.condForeignLibs gpd
            ] ++
            [ (C.CExeName name, buildInfoTree exe)
            | (name, exe) <- C.condExecutables gpd
            ] ++
            [ (C.CTestName name, buildInfoTree t)
            | (name, t) <- C.condTestSuites gpd
            ] ++
            [ (C.CBenchName name, buildInfoTree b)
            | (name, b) <- C.condBenchmarks gpd
            ]

    let setup :: Maybe C.SetupBuildInfo
        setup = C.setupBuildInfo $ C.packageDescription gpd

    for_ setup $ \s ->
        checkDepMap meta (C.prettyShow packageName ++ ":setup") $ DepMap $ Map.fromListWith C.intersectVersionRanges
            [ (pn, vr)
            | C.Dependency pn vr _ <- C.setupDepends s
            ]

    ifor_ comps $ \n c ->
        checkDepMap meta (C.prettyShow packageName ++ ":" ++ C.prettyShow n) (condTreeToDepMap c)

--    ifor_ comps $ \n c -> do
--        output (show n)
--        output (show c)
  where
    buildInfoTree :: L.HasBuildInfo x => C.CondTree w b x -> C.CondTree w () C.BuildInfo
    buildInfoTree = C.mapCondTree (L.view L.buildInfo) (const ()) id

newtype DepMap = DepMap (Map PackageName VersionRange)

instance Semigroup DepMap where
    DepMap a <> DepMap b = DepMap (Map.unionWith C.intersectVersionRanges a b)

instance Monoid DepMap where
    mempty  = DepMap Map.empty
    mappend = (<>)

unionDepMap :: DepMap -> DepMap -> DepMap
unionDepMap (DepMap a) (DepMap b) = DepMap (Map.unionWith C.unionVersionRanges a b)

checkDepMap :: Map PackageName I.PackageInfo -> String -> DepMap -> Peu r ()
checkDepMap meta cname (DepMap depMap) =
    ifor_ depMap $ \pn vr -> case Map.lookup pn meta of
        Nothing -> putWarning WNotOnHackage $ cname ++ " depends on " ++ C.prettyShow pn ++ ", which is not on Hackage"
        Just pi -> case Map.lookupMax (I.piPreferredVersions pi) of
            Nothing       ->
                putWarning WNoPreferredVersions $ C.prettyShow pn ++ " doesn't have preferred versions"
            Just (ver, _)
                | C.withinRange ver vr ->
                    -- putDebug $ C.prettyShow pn ++ " is latest version"
                    return ()
                | lessThanLowerBound ver (vrLowerBound vr) -> putWarning WNotOnHackage $
                    cname ++ " depends on " ++ C.prettyShow pn ++ " " ++ C.prettyShow vr ++ "; latest on Hackage " ++ C.prettyShow ver
                | otherwise -> putWarning WNotLatest $
                    cname ++ " doesn't accept " ++ C.prettyShow (PackageIdentifier pn ver)
                    ++ "; depends " ++ C.prettyShow vr

condTreeToDepMap :: C.CondTree C.ConfVar () L.BuildInfo -> DepMap
condTreeToDepMap = go where
    go :: C.CondTree C.ConfVar x L.BuildInfo -> DepMap
    go (C.CondNode bi _ branches) = fromBi bi <> foldMap branch branches

    branch :: C.CondBranch C.ConfVar x L.BuildInfo -> DepMap
    branch (C.CondBranch cond t f)
        -- when condition is flag, we union dependencies
        | flagCondition cond = unionDepMap f' t'
        | otherwise          = t' <> f'
      where
        t' = go t
        f' = foldMap go f

    fromBi :: C.BuildInfo -> DepMap
    fromBi bi = builddeps <> buildtools where
        builddeps = DepMap $ Map.fromListWith C.intersectVersionRanges
            [ (pn, vr)
            | C.Dependency pn vr _ <- C.targetBuildDepends bi
            ]

        buildtools = DepMap $ Map.fromListWith C.intersectVersionRanges
            [ (pn, vr)
            | C.ExeDependency pn _ vr <- C.buildToolDepends bi
            ]

flagCondition :: C.Condition C.ConfVar -> Bool
flagCondition (C.Var (C.OS _))     = False
flagCondition (C.Var (C.Arch _))   = False
flagCondition (C.Var (C.Flag _))   = True
flagCondition (C.Var (C.Impl _ _)) = False
flagCondition (C.Lit _)            = False
flagCondition (C.CNot a)           = flagCondition a
flagCondition (C.COr a b)          = flagCondition a && flagCondition b
flagCondition (C.CAnd a b)         = flagCondition a && flagCondition b

vrLowerBound :: C.VersionRange -> C.LowerBound
vrLowerBound vr = case C.asVersionIntervals vr of
    (lb, _) : _ -> lb
    _           -> C.LowerBound C.version0 C.InclusiveBound

lessThanLowerBound :: Version -> C.LowerBound -> Bool
lessThanLowerBound v (C.LowerBound v' C.InclusiveBound) = v <  v'
lessThanLowerBound v (C.LowerBound v' C.ExclusiveBound) = v <= v'

-------------------------------------------------------------------------------
-- Check plan
-------------------------------------------------------------------------------

doPlanDeps :: Map PackageName I.PackageInfo -> Peu r ()
doPlanDeps meta = do
    putInfo "Reading plan.json for current project"
    plan <- liftIO $ P.findAndDecodePlanJson (P.ProjectRelativeToDir ".")

    let pkgIds :: Set C.PackageIdentifier
        pkgIds = Set.fromList
            [ toCabal (P.uPId unit)
            | unit <- Map.elems (P.pjUnits plan)
            -- filter global-db packages
            , P.uType unit /= P.UnitTypeBuiltin
            ]

    for_ pkgIds $ \pkgId ->
        checkPlan meta pkgId

    -- TODO: use exitCode to indicate

-- TODO: return True or False if fine or not.
checkPlan
    :: Map PackageName I.PackageInfo
    -> C.PackageIdentifier
    -> Peu r ()
checkPlan meta pid@(C.PackageIdentifier pn ver) =
    case Map.lookup pn meta of
        Nothing -> putWarning WNotOnHackage $ C.prettyShow pn ++ " is not on Hackage"
        Just pi -> case Map.lookupMax (I.piPreferredVersions pi) of
            Nothing        ->
                putWarning WNoPreferredVersions $ C.prettyShow pn ++ " doesn't have preferred versions"
            Just (ver', _)
                | ver == ver' ->
                    -- putDebug $ C.prettyShow pid ++ " is latest version"
                    return ()
                | ver <  ver' -> putWarning WNotLatest $
                    C.prettyShow pn ++ " doesn't use latest version"
                    ++ "; latest " ++ C.prettyShow ver'
                    ++ "; used " ++ C.prettyShow ver
                | otherwise   -> putWarning WNotOnHackage $
                    C.prettyShow pid ++ " is not on Hackage"


-------------------------------------------------------------------------------
-- Warnings
-------------------------------------------------------------------------------

data W
    = WNotOnHackage
    | WNoPreferredVersions
    | WNotLatest

instance Warning W where
    warningToFlag WNotOnHackage        = "not-on-hackage"
    warningToFlag WNoPreferredVersions = "no-preferred-versions" -- TODO: that's not good name
    warningToFlag WNotLatest           = "not-latest"
