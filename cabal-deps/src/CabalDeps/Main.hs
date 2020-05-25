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
import qualified Distribution.Parsec                          as C
import qualified Distribution.Types.BuildInfo                 as C
import qualified Distribution.Types.BuildInfo.Lens            as L
import qualified Distribution.Types.ComponentName             as C
import qualified Distribution.Types.Condition                 as C
import qualified Distribution.Types.CondTree                  as C
import qualified Distribution.Types.ConfVar                   as C
import qualified Distribution.Types.ExeDependency             as C
import qualified Distribution.Types.GenericPackageDescription as C
import qualified Distribution.Types.LibraryName               as C
import qualified Distribution.Types.PackageDescription        as C
import qualified Distribution.Types.SetupBuildInfo            as C
import qualified Distribution.Version                         as C

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
    { optAction  :: Action
    , optExclude :: Set PackageName
    }

data Action
    = ActionCabal [FsPath]
    | ActionBuilddir FsPath

optsP :: O.Parser Opts
optsP = Opts
    <$> actionP
    <*> fmap Set.fromList (many (O.option (O.eitherReader C.eitherParsec) $ mconcat
        [ O.short 'e'
        , O.long "exclude"
        , O.metavar "PKGNAME..."
        , O.help "Don't report following packages"
        ]))

actionP :: O.Parser Action
actionP = builddirP <|> cabalP where
    cabalP    = ActionCabal <$> many (O.argument fspath (O.metavar "CABALFILE..." <> O.help "Cabal files"))
    builddirP = ActionBuilddir <$> O.option fspath (O.long "builddir" <> O.metavar "BUILDDIR" <> O.help "build directory with plan.json")

    fspath = O.eitherReader $ return . fromFilePath

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

doDeps :: Opts -> Peu r ()
doDeps opts = do
    putInfo "Reading Hackage metadata"
    meta <- liftIO I.cachedHackageMetadata

    case optAction opts of
        ActionCabal []     -> doPlanDeps meta (optExclude opts) (P.ProjectRelativeToDir ".")
        ActionCabal (x:xs) -> doGpdDeps  meta (optExclude opts) (x :| xs)
        ActionBuilddir p   -> do
            p' <- makeAbsolute p
            doPlanDeps meta (optExclude opts) (P.InBuildDir (toFilePath p'))

-------------------------------------------------------------------------------
-- Check GPD
-------------------------------------------------------------------------------

doGpdDeps :: Map PackageName I.PackageInfo -> Set PackageName -> NonEmpty FsPath -> Peu r ()
doGpdDeps meta excl fps = do
    gpds <- for fps $ \fp -> do
        fp' <- makeAbsolute fp
        liftIO $ Pkg.readPackage $ toFilePath fp'

    for_ gpds $ \gpd ->
        checkGpd meta excl gpd

    -- TODO: use exitCode to indicate

-- TODO: return True or False if fine or not.

checkGpd :: Map PackageName I.PackageInfo -> Set PackageName -> C.GenericPackageDescription -> Peu r ()
checkGpd meta excl gpd = do
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
            , Set.notMember pn excl
            ]

    ifor_ comps $ \n c ->
        checkDepMap meta (C.prettyShow packageName ++ ":" ++ C.prettyShow n) (condTreeToDepMap excl c)

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

condTreeToDepMap :: Set PackageName -> C.CondTree C.ConfVar () L.BuildInfo -> DepMap
condTreeToDepMap excl = go where
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
            , Set.notMember pn excl
            ]

        buildtools = DepMap $ Map.fromListWith C.intersectVersionRanges
            [ (pn, vr)
            | C.ExeDependency pn _ vr <- C.buildToolDepends bi
            , Set.notMember pn excl
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

doPlanDeps :: Map PackageName I.PackageInfo -> Set PackageName -> P.SearchPlanJson -> Peu r ()
doPlanDeps meta excl search = do
    putInfo "Reading plan.json for current project"
    plan <- liftIO $ P.findAndDecodePlanJson search

    let pkgIds :: Set C.PackageIdentifier
        pkgIds = Set.fromList
            [ toCabal (P.uPId unit)
            | unit <- Map.elems (P.pjUnits plan)
            -- filter global-db packages
            , P.uType unit /= P.UnitTypeBuiltin
            ]

    for_ pkgIds $ \pkgId@(PackageIdentifier pn _) ->
        unless (Set.member pn excl) $ checkPlan meta pkgId

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
