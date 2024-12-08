{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE FunctionalDependencies #-}
module CabalCoreInspection.Main (main) where

import Peura
import Prelude ()

import Control.Applicative ((<**>))
import Data.Version        (showVersion)

import qualified Cabal.Config            as Cbl
import qualified Cabal.Plan              as P
import qualified Data.Map.Strict         as M
import qualified Data.Set                as S
import qualified Data.Text               as T
import qualified Distribution.ModuleName as MN
import qualified Distribution.Package    as C
import qualified Distribution.Parsec     as C
import qualified Options.Applicative     as O

import Paths_cabal_core_inspection (version)

import CabalCoreInspection.GHC

import qualified GHC.Data.FastString       as FS
import qualified GHC.Driver.DynFlags       as GHC
import qualified GHC.Iface.Syntax          as GHC
import qualified GHC.Iface.Type            as GHC
import qualified GHC.Types.Name            as GHC
import qualified GHC.Types.Name.Cache      as GHC (initNameCache)
import qualified GHC.Types.Name.Occurrence as GHC
import qualified GHC.Types.Var             as GHC
import qualified GHC.Unit.Module.ModIface  as GHC
import qualified GHC.Utils.Fingerprint     as GHC
import qualified GHC.Unit.Module.Deps as GHC
import qualified GHC.Unit.Types as GHC


-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    -- options
    opts <- O.execParser optsP'
    tracer <- makeTracerPeu @(V1 W) (optTracer opts defaultTracerOptions)

    runPeu tracer () $ do

        -- ghc info, in particular storeDir
        ghcInfo <- getGhcInfo tracer $ optCompiler opts
        cblCfg  <- liftIO Cbl.readConfig
        storeDir <- makeAbsoluteFilePath $ runIdentity $ Cbl.cfgStoreDir cblCfg
        let storeDir' = storeDir </> fromUnrootedFilePath ("ghc-" ++ prettyShow (ghcVersion ghcInfo))
        -- TODO: add platform syntax
        let globalDir = takeDirectory (ghcGlobalDb ghcInfo) </> fromUnrootedFilePath "x86_64-linux-ghc-9.8.2"
        putInfo tracer $ "global dir " ++ show globalDir

        -- read plan
        plan <- case optPlan opts of
            Just planPath -> do
                planPath' <- makeAbsolute planPath
                putInfo tracer $ "Reading plan.json: " ++ toFilePath planPath'
                liftIO $ P.decodePlanJson (toFilePath planPath')
            Nothing -> do
                putInfo tracer "Reading plan.json for current project"
                liftIO $ P.findAndDecodePlanJson (P.ProjectRelativeToDir ".")

        -- TODO: check that ghcInfo version and plan one are the same

        dflags <- getDynFlags tracer ghcInfo

        let units :: Map P.UnitId P.Unit
            units = P.pjUnits plan

        return ()

{-

        -- read destination directories of units in the plan
        unitDistDirs <- traverse makeAbsoluteFilePath
                [ distDir
                | unit <- M.elems (P.pjUnits plan)
                , P.uType unit == P.UnitTypeLocal
                , distDir <- toList (P.uDistDir unit)
                ]

        -- interface files
        hiFiles <- fmap concat $ for unitDistDirs $ \distDir ->  do
            hiFiles <- globDir1 "build/**/*.hi" distDir
            return $ (,) distDir <$> hiFiles

        -- name cache, needed for reading interface files
        ncu <- liftIO $ GHC.initNameCache 'q' []

        -- For each of .hi file we found, let us see if there are orphans
        for_ hiFiles $ \(distDir, hiFile) -> do
            modIface <- liftIO $ easyReadBinIface dflags ncu hiFile
            let mn = ghcShow dflags (GHC.mi_module modIface)
            putInfo tracer $ "Found interface file for " ++ mn

            for_ (GHC.mi_usages modIface) $ \usage -> do
                putInfo tracer $ "usage: " ++ case usage of
                    GHC.UsagePackageModule { GHC.usg_mod = m } -> "package module " ++ ghcShow dflags m
                    GHC.UsageHomeModule { GHC.usg_mod_name = mn } -> "home module " ++ ghcShow dflags mn
                    GHC.UsageHomeModuleInterface { GHC.usg_mod_name = mn } -> "home module if " ++ ghcShow dflags mn
                    GHC.UsageMergedRequirement { GHC.usg_mod = m } -> "merged requirement " ++ ghcShow dflags m
                    GHC.UsageFile { GHC.usg_file_path = p } -> "usage file " ++ ghcShow dflags p

                case usage of
                    GHC.UsagePackageModule { GHC.usg_mod = GHC.Module u mn } -> do
                        u' <- case u of
                            GHC.RealUnit (GHC.Definite u') -> return (ghc2plan u')
                            _ -> error "FAIL" -- TODO
                        putInfo tracer $ "  " ++ ghcShow dflags u ++ " " ++ show (M.member u' units)
                    _ -> return ()

{-
            when (mn == "Example") $ do

                case GHC.mi_extra_decls modIface of
                    Nothing ->
                        putWarning tracer WModuleWithoutCore $ "Module without Core bindings " ++ mn

                    Just extra_decls -> do
                        example tracer dflags (GHC.mi_decls modIface) extra_decls
-}
-}

  where
    optsP' = O.info (optsP <**> O.helper <**> versionP) $ mconcat
        [ O.fullDesc
        , O.progDesc "Query interface files"
        , O.header "cabal-iface-query - all iface belong to us"
        ]

    versionP = O.infoOption (showVersion version)
        $ O.long "version" <> O.help "Show version"

-------------------------------------------------------------------------------
-- Opts
-------------------------------------------------------------------------------

data Opts = Opts
    { optCompiler     :: FilePath
    , optPlan         :: Maybe FsPath
    -- , optSkipPackages :: Set C.PackageName
    , optTracer       :: TracerOptions W -> TracerOptions W
    }

optsP :: O.Parser Opts
optsP = Opts
    <$> O.strOption (O.short 'w' <> O.long "with-compiler" <> O.value "ghc" <> O.showDefault <> O.help "Specify compiler to use")
    <*> optional (O.option (O.eitherReader $ return . fromFilePath) (O.short 'p' <> O.long "plan" <> O.metavar "PATH" <> O.help "Use plan.json provided"))
{-
    <*> fmap S.fromList (many (O.option (O.eitherReader C.eitherParsec) $ mconcat
        [ O.short 'e'
        , O.long "exclude"
        , O.metavar "PKGNAME..."
        , O.help "Don't report following packages"
        ]))
-}
    <*> tracerOptionsParser

-------------------------------------------------------------------------------
-- Warnings
-------------------------------------------------------------------------------

data W
    = WOrphans
    | WModuleWithoutCore
    | WUnknownUnit
    | WMissingIfaceFile
    | WInspection
  deriving (Eq, Ord, Enum, Bounded)

instance Universe W where universe = [minBound .. maxBound]
instance Finite W

instance Warning W where
    warningToFlag WOrphans             = "orphans"
    warningToFlag WModuleWithoutCore   = "module-without-core"
    warningToFlag WUnknownUnit         = "unknown-unit"
    warningToFlag WMissingIfaceFile    = "missing-iface-file"
    warningToFlag WInspection          = "inspection"

-------------------------------------------------------------------------------
-- Ad-hoc tests
-------------------------------------------------------------------------------

example
    :: forall r. TracerPeu  r (V1 W)
    -> GHC.DynFlags
    -> [(GHC.Fingerprint, GHC.IfaceDecl)]
    -> [GHC.IfaceBindingX GHC.IfaceMaybeRhs GHC.IfaceTopBndrInfo]
    -> Peu r ()
example tracer dflags decls extra_decls = do
{-
    putInfo tracer "mi_decls"
    traverse_ (putInfo tracer . ghcShow dflags) decls

    putInfo tracer "mi_extra_decls"
    traverse_ (putInfo tracer . ghcShow dflags) extra_decls
-}
    ifor_ declMap $ \b r -> case b of
        GblBndr name -> do
            let nameStr = GHC.occNameString $ GHC.occName name

            -- no Text
            when (nameStr == "countChars") $ do

                let f :: GHC.Name -> Peu r ()
                    f name' = do
                        let nameStr' = GHC.occNameString $ GHC.occName name'
                        when (nameStr' == "decodeUtf8With1") $ do
                            putWarning tracer WInspection $ "Text value created with " ++ nameStr' ++ " in " ++ nameStr

                noTypes tracer dflags f r

            -- no Generics
            when (nameStr == "$fEqT") $ do

                let f :: GHC.Name -> Peu r ()
                    f name' = do
                        case M.lookup (GblBndr name') declMap of
                            Nothing -> do
                                let m = GHC.nameModule name'
                                let moduleNameStr = ghcShow dflags m -- TODO
                                let nameStr' = GHC.occNameString $ GHC.occName name'
                                when (moduleNameStr == "GHC.Generics") $ do
                                    putWarning tracer WInspection $ "Found " ++ nameStr' ++ " from " ++ moduleNameStr

                            -- this might loop
                            Just r' -> do
                                -- putInfo tracer $ ghcShow dflags name' ++ " = " ++ ghcShow dflags r'
                                noTypes tracer dflags f r'

                noTypes tracer dflags f r

        _ -> return ()

  where
    declMap :: Map Bndr GHC.IfaceExpr
    declMap = M.fromList
        [ case d of
            GHC.IfaceNonRec b r ->
                (toBndr b, case r of { GHC.IfRhs e -> e; GHC.IfUseUnfoldingRhs -> error "lookup decls" })

            GHC.IfaceRec {} -> error "TODO1"
        | d <- extra_decls
        ]

toBndr :: GHC.IfaceTopBndrInfo -> Bndr
toBndr (GHC.IfGblTopBndr b)       = GblBndr b
toBndr (GHC.IfLclTopBndr s _ _ _) = LclBndr s

data Bndr
    = GblBndr GHC.IfaceTopBndr
    | LclBndr GHC.IfLclName

instance Eq Bndr where
    x == y = compare x y == EQ

instance Ord Bndr where
    compare (GblBndr x) (GblBndr y) = compare x y
    compare (GblBndr _) (LclBndr _) = LT
    compare (LclBndr _) (GblBndr _) = GT
    compare (LclBndr x) (LclBndr y) = FS.uniqCompareFS x y

noTypes
    :: forall r tr. TracerPeu  r tr
    -> GHC.DynFlags
    -> (GHC.IfExtName -> Peu r ())
    -> GHC.IfaceExpr
    -> Peu r ()
noTypes tracer dflags onName = go where
    go (GHC.IfaceTick _ e) =
        go e
    go (GHC.IfaceLcl _) = return ()
    go (GHC.IfaceExt x) = do
        onName x
    go (GHC.IfaceType ty) = do
        goTy ty
    go (GHC.IfaceCo co) =
        goCoercion co
    go (GHC.IfaceTuple _sort es) = do
        traverse_ go es
    go (GHC.IfaceLam (bndr, _oneShot) e) = do
        goBndr bndr
        go e
    go (GHC.IfaceApp f t) = do
        go f
        go t
    go (GHC.IfaceCase e _x alts) = do
        go e
        for_ alts $ \case
            GHC.IfaceAlt _con _xs e -> go e

    go (GHC.IfaceECase _ _)      = error "ecase?"
    go (GHC.IfaceLet bndrs e)    = do
        case bndrs of
            GHC.IfaceNonRec (GHC.IfLetBndr _x ty _idInfo _jInfo) t -> do
                goTy ty
                go t

            GHC.IfaceRec bndrs' -> for_ bndrs' $ \(GHC.IfLetBndr _x ty _idInfo _jInfo, t) -> do
                goTy ty
                go t
        go e
    go (GHC.IfaceCast e co) = do
        go e
        goCoercion co
    go (GHC.IfaceLit _)          = return ()
    go (GHC.IfaceLitRubbish _ _) = return ()
    go (GHC.IfaceFCall _ ty) =
        goTy ty

    goCoercion :: GHC.IfaceCoercion -> Peu r ()
    goCoercion _co = return () -- TODO

    goBndr :: GHC.IfaceBndr -> Peu r ()
    goBndr bndr = do
        let ty = ifaceBndrType bndr
        goTy ty

    goTy :: GHC.IfaceType -> Peu r ()
    goTy (GHC.IfaceFreeTyVar _)
        = return ()
    goTy (GHC.IfaceTyVar _)
        = return ()
    goTy (GHC.IfaceLitTy _)
        = return ()
    goTy (GHC.IfaceAppTy ty args) = do
        goTy ty
        goTyArgs args
    goTy (GHC.IfaceFunTy _flag _mult a b) = do
        goTy a
        goTy b
    goTy (GHC.IfaceForAllTy (GHC.Bndr b _flag) ty) = do
        goBndr b
        goTy ty
    goTy (GHC.IfaceTyConApp tycon args) = do
        -- tycon
        -- putInfo tracer $ ghcShow dflags tycon
        goTyArgs args
    goTy (GHC.IfaceCastTy _ _) = error "castty"
    goTy (GHC.IfaceCoercionTy _) = error "coTy"
    goTy (GHC.IfaceTupleTy _sort _prom args) =
        goTyArgs args

    goTyArgs :: GHC.IfaceAppArgs -> Peu r ()
    goTyArgs GHC.IA_Nil = return ()
    goTyArgs (GHC.IA_Arg ty _forall args) = do
        goTy ty
        goTyArgs args

ifaceBndrType :: GHC.IfaceBndr -> GHC.IfaceType
ifaceBndrType (GHC.IfaceIdBndr (_, _, t)) = t
ifaceBndrType (GHC.IfaceTvBndr (_, t)) = t


-- | Convert between @Cabal@ and @cabal-plan@ types.
class CabalPlan2GHC p g | p -> g, g -> p where
    plan2ghc :: p -> g
    ghc2plan :: g -> p

instance CabalPlan2GHC P.UnitId GHC.UnitId where
    plan2ghc = undefined
    ghc2plan (GHC.UnitId fs) = P.UnitId (fastStringToText fs)

fastStringToText :: FS.FastString -> T.Text
fastStringToText = T.pack . FS.unpackFS
