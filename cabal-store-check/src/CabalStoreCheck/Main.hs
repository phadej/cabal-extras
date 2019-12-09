{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
-- |
-- Copyright: Oleg Grenrus
-- License: GPL-2.0-or-later
module CabalStoreCheck.Main (main) where

import Peura

import Control.Applicative ((<**>))
import Data.List           (isSuffixOf, intercalate)
import Data.Maybe          (isJust)
import Data.Version        (showVersion)

import qualified Cabal.Config                                         as Cbl
import qualified Cabal.Parse                                          as Cbl
import qualified Data.List.NonEmpty                                   as NE
import qualified Data.Map.Strict                                      as Map
import qualified Data.Set                                             as Set
import qualified Distribution.CabalSpecVersion                        as C
import qualified Distribution.FieldGrammar                            as C
import qualified Distribution.ModuleName                              as C
import qualified Distribution.Types.InstalledPackageInfo              as C
import qualified Distribution.Types.InstalledPackageInfo.FieldGrammar as C
import qualified Distribution.Types.UnitId                            as C
import qualified Topograph as TG
import qualified Options.Applicative                                  as O

import Paths_cabal_store_check (version)

main :: IO ()
main = do
    opts <- O.execParser optsP'
    runPeu () $ checkConsistency opts
  where
    optsP' = O.info (optsP <**> O.helper <**> versionP) $ mconcat
        [ O.fullDesc
        , O.progDesc "Check cabal store"
        , O.header "cabal-store-check - ghc-pkg check for .cabal/store"
        ]

    versionP = O.infoOption (showVersion version)
        $ O.long "version" <> O.help "Show version"

-------------------------------------------------------------------------------
-- Options parser
-------------------------------------------------------------------------------

data Opts = Opts
    { optCompiler :: FilePath
    , optRepair   :: Bool
    }

optsP :: O.Parser Opts
optsP = Opts
    <$> O.strOption (O.short 'w' <> O.long "with-compiler" <> O.value "ghc" <> O.showDefault <> O.help "Specify compiler to use")
    <*> repairP
  where
    repairP = O.flag' True (O.long "repair" <> O.help "Try to repair the db, i.e. remove invalid packages") <|> pure False

-------------------------------------------------------------------------------
-- check consistency
-------------------------------------------------------------------------------

checkConsistency :: Opts -> Peu r ()
checkConsistency opts = do
    ghcInfo <- getGhcInfo $ optCompiler opts
    cblCfg  <- liftIO Cbl.readConfig

    putInfo "Reading global package db"
    dbG <- readDb (ghcGlobalDb ghcInfo)
    putInfo $ show (Map.size dbG) ++ " packages in " ++ toFilePath (ghcGlobalDb ghcInfo)

    -- TODO: handle non-existence of store-db
    putInfo "Reading store package db"
    storeDir <- makeAbsoluteFilePath $ runIdentity $ Cbl.cfgStoreDir cblCfg
    let storeDir' = storeDir </> fromUnrootedFilePath ("ghc-" ++ prettyShow (ghcVersion ghcInfo))
    let storeDb = storeDir' </> fromUnrootedFilePath "package.db"
    dbS <- readDb storeDb
    putInfo $ show (Map.size dbS) ++ " packages in " ++ toFilePath (ghcGlobalDb ghcInfo)

    let db = dbG <> dbS

    -- report broken packages.
    brokenUnitIds <- fmap (Set.fromList . concat) $ ifor db $ \unitId ipi -> do
        b <- checkIpi db ipi
        return $ if b then [] else [unitId]

    -- reverse dependencies
    let dbAm = Map.map (Set.fromList . C.depends) db
        reportLoop unitIds = do
            putError $ "There is a loop in package-db " ++ intercalate " -> " (map prettyShow unitIds)
            exitFailure

    closure <- either reportLoop (return . Set.fromList) $ TG.runG dbAm $ \g ->
        [ TG.gFromVertex g' dep
        | let g' = TG.closure $ TG.transpose g
        , broken <- Set.toList brokenUnitIds
        , broken' <- toList $ TG.gToVertex g' broken
        , dep     <- TG.gEdges g' broken'
        ]

    for_ closure $ \unitId ->
        putError $ prettyShow unitId ++ " is broken transitively"

    when (not (null brokenUnitIds) && optRepair opts) $ do
        putInfo "Repairing db"
        ghcPkg <- findGhcPkg ghcInfo

        for_ brokenUnitIds $ \unitId -> for_ (Map.lookup unitId db) $ \_ipi -> do
            let pkgDir = storeDir' </> fromUnrootedFilePath (prettyShow unitId)
                pkgConf = storeDb </> fromUnrootedFilePath (prettyShow unitId ++ ".conf")

            ed <- doesDirectoryExist pkgDir
            if ed
            then do
                putDebug $ "Removing " ++ toFilePath pkgDir
                removePathForcibly pkgDir
            -- TODO: Warning
            else putError $ toFilePath pkgDir ++ " does not exist"

            ec <- doesFileExist pkgConf
            if ec
            then do
                putDebug $ "Removing " ++ toFilePath pkgConf
                removePathForcibly pkgConf
            else putError $ toFilePath pkgConf ++ " does not exist"

        -- finally recache the db.
        void $ runProcessCheck storeDir ghcPkg
            [ "recache"
            , "--package-db=" ++ toFilePath storeDb
            ]

        -- and run vanilla ghc-pkg check on the db
        void $ runProcessOutput storeDir ghcPkg
            [ "check"
            , "--package-db=" ++ toFilePath storeDb
            , "--simple-output"
            ]

-------------------------------------------------------------------------------
-- Check IPI
-------------------------------------------------------------------------------

-- | Light check for validity of IPI. We only check that
--
-- * dependency units exist
--
-- * module files are there
--
checkIpi :: PackageDb -> C.InstalledPackageInfo -> Peu r Bool
checkIpi db ipi = fmap isJust $ validate (reportV . NE.head) $
    traverse_ checkDep           (C.depends ipi) *>
    traverse_ checkExposedModule (C.exposedModules ipi) *>
    traverse_ checkModuleFile    (C.hiddenModules ipi)
  where
    unitId = C.installedUnitId ipi

    checkDep :: C.UnitId -> Validate r ()
    checkDep dep
        | Map.member dep db = pure ()
        | otherwise         = invalid $ pure $ VMissingDep unitId dep

    checkExposedModule :: C.ExposedModule -> Validate r ()
    checkExposedModule (C.ExposedModule mdl reexport) = do
        let orig = checkModuleFile mdl
        maybe orig (\_ -> pure ()) reexport

    checkModuleFile :: C.ModuleName -> Validate r ()
    checkModuleFile mdl
        | mdl == C.fromComponents ["GHC", "Prim"] = pure ()
        | otherwise = ValidateT $ do
            let files :: [(FilePath, FilePath)]
                files =
                    [ (dir, C.toFilePath mdl ++ "." ++ ext)
                    | dir <- C.importDirs ipi
                    , ext <- ["hi", "p_hi", "dyn_hi"]
                    ]

            files' <- for files $ \(d, p) -> do
                d' <- makeAbsoluteFilePath d
                return (d' </> fromUnrootedFilePath p)

            exists <- anyM doesFileExist files'
            if exists
            then return (Right ())
            else return $ Left $ pure $ VMissingModuleFile unitId mdl

-------------------------------------------------------------------------------
-- V: Validation reports
-------------------------------------------------------------------------------

data V
    = VMissingDep C.UnitId C.UnitId
    | VMissingModuleFile C.UnitId C.ModuleName

type Validate r = ValidateT (NonEmpty V) (Peu r)

reportV :: V -> Peu r ()
reportV (VMissingDep pkg dep) = putError $
    prettyShow pkg ++ " dependency " ++ prettyShow dep ++ " is missing"
reportV (VMissingModuleFile pkg mdl) = putError $
    prettyShow pkg ++ " interface file for " ++ prettyShow mdl ++ " is missing"

-------------------------------------------------------------------------------
-- ValidateT
-------------------------------------------------------------------------------

newtype ValidateT e m a = ValidateT (m (Either e a))
  deriving Functor

invalid :: Applicative m => e -> ValidateT e m a
invalid e = ValidateT $ pure $ Left e

instance (Applicative m, Semigroup e) => Applicative (ValidateT e m) where
    pure x = ValidateT $ pure $ Right x

    ValidateT f <*> ValidateT x = ValidateT $ combine <$> f <*> x where
        combine (Right f') (Right x') = Right (f' x')
        combine (Left e)   (Right _)  = Left e
        combine (Right _)  (Left e')  = Left e'
        combine (Left e)   (Left e')  = Left (e <> e')

validate :: (e -> Peu r ()) -> ValidateT e (Peu r) a -> Peu r (Maybe a)
validate report (ValidateT m) = do
    x <- m
    case x of
        Left e  -> Nothing <$ report e
        Right a -> return (Just a)

-------------------------------------------------------------------------------
-- PackageDb
-------------------------------------------------------------------------------

type PackageDb = Map C.UnitId C.InstalledPackageInfo

readDb :: Path Absolute -> Peu r PackageDb
readDb db = do
    files <- listDirectory db
    fmap (Map.fromList . concat) $ for files $ \p' -> do
        let p = db </> p'
        if ".conf" `isSuffixOf` toFilePath p
        then do
            contents <- readByteString p
            ipi <- either throwM return $ Cbl.parseWith parseIpi (toFilePath p) contents
            return [(C.installedUnitId ipi, ipi)]
        else return []
  where
    parseIpi fields = case C.partitionFields fields of
        (fields', _) -> C.parseFieldGrammar C.cabalSpecLatest fields' C.ipiFieldGrammar

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

anyM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m Bool
anyM p = go . toList where
    go [] = return False
    go (x:xs) = do
        c <- p x
        if c
        then return True
        else go xs
