{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Copyright: Oleg Grenrus
-- License: GPL-2.0-or-later
module CabalStoreCheck.Main (main) where

import Peura

import Control.Applicative ((<**>))
import Data.List           (intercalate, isPrefixOf)
import Data.Version        (showVersion)

import qualified Cabal.Config                            as Cbl
import qualified Data.ByteString.Char8                   as BS8
import qualified Data.List.NonEmpty                      as NE
import qualified Data.Map.Strict                         as Map
import qualified Data.Set                                as Set
import qualified Distribution.Compat.Newtype             as C
import qualified Distribution.ModuleName                 as C
import qualified Distribution.Parsec                     as C
import qualified Distribution.Parsec.Newtypes            as C
import qualified Distribution.Types.InstalledPackageInfo as C
import qualified Distribution.Types.UnitId               as C
import qualified Options.Applicative                     as O
import qualified Topograph                               as TG

import Paths_cabal_store_check (version)

main :: IO ()
main = do
    opts <- O.execParser optsP'
    tracer <- makeTracerPeu (optTracer opts defaultTracerOptions)
    runPeu tracer () $ checkConsistency tracer opts
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
    , optTracer   :: TracerOptions Void -> TracerOptions Void
    }

optsP :: O.Parser Opts
optsP = Opts
    <$> O.strOption (O.short 'w' <> O.long "with-compiler" <> O.value "ghc" <> O.showDefault <> O.help "Specify compiler to use")
    <*> repairP
    <*> tracerOptionsParser
  where
    repairP = O.flag' True (O.long "repair" <> O.help "Try to repair the db, i.e. remove invalid packages") <|> pure False

-------------------------------------------------------------------------------
-- check consistency
-------------------------------------------------------------------------------

checkConsistency :: forall r w. TracerPeu r w -> Opts -> Peu r ()
checkConsistency tracer opts = do
    ghcInfo <- getGhcInfo tracer $ optCompiler opts
    cblCfg  <- liftIO Cbl.readConfig

    putInfo tracer "Reading global package db"
    dbG <- readPackageDb tracer (ghcGlobalDb ghcInfo)
    putInfo tracer $ show (Map.size dbG) ++ " packages in " ++ toFilePath (ghcGlobalDb ghcInfo)

    -- TODO: handle non-existence of store-db
    putInfo tracer "Reading store package db"
    storeDir <- makeAbsoluteFilePath $ runIdentity $ Cbl.cfgStoreDir cblCfg
    let storeDir' = storeDir </> fromUnrootedFilePath ("ghc-" ++ prettyShow (ghcVersion ghcInfo))
    let storeDb = storeDir' </> fromUnrootedFilePath "package.db"
    dbS <- readPackageDb tracer storeDb
    putInfo tracer $ show (Map.size dbS) ++ " packages in " ++ toFilePath storeDb

    let db = dbG <> dbS

    -- report broken packages.
    brokenUnitIds <- fmap (Set.fromList . concat) $ ifor dbS $ \unitId ipi -> do
        b <- checkIpi tracer storeDir' db ipi
        return $ if b then [] else [unitId]

    -- reverse dependencies
    let dbAm = Map.map (Set.fromList . C.depends) db
        reportLoop unitIds = do
            die tracer $ "There is a loop in package-db " ++ intercalate " -> " (map prettyShow unitIds)

    closure <- either reportLoop (return . Set.fromList) $ TG.runG dbAm $ \g ->
        [ TG.gFromVertex g' dep
        | let g' = TG.closure $ TG.transpose g
        , broken <- Set.toList brokenUnitIds
        , broken' <- toList $ TG.gToVertex g' broken
        , dep     <- TG.gEdges g' broken'
        ]

    for_ closure $ \unitId ->
        putError tracer $ prettyShow unitId ++ " is broken transitively"

    let brokenLibs :: Set UnitId
        brokenLibs = brokenUnitIds <> closure

    -- lookup executables
    unitDirs <- fmap (filter (`notElem` map fromUnrootedFilePath ["incoming", "package.db"]))
        $ listDirectory storeDir'

    let unitDirIds :: Set UnitId
        unitDirIds = Set.fromList $ map (C.mkUnitId . toUnrootedFilePath) unitDirs

    -- The unregistered packages must all be executables,
    -- we check that they are, and that their dependencies are present,
    -- i.e. none is in broken closure
    --
    brokenExes <- fmap (Set.fromList . catMaybes) $ for (Set.toList $ unitDirIds `Set.difference` Map.keysSet db) $ \unitId -> do
        let fp = storeDir' </> fromUnrootedFilePath (C.unUnitId unitId)
                           </> fromUnrootedFilePath "cabal-hash.txt"

        let handler :: IOException -> Peu r (Maybe UnitId)
            handler exc = do
                putError tracer $ prettyShow unitId ++ " executable is broken: " ++ displayException exc
                return (Just unitId)

        handle handler $ do
            contents <- readByteString fp

            let ch :: CabalHash
                ch = extractDeps contents

            let broken :: Either String ()
                broken = do
                    unless ("ComponentExe" `isPrefixOf` chComponent ch) $
                        Left "No component entry in cabal-hash.txt"

                    when (any (`Set.member` closure) $ chDeps ch) $
                        Left "Broken dependency"

            case broken of
                Right () -> return Nothing
                Left err -> do
                    putError tracer $ prettyShow unitId ++ " executable is broken: " ++ show err
                    return (Just unitId)

    -- totals

    putInfo tracer $ show (Set.size brokenUnitIds) ++ " directly broken library components"
    putInfo tracer $ show (Set.size closure) ++ " transitively broken libraries"
    putInfo tracer $ show (Set.size brokenExes) ++ " broken executable components"

    -- Repairing

    when (not (null brokenExes) && optRepair opts) $ do
        for_ brokenExes $ \unitId -> do
            let pkgDir = storeDir' </> fromUnrootedFilePath (prettyShow unitId)

            ed <- doesDirectoryExist pkgDir
            if ed
            then do
                putDebug tracer $ "Removing " ++ toFilePath pkgDir
                removePathForcibly pkgDir
            -- TODO: Warning
            else putError tracer $ toFilePath pkgDir ++ " does not exist"

    when (not (null brokenLibs) && optRepair opts) $ do
        putInfo tracer "Repairing db"
        ghcPkg <- findGhcPkg tracer ghcInfo

        for_ brokenLibs $ \unitId -> do
            let pkgDir = storeDir' </> fromUnrootedFilePath (prettyShow unitId)
                pkgConf = storeDb </> fromUnrootedFilePath (prettyShow unitId ++ ".conf")

            ed <- doesDirectoryExist pkgDir
            when ed $ do
                putDebug tracer $ "Removing " ++ toFilePath pkgDir
                removePathForcibly pkgDir

            ec <- doesFileExist pkgConf
            when ec $ do
                putDebug tracer $ "Removing " ++ toFilePath pkgConf
                removePathForcibly pkgConf

        let packageDbFlag :: String
            packageDbFlag
                | ghcVersion ghcInfo >= mkVersion [7,6] = "--package-db=" ++ toFilePath storeDb
                | otherwise                             = "--package-conf=" ++ toFilePath storeDb

        -- finally recache the db.
        void $ runProcessCheck tracer storeDir ghcPkg
            [ "recache"
            , packageDbFlag
            ]

        -- and run vanilla ghc-pkg check on the db
        void $ runProcessOutput tracer storeDir ghcPkg
            [ "check"
            , packageDbFlag
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
checkIpi :: TracerPeu r w -> Path Absolute -> PackageDb -> C.InstalledPackageInfo -> Peu r Bool
checkIpi tracer storeDir db ipi = fmap isJust $ validate (reportV tracer . NE.head) $
    checkDirectory *>
    traverse_ checkDep           (C.depends ipi) *>
    traverse_ checkExposedModule (C.exposedModules ipi) *>
    traverse_ checkModuleFile    (C.hiddenModules ipi)
  where
    unitId = C.installedUnitId ipi

    checkDep :: UnitId -> Validate r ()
    checkDep dep
        | Map.member dep db = pure ()
        | otherwise         = invalid $ pure $ VMissingDep unitId dep

    checkDirectory :: Validate r ()
    checkDirectory = ValidateT $ do
        let dir = storeDir </> fromUnrootedFilePath (prettyShow unitId)
        exists <- doesDirectoryExist dir
        if exists
        then return $ Right ()
        else return $ Left $ pure $ VMissingDir unitId

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
    = VMissingDir UnitId
    | VMissingDep UnitId UnitId
    | VMissingModuleFile UnitId C.ModuleName

type Validate r = ValidateT (NonEmpty V) (Peu r)

reportV :: TracerPeu r w -> V -> Peu r ()
reportV tracer (VMissingDir pkg) = putError tracer $
    prettyShow pkg ++ " unit directory is missing"
reportV tracer (VMissingDep pkg dep) = putError tracer $
    prettyShow pkg ++ " dependency " ++ prettyShow dep ++ " is missing"
reportV tracer (VMissingModuleFile pkg mdl) = putError tracer $
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

-------------------------------------------------------------------------------
-- cabal-hash.txt
-------------------------------------------------------------------------------

-- TODO: we should parse the component, and check that there's executable
-- matching the component name.
data CabalHash = CabalHash
    { chComponent :: String
    , chDeps      :: [UnitId]
    }
  deriving Show

extractDeps :: ByteString -> CabalHash
extractDeps contents = CabalHash
    { chComponent = firstOr ""
        [ fromUTF8BS sfx
        | l <- ls
        , Just sfx' <- return (BS8.stripPrefix "component:" l)
        , let sfx = BS8.dropWhile isSpace sfx'
        ]
    , chDeps =
        [ unitId
        | l <- ls
        , Just sfx' <- return (BS8.stripPrefix "deps:" l)
        , let sfx = BS8.dropWhile isSpace sfx'
        , unitId <- either fail (C.unpack' (C.alaList C.CommaFSep))
            $ C.eitherParsec $ fromUTF8BS sfx
        ]
    }
  where
    firstOr x []    = x
    firstOr _ (x:_) = x

    ls = BS8.lines contents
