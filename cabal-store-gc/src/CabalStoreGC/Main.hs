{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Copyright: Oleg Grenrus
-- License: GPL-2.0-or-later
module CabalStoreGC.Main (main) where

import Peura

import Control.Applicative ((<**>))
import Data.Char           (isSpace)
import Data.Foldable       (foldlM)
import Data.List           (intercalate, stripPrefix)
import Data.Version        (showVersion)

import Data.Set.Optics (setOf)
import Optics.Core     (folded, _Just, (%))

import qualified Cabal.Config                                         as Cbl
import qualified Cabal.Plan                                           as P
import qualified Crypto.Hash.SHA256                                   as SHA256
import qualified Data.ByteString.Base64.URL                           as Base64
import qualified Data.ByteString.Char8                                as BS8
import qualified Data.Map.Strict                                      as Map
import qualified Data.Set                                             as Set
import qualified Distribution.Types.InstalledPackageInfo              as C
import qualified Distribution.Types.UnitId                            as C
import qualified Options.Applicative                                  as O
import qualified System.Path                                          as P
import qualified Topograph                                            as TG

import Paths_cabal_store_gc (version)

import CabalStoreGC.Deps


main :: IO ()
main = do
    opts <- O.execParser optsP'
    runPeu () $ case optAction opts of
        Default        -> doDefault opts
        Count          -> doCount opts
        Collect        -> doCollect opts
        AddRoot p      -> doAddRoot p
        AddProjectRoot -> doAddProjectRoot
        CleanupRoots   -> doCleanupRoots
  where
    optsP' = O.info (optsP <**> O.helper <**> versionP) $ mconcat
        [ O.fullDesc
        , O.progDesc "Check cabal gc"
        , O.header "cabal-store-gc - collect gc"
        ]

    versionP = O.infoOption (showVersion version)
        $ O.long "version" <> O.help "Show version"

-------------------------------------------------------------------------------
-- Options parser
-------------------------------------------------------------------------------

data Opts = Opts
    { optCompiler :: FilePath
    , optAction   :: Action
    }

data Action
    = Default
    | Count
    | Collect
    | AddRoot FsPath
    | AddProjectRoot
    | CleanupRoots

optsP :: O.Parser Opts
optsP = Opts
    <$> O.strOption (O.short 'w' <> O.long "with-compiler" <> O.value "ghc" <> O.showDefault <> O.help "Specify compiler to use")
    <*> actionP

actionP :: O.Parser Action
actionP = defaultP <|> countP <|>  collectP <|> addProjectRootP <|> addRootP <|> cleanupRootsP <|> pure Default where
    defaultP        = O.flag' Default        (O.long "default"          <> O.help "Default action is to --add-project-root (if exists), and --count the unreferenced components")
    countP          = O.flag' Count          (O.long "count"            <> O.help "Count the unreferenced components")
    collectP        = O.flag' Collect        (O.long "collect"          <> O.help "Collect the unreferenced components")
    cleanupRootsP   = O.flag' CleanupRoots   (O.long "cleanup-roots"    <> O.help "Collect dangling indirect roots")
    addProjectRootP = O.flag' AddProjectRoot (O.long "add-project-root" <> O.help "Add current plan.json as an indirect root")
    addRootP        = AddRoot <$> O.option (O.eitherReader $ return . fromFilePath)
                                             (O.long "add-root"         <> O.help "Add plan.json as indirect root" <> O.metavar "PLANJSON")

-------------------------------------------------------------------------------
-- Default
-------------------------------------------------------------------------------

doDefault :: Opts -> Peu r ()
doDefault opts = do
    -- First add current project plan.json as an indirect root
    p <- handle (\(_ :: IOException) -> return Nothing)
        $ fmap Just $ liftIO $ P.findPlanJson $ P.ProjectRelativeToDir "."

    for_ p $ \p' -> do
        p'' <- makeAbsoluteFilePath p'
        doAddRoot (FsPath p'')

    -- then count
    doCount opts

-------------------------------------------------------------------------------
-- Count packages
-------------------------------------------------------------------------------

doCount :: Opts -> Peu r ()
doCount opts = do
    Counted _ _ storeDir' storeDb removableUnitIds <- doCountImpl opts

    putInfo $ show (Set.size removableUnitIds) ++ " components can be removed from the store"

    sizes <- flippedFoldlM removableUnitIds 0 $ \acc unitId -> do
        let pkgDir = storeDir' </> fromUnrootedFilePath (prettyShow unitId)
            pkgConf = storeDb </> fromUnrootedFilePath (prettyShow unitId ++ ".conf")

        pkgDirSize <- getPathSize pkgDir
        pkgConfSize <- getPathSize pkgConf

        return $! acc + pkgDirSize + pkgConfSize

    putInfo $ show (sizes `div` (1024 * 1024)) ++ " MB can be freed"


doCollect :: Opts -> Peu r ()
doCollect opts = do
    Counted ghcInfo storeDir storeDir' storeDb removableUnitIds <- doCountImpl opts
    putInfo $ show (Set.size removableUnitIds) ++ " components will be removed from the store"

    ghcPkg <- findGhcPkg ghcInfo

    for_ removableUnitIds $ \unitId -> do
        putInfo $ "Removing " ++ prettyShow unitId

        let pkgDir = storeDir' </> fromUnrootedFilePath (prettyShow unitId)
            pkgConf = storeDb </> fromUnrootedFilePath (prettyShow unitId ++ ".conf")

        removePathForcibly pkgConf
        removePathForcibly pkgDir

    let packageDbFlag :: String
        packageDbFlag
            | ghcVersion ghcInfo >= mkVersion [7,6] = "--package-db=" ++ toFilePath storeDb
            | otherwise                             = "--package-conf=" ++ toFilePath storeDb

    -- finally recache the db.
    void $ runProcessCheck storeDir ghcPkg
        [ "recache"
        , packageDbFlag
        ]

    -- and run vanilla ghc-pkg check on the db
    void $ runProcessOutput storeDir ghcPkg
        [ "check"
        , packageDbFlag
        , "--simple-output"
        ]

flippedFoldlM :: (Foldable f, Monad m) => f a -> b -> (b -> a -> m b) -> m b
flippedFoldlM xs b f = foldlM f b xs


data Counted = Counted
    { cGhcInfo          :: GhcInfo
    , cStoreDir         :: Path Absolute
    , cStoreDir'        :: Path Absolute
    , cStoreDb          :: Path Absolute
    , cRemovableUnitIds :: Set UnitId
    }

doCountImpl :: Opts -> Peu r Counted
doCountImpl opts = do
    ghcInfo <- getGhcInfo $ optCompiler opts
    cblCfg  <- liftIO Cbl.readConfig

    putInfo "Reading global package db"
    dbG <- readPackageDb (ghcGlobalDb ghcInfo)
    putInfo $ show (Map.size dbG) ++ " packages in " ++ toFilePath (ghcGlobalDb ghcInfo)

    putInfo "Reading store package db"
    storeDir <- makeAbsoluteFilePath $ runIdentity $ Cbl.cfgStoreDir cblCfg
    let storeDir' = storeDir </> fromUnrootedFilePath ("ghc-" ++ prettyShow (ghcVersion ghcInfo))
    let storeDb = storeDir' </> fromUnrootedFilePath "package.db"
    dbS <- doesDirectoryExist storeDb >>= \exists ->
        if exists
        then readPackageDb storeDb
        else return Map.empty
    putInfo $ show (Map.size dbS) ++ " packages in " ++ toFilePath storeDb

    let db = dbG <> dbS

    putInfo "Reading installdir roots"
    installDir <- makeAbsoluteFilePath $ runIdentity $ Cbl.cfgInstallDir cblCfg
    exes <- listDirectory installDir

    exes' <- fmap catMaybes $ for exes $ \exe ->
        handle (\(_ :: IOException) -> return Nothing) $
        fmap Just $ getSymbolicLinkTarget (installDir </> exe) >>= canonicalizePath

    let exes'' :: [UnitId]
        exes''
            = mapMaybe (isInStore storeDir')
            $ Set.toList
            $ Set.fromList exes'

    installDirUnitIds <- Set.fromList . concat <$> traverse (readExeDeps storeDir') exes''

    putDebug $ "Found " ++ show (Set.size installDirUnitIds) ++ " installdir roots from "
        ++ show (length exes'') ++ " executables"

    putInfo "Reading environment roots"
    envs <- handle (\(_ :: IOException) -> return [])
        $ listDirectory $ ghcEnvDir ghcInfo

    envs' <- for envs $ \env -> do
        contents <- readByteString $ ghcEnvDir ghcInfo </> env
        for (BS8.lines contents) $ \line ->
            for (BS8.stripPrefix "package-id " line) $ \sfx ->
                return (C.mkUnitId (trim (fromUTF8BS sfx)))

    let envUnitIds :: Set UnitId
        envUnitIds = setOf (folded % folded % _Just) envs'

    putDebug $ "Found " ++ show (Set.size envUnitIds) ++ " environment roots"

    putInfo "Reading indirect roots"
    let rootsDir = storeDir </> fromUnrootedFilePath "roots"
    indirects <- listDirectory rootsDir
    plans <- fmap catMaybes $ for indirects $ \indirect ->
        handle (\(_ :: IOException) -> return Nothing) $
        fmap Just $ liftIO $ P.findAndDecodePlanJson $
        P.ExactPath $ toFilePath $ rootsDir </> indirect

    let indirectUnitIds ::  Set UnitId
        indirectUnitIds = Set.fromList
            [ toCabal unitId
            | plan <- plans
            , toCabal (P.pjCompilerId plan) == PackageIdentifier (mkPackageName "ghc") (ghcVersion ghcInfo)
            , unitId <- Map.keys (P.pjUnits plan)
            ]

    putDebug $ "Found " ++ show (Set.size envUnitIds) ++ " indirect roots"

    let rootUnitIds :: Set UnitId
        rootUnitIds = installDirUnitIds <> envUnitIds <> indirectUnitIds

    putInfo $ "Found " ++ show (Set.size rootUnitIds) ++ " roots"

    putInfo "Finding dependencies"
    -- reverse dependencies
    let dbAm :: Map UnitId (Set UnitId)
        dbAm = Map.map (Set.fromList . C.depends) db
        reportLoop unitIds = do
            putError $ "There is a loop in package-db " ++ intercalate " -> " (map prettyShow unitIds)
            exitFailure

    closure <- either reportLoop (return . Set.union rootUnitIds . Set.fromList) $ TG.runG dbAm $ \g ->
        [ TG.gFromVertex g' dep
        | let g' = TG.closure g
        , broken <- Set.toList rootUnitIds
        , broken' <- toList $ TG.gToVertex g' broken
        , dep     <- TG.gEdges g' broken'
        ]

    putInfo $ show (Set.size closure) ++ " components are referenced from the roots"
    putInfo $ show (Set.size $ Map.keysSet dbS `Set.intersection` closure) ++ " components are in the store"

    let removableUnitIds :: Set UnitId
        removableUnitIds = Map.keysSet dbS `Set.difference` closure

    return Counted
        { cGhcInfo          = ghcInfo
        , cStoreDir         = storeDir
        , cStoreDir'        = storeDir'
        , cStoreDb          = storeDb
        , cRemovableUnitIds = removableUnitIds
        }

-------------------------------------------------------------------------------
-- Store Exe
-------------------------------------------------------------------------------

readExeDeps :: Path Absolute -> UnitId -> Peu r [UnitId]
readExeDeps storeDir unitId = handle (\(_ :: IOException) -> return []) $ do
    let fp = storeDir </> fromUnrootedFilePath (C.unUnitId unitId)
                      </> fromUnrootedFilePath "cabal-hash.txt"
    contents <- readByteString fp
    return (unitId : extractDeps contents)

isInStore :: Path Absolute -> Path Absolute -> Maybe UnitId
isInStore storeDir p = do
    sfx <- stripPrefix (P.splitFragments (P.unrootPath storeDir)) (P.splitFragments (P.unrootPath p))
    case sfx of
        (unitId:_) -> Just (C.mkUnitId (toUnrootedFilePath unitId))
        []         -> Nothing

-------------------------------------------------------------------------------
-- Add package root
-------------------------------------------------------------------------------

doAddRoot :: FsPath -> Peu r ()
doAddRoot p' = do
    cblCfg  <- liftIO Cbl.readConfig

    p <- makeAbsolute p'

    let fp = toUTF8BS $ toFilePath p
    let hash = filter (/= '=') $ fromUTF8BS $ Base64.encode $ SHA256.hash fp

    storeDir <- makeAbsoluteFilePath $ runIdentity $ Cbl.cfgStoreDir cblCfg
    let rootsDir = storeDir </> fromUnrootedFilePath "roots"
        rootFile = rootsDir </> fromUnrootedFilePath hash

    createDirectoryIfMissing True rootsDir

    putInfo $ "Creating indirect root " ++ toFilePath rootFile ++ " pointing to " ++ toFilePath p

    -- remove existing path, before creating the link
    removePathForcibly rootFile
    createFileLink p rootFile

doAddProjectRoot :: Peu r ()
doAddProjectRoot = do
    p <- liftIO $ P.findPlanJson $ P.ProjectRelativeToDir "."
    p' <- makeAbsoluteFilePath p
    doAddRoot (FsPath p')

-------------------------------------------------------------------------------
-- Cleaning up roots
-------------------------------------------------------------------------------

doCleanupRoots :: Peu r ()
doCleanupRoots = do
    cblCfg  <- liftIO Cbl.readConfig
    storeDir <- makeAbsoluteFilePath $ runIdentity $ Cbl.cfgStoreDir cblCfg
    let rootsDir = storeDir </> fromUnrootedFilePath "roots"

    roots <- listDirectory rootsDir

    for_ roots $ \r -> do
        let r' = rootsDir </> r
        e <- doesFileExist r'
        unless e $ do
            isLink <- pathIsSymbolicLink r'
            if isLink
            then do
                l <- getSymbolicLinkTarget r'
                putInfo $ "Indirect root " ++ toFilePath l ++ " is gone; cleaning up"
                removePathForcibly r'
            else
                putError $ "Doesn't exist, but isn't a link either: " ++ toFilePath r'

-------------------------------------------------------------------------------
-- getPathSize
-------------------------------------------------------------------------------

getPathSize :: Path Absolute -> Peu r Integer
getPathSize p = do
    e <- doesPathExist p
    if e
    then do
        isLink <- pathIsSymbolicLink p
        if isLink
        then return 0
        else do
            isFile <- doesFileExist p
            if isFile
            then getFileSize p
            else do
                isDirectory <- doesDirectoryExist p
                if isDirectory
                then do
                    contents <- listDirectory p
                    sizes <- traverse (\p' -> getPathSize $ p </> p') contents
                    return (foldl' (+) 0 sizes)

                else return 0
    else return 0

-------------------------------------------------------------------------------
-- MissingH
-------------------------------------------------------------------------------

trim :: String -> String
trim = tr . tr where tr = dropWhile isSpace . reverse
