{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Copyright: Oleg Grenrus
-- License: GPL-2.0-or-later
module CabalDiff.Main (main) where

import Peura
import Prelude ()

import Data.Version         (showVersion)
import System.FilePath.Glob (glob)

import Control.Applicative         ((<**>))
import Control.Concurrent.STM      (atomically)
import Control.Concurrent.STM.TSem (TSem, newTSem, signalTSem, waitTSem)
import Data.List                   (stripPrefix, sort)
import Distribution.Parsec         (eitherParsec)

import qualified Crypto.Hash.SHA256     as SHA256
import qualified Data.Binary            as Binary
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy   as LBS
import qualified Options.Applicative    as O

import CabalDiff.Diff
import CabalDiff.Hoogle

import Paths_cabal_diff (version)


main :: IO ()
main = do
    opts <- O.execParser optsP'
    tracer <- makeTracerPeu (optTracer opts defaultTracerOptions)
    runPeu tracer () $ doDiff tracer opts
  where
    optsP' = O.info (versionP <*> optsP <**> O.helper) $ mconcat
        [ O.fullDesc
        , O.progDesc "Diff cabal package APIs"
        , O.header "cabal-diff"
        ]

    versionP = O.infoOption (showVersion version)
        $ O.long "version" <> O.help "Show version"

-------------------------------------------------------------------------------
-- Options parser
-------------------------------------------------------------------------------

data Opts c = Opts
    { _optCompiler :: OneTwo c
    , _optPkgName  :: PackageName
    , _optVerA     :: DiffVersion
    , _optVerB     :: DiffVersion
    , optTracer    :: TracerOptions Void -> TracerOptions Void
    }

data OneTwo a
    = One a
    | Two a a
  deriving (Show, Functor, Foldable, Traversable)

data DiffVersion
    = HackageVersion Version
    | LocalVersion
  deriving (Show)

optsP :: O.Parser (Opts FilePath)
optsP = Opts
    <$> compilerP
    <*> O.argument (O.eitherReader eitherParsec) (O.metavar "PKGNAME" <> O.help "package name")
    <*> O.argument readDiffVersion (O.metavar "OLDVER" <> O.help "new version")
    <*> O.argument readDiffVersion (O.metavar "NEWVER" <> O.help "new package")
    <*> tracerOptionsParser
  where
    compilerP :: O.Parser (OneTwo FilePath)
    compilerP = one <|> two
      where
        one = One <$> O.strOption (O.short 'w' <> O.long "with-compiler" <> O.value "ghc" <> O.showDefault <> O.help "Specify compiler to use")
        two = Two
            <$> O.strOption (O.long "old-compiler" <> O.value "ghc" <> O.showDefault <> O.help "Compiler for old version")
            <*> O.strOption (O.long "new-compiler" <> O.value "ghc" <> O.showDefault <> O.help "Compiler for new version")

readDiffVersion :: O.ReadM DiffVersion
readDiffVersion = O.eitherReader $ \str -> case str of
    "." -> Right LocalVersion
    _   -> HackageVersion <$> eitherParsec str

-------------------------------------------------------------------------------
-- cache and locking
-------------------------------------------------------------------------------

cacheDir :: Path XdgCache
cacheDir = root </> fromUnrootedFilePath "cabal-diff"

-------------------------------------------------------------------------------
-- Diffing
-------------------------------------------------------------------------------

doDiff :: TracerPeu r w -> Opts FilePath -> Peu r ()
doDiff tracer (Opts withCompiler pn pkgVerA pkgVerB _) = do
    buildSem <- liftIO $ atomically (newTSem 1)

    (withCompilerA, withCompilerB) <- getGhcInfos tracer withCompiler

    dbA' <- async $ getHoogleTxt tracer withCompilerA buildSem pn pkgVerA
    dbB' <- async $ getHoogleTxt tracer withCompilerB buildSem pn pkgVerB

    dbA <- wait dbA'
    dbB <- wait dbB'

    outputApiDiff tracer (apiDiff dbA dbB)

getGhcInfos :: TracerPeu r w -> OneTwo FilePath -> Peu r (GhcInfo, GhcInfo)
getGhcInfos tracer = fmap toPair . traverse (getGhcInfo tracer) where
    toPair (One x)   = (x, x)
    toPair (Two x y) = (x, y)

getHoogleTxt :: TracerPeu r w -> GhcInfo -> TSem -> PackageName -> DiffVersion -> Peu r API
getHoogleTxt tracer withCompiler buildSem pn LocalVersion =
    getLocalHoogleTxt tracer withCompiler buildSem pn
getHoogleTxt tracer withCompiler buildSem pn (HackageVersion ver) =
    getHackageHoogleTxt tracer withCompiler buildSem (PackageIdentifier pn ver)

-------------------------------------------------------------------------------
-- Local
-------------------------------------------------------------------------------

getLocalHoogleTxt
    :: forall r w. TracerPeu r w
    -> GhcInfo            -- ^ compiler to use
    -> TSem               -- ^ is someone building dependencies
    -> PackageName
    -> Peu r API
getLocalHoogleTxt tracer gi buildSem pn = do
    cwd <- getCurrentDirectory
    withSystemTempDirectory "cabal-diff" $ \dir -> do
        _ <- runProcessCheck tracer cwd "cabal"
            [ "sdist"
            , "--builddir=" ++ toFilePath (dir </> fromUnrootedFilePath "dist-newstyle")
            , "all"
            ]

        -- we don't know the local package version, so we glob for it.
        -- Without cabal.project we'd need to glob for *.cabal file anyway.
        res  <- liftIO $ glob (toFilePath dir ++ "/dist-newstyle/sdist/*.tar.gz")
        (tarballs, tarball, pkgId) <- elaborateSdistLocation res

        -- tarball hash is using for caching, version alone is not enough
        hash <- calculateHash (tarball : tarballs)

        -- cache dir
        cacheDir' <- makeAbsolute $ cacheDir </> fromUnrootedFilePath ("ghc-" ++ prettyShow (ghcVersion gi))
        createDirectoryIfMissing True cacheDir'

        let cacheFile = cacheDir' </> fromUnrootedFilePath (prettyShow pkgId ++ "-" ++ hash ++ ".txt")

        exists <- doesFileExist cacheFile
        if exists
        then do
            putInfo tracer $ "Using cached hoogle.txt for local " ++ prettyShow pkgId
            liftIO $ Binary.decodeFile (toFilePath cacheFile)
        else do
            -- untar
            _ <- runProcessCheck tracer dir "tar" ["-xzf", toFilePath tarball]

            -- directory we expect things got untarred to
            let dir' = dir </> fromUnrootedFilePath (prettyShow pkgId)

            -- build hoogle.txt
            api <- buildHoogleTxt tracer gi buildSem pkgId tarballs dir'

            -- write cache
            liftIO $ Binary.encodeFile (toFilePath cacheFile) api

            -- return
            return api
  where
    elaborateSdistLocation :: [FilePath] -> Peu r ([Path Absolute], Path Absolute, PackageIdentifier)
    elaborateSdistLocation = go Nothing [] where
        go :: Maybe (Path Absolute, PackageIdentifier) -> [Path Absolute] -> [FilePath]
           -> Peu r ([Path Absolute], Path Absolute, PackageIdentifier)
        go (Just (fp, pid)) acc [] =
            return (acc, fp, pid)
        go Nothing _acc [] = do
            die tracer "Cannot find sdist tarball"
        go mpid acc (fp:fps) = do
            fp' <- makeAbsoluteFilePath fp
            let fn = toUnrootedFilePath (takeFileName fp')
            pid <- elaboratePkgId fn
            if pkgName pid == pn
            then case mpid of
                Nothing -> go (Just (fp', pid)) acc fps
                Just _  ->
                    die tracer $ "Found multiple sdist tarballs " ++ show fps
            else go mpid (fp' : acc) fps

    elaboratePkgId :: String -> Peu r PackageIdentifier
    elaboratePkgId str = case stripSuffix ".tar.gz" str of
        Nothing -> do
            die tracer $ "tarball path doesn't end with .tar.gz -- " ++ str

        Just pfx -> case eitherParsec pfx of
            Right pkgId -> return pkgId
            Left  err   -> die tracer err

calculateHash :: [Path Absolute] -> Peu r String
calculateHash xs = fmap post (foldM step SHA256.init (sort xs)) where
    post ctx =
        let hash = Base16.encode $ SHA256.finalize ctx
        in fromUTF8BS hash

    step ctx f = do
        lbs <- readLazyByteString f
        return (SHA256.updates ctx (LBS.toChunks lbs))

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix sfx str = fmap reverse (stripPrefix (reverse sfx) (reverse str))

-------------------------------------------------------------------------------
-- Hackage
-------------------------------------------------------------------------------

getHackageHoogleTxt
    :: TracerPeu r w
    -> GhcInfo            -- ^ compiler to use
    -> TSem               -- ^ is someone building dependencies
    -> PackageIdentifier
    -> Peu r API
getHackageHoogleTxt tracer gi buildSem pkgId = do
    cacheDir' <- makeAbsolute $ cacheDir </> fromUnrootedFilePath ("ghc-" ++ prettyShow (ghcVersion gi))
    createDirectoryIfMissing True cacheDir'

    let cacheFile = cacheDir' </> fromUnrootedFilePath (prettyShow pkgId ++ ".txt")

    exists <- doesFileExist cacheFile
    if exists
    then do
        putInfo tracer $ "Using cached hoogle.txt for " ++ prettyShow pkgId
        liftIO $ Binary.decodeFile (toFilePath cacheFile)
    else do
        api <- getHackageHoogleTxt' tracer gi buildSem pkgId
        liftIO $ Binary.encodeFile (toFilePath cacheFile) api
        return api

getHackageHoogleTxt'
    :: TracerPeu r w
    -> GhcInfo
    -> TSem
    -> PackageIdentifier
    -> Peu r API
getHackageHoogleTxt' tracer gi buildSem pkgId =
    withSystemTempDirectory "cabal-diff" $ \dir -> do
        -- fetch the package
        _ <- runProcessCheck tracer dir "cabal" ["get", prettyShow pkgId]

        -- directory cabal got
        let dir' = dir </> fromUnrootedFilePath (prettyShow pkgId)

        -- build hoogle.txt
        buildHoogleTxt tracer gi buildSem pkgId [] dir'

buildHoogleTxt
    :: TracerPeu r w
    -> GhcInfo
    -> TSem
    -> PackageIdentifier
    -> [Path Absolute] -- ^ additional tarballs 
    -> Path Absolute -> Peu r API
buildHoogleTxt tracer gi buildSem pkgId tarballs dir = do
    let cabalProjectLines :: [String]
        cabalProjectLines = "packages: ." :
            [ "packages: " ++ toFilePath t
            | t <- tarballs
            ] 

    writeByteString (dir </> fromUnrootedFilePath "cabal.project")
        $ toUTF8BS $ unlines cabalProjectLines

    -- build dependencies, for one package at the time
    when (null tarballs) $ void $
        bracket acquire release $ \_ ->
            runProcessCheck tracer dir "cabal" ["v2-build", "--with-compiler", ghcPath gi, "--dependencies-only", "."]

    -- build packages concurrently
    _ <- runProcessCheck tracer dir "cabal" ["v2-haddock", "--with-compiler", ghcPath gi, "--haddock-hoogle", "-O0", "."]

    -- find, read, and parse hoogle.txt
    hoogle <- globHoogle tracer dir pkgId
    contents <- readByteString hoogle
    case parseFile contents of
        Right x  -> return x
        Left err -> die tracer err
  where
    acquire   = liftIO $ atomically (waitTSem buildSem)
    release _ = liftIO $ atomically (signalTSem buildSem)

-------------------------------------------------------------------------------
-- Hoogle utils
-------------------------------------------------------------------------------

globHoogle :: TracerPeu r w -> Path Absolute -> PackageIdentifier -> Peu r (Path Absolute)
globHoogle tracer dir (PackageIdentifier name _) = do
    found <- liftIO  $  glob (toFilePath dir ++ "/**/" ++ prettyShow name ++ ".txt")
    case found of
        [p] -> makeAbsoluteFilePath p
        []  -> die tracer $ "cannot find " ++ prettyShow name ++ ".txt (hoogle file)"
        _   -> die tracer $ "found multiple " ++ prettyShow name ++ ".txt (hoogle files)"
