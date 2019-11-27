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

import Data.List (stripPrefix)
import Control.Applicative         ((<**>))
import Control.Concurrent.STM      (atomically)
import Control.Concurrent.STM.TSem (TSem, newTSem, signalTSem, waitTSem)
import Distribution.Parsec         (eitherParsec)

import qualified Data.Binary         as Binary
import qualified Options.Applicative as O

import CabalDiff.Diff
import CabalDiff.Hoogle

import Paths_cabal_diff (version)


main :: IO ()
main = do
    opts <- O.execParser optsP'
    runPeu () $ doDiff opts
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

data Opts = Opts
    { _optCompiler :: FilePath
    , _optPkgName  :: PackageName
    , _optVerA     :: DiffVersion
    , _optVerB     :: DiffVersion
    }
  deriving (Show)

data DiffVersion
    = HackageVersion Version
    | LocalVersion
  deriving (Show)

optsP :: O.Parser Opts
optsP = Opts
    <$> O.strOption (O.short 'w' <> O.long "with-compiler" <> O.value "ghc" <> O.showDefault <> O.help "Specify compiler to use")
    <*> O.argument (O.eitherReader eitherParsec) (O.metavar "PKGNAME" <> O.help "package name")
    <*> O.argument readDiffVersion (O.metavar "OLDVER" <> O.help "new version")
    <*> O.argument readDiffVersion (O.metavar "NEWVER" <> O.help "new package")

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

doDiff :: Opts -> Peu () ()
doDiff (Opts withCompiler pn pkgVerA pkgVerB) = do
    buildSem <- liftIO $ atomically (newTSem 1)

    dbA' <- async $ getHoogleTxt withCompiler buildSem pn pkgVerA
    dbB' <- async $ getHoogleTxt withCompiler buildSem pn pkgVerB

    dbA <- wait dbA'
    dbB <- wait dbB'

    outputApiDiff (apiDiff dbA dbB)

getHoogleTxt :: FilePath -> TSem -> PackageName -> DiffVersion -> Peu () API
getHoogleTxt withCompiler buildSem pn LocalVersion =
    getLocalHoogleTxt withCompiler buildSem pn
getHoogleTxt withCompiler buildSem pn (HackageVersion ver) =
    getHackageHoogleTxt withCompiler buildSem (PackageIdentifier pn ver)

-------------------------------------------------------------------------------
-- Local
-------------------------------------------------------------------------------

getLocalHoogleTxt
    :: FilePath           -- ^ compiler to use
    -> TSem               -- ^ is someone building dependencies
    -> PackageName
    -> Peu () API
getLocalHoogleTxt withCompiler buildSem pn = do
    -- TODO: cache based on sha256 of tarball
    cwd <- getCurrentDirectory
    withSystemTempDirectory "cabal-diff" $ \dir -> do
        _ <- runProcessCheck cwd "cabal"
            [ "sdist"
            , "--builddir=" ++ toFilePath (dir </> fromUnrootedFilePath "dist-newstyle")
            , "pkg:" ++ prettyShow pn
            ]

        -- we don't know the local package version, so we glob for it.
        -- Without cabal.project we'd need to glob for *.cabal file anyway.
        res  <- liftIO $ glob (toFilePath dir ++ "/dist-newstyle/sdist/" ++ prettyShow pn ++ "-*.tar.gz")
        (tarball, pkgId) <- elaborateSdistLocation res

        -- untar
        _ <- runProcessCheck dir "tar" ["-xzf", toFilePath tarball]
    
        -- directory we expect things got untarred to
        let dir' = dir </> fromUnrootedFilePath (prettyShow pkgId)

        -- build hoogle.txt
        buildHoogleTxt withCompiler buildSem pkgId dir'
  where
    elaborateSdistLocation :: [FilePath] -> Peu r (Path Absolute, PackageIdentifier)
    elaborateSdistLocation [fp] = do
        fp' <- makeAbsoluteFilePath fp
        let fn = toUnrootedFilePath (takeFileName fp')
        pid <- elaboratePkgId fn
        return (fp', pid)

    elaborateSdistLocation [] = do
        putError "Cannot find sdist tarball"
        exitFailure
    elaborateSdistLocation fps = do
        putError $ "Found multiple sdist tarballs " ++ show fps
        exitFailure

    elaboratePkgId :: String -> Peu r PackageIdentifier
    elaboratePkgId str = case stripSuffix ".tar.gz" str of
        Nothing -> do
            putError $ "tarball path doesn't end with .tar.gz -- " ++ str
            exitFailure

        Just pfx -> case eitherParsec pfx of
            Right pkgId -> return pkgId
            Left  err   -> putError err *> exitFailure

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix sfx str = fmap reverse (stripPrefix (reverse sfx) (reverse str))

-------------------------------------------------------------------------------
-- Hackage
-------------------------------------------------------------------------------

getHackageHoogleTxt
    :: FilePath           -- ^ compiler to use
    -> TSem               -- ^ is someone building dependencies
    -> PackageIdentifier
    -> Peu () API
getHackageHoogleTxt withCompiler buildSem pkg = do
    cacheDir' <- makeAbsolute cacheDir
    createDirectoryIfMissing True  cacheDir'

    -- todo: should include GHC version
    let cacheFile = cacheDir' </> fromUnrootedFilePath (prettyShow pkg)

    exists <- doesFileExist cacheFile
    if exists
    then do
        putInfo $ "Using cached hoogle.txt for " ++ prettyShow pkg
        liftIO $ Binary.decodeFile (toFilePath cacheFile)
    else do
        api <- getHackageHoogleTxt' withCompiler buildSem pkg
        liftIO $ Binary.encodeFile (toFilePath cacheFile) api
        return api

getHackageHoogleTxt'
    :: FilePath
    -> TSem
    -> PackageIdentifier
    -> Peu () API
getHackageHoogleTxt' withCompiler buildSem pkgId =
    withSystemTempDirectory "cabal-diff" $ \dir -> do
        -- fetch the package
        _ <- runProcessCheck dir "cabal" ["get", prettyShow pkgId]

        -- directory cabal got
        let dir' = dir </> fromUnrootedFilePath (prettyShow pkgId)

        -- build hoogle.txt
        buildHoogleTxt withCompiler buildSem pkgId dir'

buildHoogleTxt
    :: FilePath
    -> TSem
    -> PackageIdentifier
    -> Path Absolute -> Peu r API
buildHoogleTxt withCompiler buildSem pkgId dir = do
    -- build dependencies, for one package at the time
    _ <- bracket acquire release $ \_ ->
        runProcessCheck dir "cabal" ["v2-build", "--with-compiler", withCompiler, "--dependencies-only", "."]

    -- build packages concurrently
    _ <- runProcessCheck dir "cabal" ["v2-haddock", "--with-compiler", withCompiler, "--haddock-hoogle", "-O0", "."]

    -- find, read, and parse hoogle.txt
    hoogle <- globHoogle dir pkgId
    contents <- readByteString hoogle
    case parseFile contents of
        Right x  -> return x
        Left err -> do
            putError err
            exitFailure
  where
    acquire   = liftIO $ atomically (waitTSem buildSem)
    release _ = liftIO $ atomically (signalTSem buildSem)

-------------------------------------------------------------------------------
-- Hoogle utils
-------------------------------------------------------------------------------

globHoogle :: Path Absolute -> PackageIdentifier -> Peu r (Path Absolute)
globHoogle dir (PackageIdentifier name _) = do
    found <- liftIO  $  glob (toFilePath dir ++ "/**/" ++ prettyShow name ++ ".txt")
    case found of
        [p] -> makeAbsoluteFilePath p
        []  -> do
            putError $ "cannot find " ++ prettyShow name ++ ".txt (hoogle file)"
            exitFailure
        _   -> do
            putError $ "found multiple " ++ prettyShow name ++ ".txt (hoogle files)"
            exitFailure
