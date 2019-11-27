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
    , _optVerA     :: Version
    , _optVerB     :: Version
    }

optsP :: O.Parser Opts
optsP = Opts
    <$> O.strOption (O.short 'w' <> O.long "with-compiler" <> O.value "ghc" <> O.showDefault <> O.help "Specify compiler to use")
    <*> O.argument (O.eitherReader eitherParsec) (O.metavar "PKGNAME" <> O.help "package name")
    <*> O.argument (O.eitherReader eitherParsec) (O.metavar "OLDVER" <> O.help "new version")
    <*> O.argument (O.eitherReader eitherParsec) (O.metavar "NEWVER" <> O.help "new package")

-------------------------------------------------------------------------------
-- cache and locking
-------------------------------------------------------------------------------

cacheDir :: Path XdgCache
cacheDir = root </> fromUnrootedFilePath "cabal-diff"

{-
withLukko :: Peu r a -> Peu r a
withLukko m = do
    cacheDir' <- makeAbsolute cacheDir
    let lock = cacheDir' </> fromUnrootedFilePath "lock"
    bracket (acquire lock) release (const m)
  where
    acquire :: Path Absolute -> Peu r Lukko.FD
    acquire lock = liftIO $ do
        fd <- Lukko.fdOpen (toFilePath lock)
        Lukko.fdLock fd Lukko.ExclusiveLock `onException` Lukko.fdClose fd
        return fd

    release :: Lukko.FD -> Peu r ()
    release fd = liftIO $ Lukko.fdUnlock fd `finally` Lukko.fdClose fd
-}

-------------------------------------------------------------------------------
-- Diffing
-------------------------------------------------------------------------------

doDiff :: Opts -> Peu () ()
doDiff (Opts withCompiler pn pkgVerA pkgVerB) = do
    buildSem <- liftIO $ atomically (newTSem 1)

    dbA' <- async $ getHackageHoogleTxt withCompiler buildSem (PackageIdentifier pn pkgVerA)
    dbB' <- async $ getHackageHoogleTxt withCompiler buildSem (PackageIdentifier pn pkgVerB)

    dbA <- wait dbA'
    dbB <- wait dbB'

    outputApiDiff (apiDiff dbA dbB)

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
getHackageHoogleTxt' withCompiler buildSem pkg =
    withSystemTempDirectory "cabal-diff" $ \dir -> do
        -- fetch the package
        _ <- runProcessCheck dir "cabal" ["get", prettyShow pkg]

        -- directory cabal got
        let dir' = dir </> fromUnrootedFilePath (prettyShow pkg)

        -- build dependencies, for one package at the time
        _ <- bracket acquire release $ \_ ->
            runProcessCheck dir' "cabal" ["v2-build", "--with-compiler", withCompiler, "--dependencies-only"]

        -- build packages concurrently
        _ <- runProcessCheck dir' "cabal" ["v2-haddock", "--with-compiler", withCompiler, "--haddock-hoogle", "-O0"]

        hoogle <- globHoogle dir' pkg
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
