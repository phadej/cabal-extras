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

import Control.Applicative          ((<**>))
import Control.Concurrent.STM
       (atomically, newTVarIO, readTVar, retry, writeTVar)
import Distribution.Parsec          (eitherParsec)

import qualified Options.Applicative             as O

import CabalDiff.Diff
import CabalDiff.Hoogle

import Paths_cabal_diff (version)

main :: IO ()
main = do
    opts <- O.execParser optsP'
    runPeu () $ doDiff opts
  where
    optsP' = O.info (optsP <**> O.helper <**> versionP) $ mconcat
        [ O.fullDesc
        , O.progDesc "Diff cabal package APIs"
        , O.header "cabal-diff"
        ]

    versionP = O.infoOption (showVersion version)
        $ O.long "version" <> O.help "Show version"

-------------------------------------------------------------------------------
-- Options parser
-------------------------------------------------------------------------------

-- TODO: commands to list package environments, their contents, delete, copy.
-- TODO: special . name for "package environment in this directory"
data Opts = Opts
    { _optCompiler :: FilePath
    , _optPackageA :: PackageIdentifier
    , _optPackageB :: PackageIdentifier
    }

optsP :: O.Parser Opts
optsP = Opts
    <$> O.strOption (O.short 'w' <> O.long "with-compiler" <> O.value "ghc" <> O.showDefault <> O.help "Specify compiler to use")
    <*> O.argument (O.eitherReader eitherParsec) (O.metavar "OLDPKG" <> O.help "old package")
    <*> O.argument (O.eitherReader eitherParsec) (O.metavar "NEWPKG" <> O.help "new package")

-------------------------------------------------------------------------------
-- Fake project
-------------------------------------------------------------------------------

doDiff :: Opts -> Peu () ()
doDiff (Opts withCompiler pkgA pkgB) =
    withSystemTempDirectory "cabal-diff-old" $ \dirA ->
    withSystemTempDirectory "cabal-diff-new" $ \dirB -> do
        let dirA' = dirA </> fromUnrootedFilePath (prettyShow pkgA)
        let dirB' = dirB </> fromUnrootedFilePath (prettyShow pkgB)

        buildLock <- liftIO $ newTVarIO False
        dbA' <- getPackageDB buildLock dirA dirA' pkgA
        dbB' <- getPackageDB buildLock dirB dirB' pkgB

        dbA <- wait dbA'
        dbB <- wait dbB'

        outputApiDiff (apiDiff dbA dbB)
  where
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

    getPackageDB buildLock dir dir' pkg = do
        x1 <- async $
            runProcessCheck dir "cabal" ["get", prettyShow pkg]
        x2 <- async $ do
            wait x1

            let acquire = liftIO $ atomically $ do
                    l <- readTVar buildLock
                    if l
                    then retry
                    else writeTVar buildLock True

                release = liftIO $ atomically $ writeTVar buildLock False

            bracket acquire (const release) $ \_ ->
                runProcessCheck dir' "cabal" ["v2-build", "--with-compiler", withCompiler, "--dependencies-only"]

        x3 <- async $ do
            wait x2
            runProcessCheck dir' "cabal" ["v2-haddock", "--with-compiler", withCompiler, "--haddock-hoogle"] -- TODO: -O0

        async $ do
            wait x3

            hoogle <- globHoogle dir' pkg
            contents <- readByteString hoogle
            case parseFile contents of
                Right x  -> return x
                Left err -> do
                    putError err
                    exitFailure

-------------------------------------------------------------------------------
-- Async
-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
-- temporary
-------------------------------------------------------------------------------


