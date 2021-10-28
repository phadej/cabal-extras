{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Copyright: Oleg Grenrus
-- License: GPL-3.0-or-later
module CabalHaddockServer.Main (main) where

import Peura

import System.TimeManager (TimeoutThread)

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import qualified Cabal.Index              as CI
import qualified Network.Wai.Handler.Warp as Warp

import CabalHaddockServer.DocsContents
import CabalHaddockServer.Hoogle
import CabalHaddockServer.Options
import CabalHaddockServer.Wai
import CabalHaddockServer.Warning

main :: IO ()
main = do
    opts <- parseOptions
    tracer <- makeTracerPeu @(V1 W) (optTracer opts defaultTracerOptions)
    runPeu tracer () $ do
        meta <- cachedHackageMetadata tracer
        let hackagePkgIds :: Set PackageIdentifier
            hackagePkgIds = Set.fromList
                [ PackageIdentifier pn v
                | (pn, pi) <- Map.toList meta
                , (v, _)   <- Map.toList (CI.piVersions pi)
                ]

        dcs <- for (optTarballs opts) $ \fspath -> do
            tarball <- makeAbsolute fspath >>= canonicalizePath
            putInfo tracer $ show tarball

            isFile <- doesFileExist tarball
            unpacked <- if isFile
            then die tracer "files are not supported yet"
            else do
                isDir <- doesDirectoryExist tarball
                if isDir
                then return tarball
                else die tracer $ toFilePath tarball ++ " is not a file or a directory"

            readDocsContents tracer unpacked

        let contents :: Map PackageIdentifier DocsContents
            contents = Map.fromList
                [ (apiPackageId $ docsContentsApi dc, dc)
                | dc <- catMaybes dcs
                ]

        let port = 13333

        putInfo tracer $ "Starting server at http://localhost:" ++ show port
        withRunInIO $ \runInIO -> do
            let warpOnException _mreq sexc@(SomeException exc) =
                    case fromException sexc :: Maybe TimeoutThread of
                        Just _  -> return ()
                        Nothing -> runInIO $ putError tracer $ displayException exc

            let warpExceptionResponse (SomeException exc) =
                    internalErrorResponse exc

            let settings = Warp.defaultSettings
                    & Warp.setPort port
                    & Warp.setOnException warpOnException
                    & Warp.setOnExceptionResponse warpExceptionResponse

            Warp.runSettings settings $ application Ctx
                { ctxPackages = contents
                , ctxHackage  = hackagePkgIds
                }
