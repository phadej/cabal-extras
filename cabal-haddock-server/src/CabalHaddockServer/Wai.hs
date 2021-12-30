{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module CabalHaddockServer.Wai (
    application,
    Ctx (..),
    internalErrorResponse,
) where

import Peura

import Network.HTTP.Types.Status (Status, notFound404, ok200, status302, status500)

import qualified Data.List       as L
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Lucid
import qualified Network.Wai     as Wai
import qualified Hooglite

import CabalHaddockServer.DocsContents
import CabalHaddockServer.Pages.Error
import CabalHaddockServer.Pages.Index
import CabalHaddockServer.Pages.NotFound
import CabalHaddockServer.Pages.Package
import CabalHaddockServer.Pages.Redirect
import CabalHaddockServer.Pages.Search
import CabalHaddockServer.Routes
import CabalHaddockServer.Static

data Ctx = Ctx
    { ctxPackages :: Map PackageIdentifier DocsContents
    , ctxHackage  :: Set PackageIdentifier
    , ctxDatabase :: Hooglite.Database
    }

application :: Ctx -> Wai.Application
application ctx req res = case parseRoute (Wai.pathInfo req) of
    Just RouteIndex -> res indexResponse
    Just RouteSearch -> res searchResponse

    Just (RoutePackageId pkgId)
        | Just dc <- Map.lookup pkgId (ctxPackages ctx)
        -> res $ packageResponse dc

    Just (RoutePackageDocs pkgId fp)
        | Just dc <- Map.lookup pkgId (ctxPackages ctx)
        , Set.member fp (docsContentsFiles dc)
        -> res $ case docsContentsServe dc fp of
            DocsFileOnDisk fp' -> Wai.responseFile ok200
                [("Content-Type", contentType (takeExtension fp))]
                (toFilePath fp')
                Nothing
            DocsFileInMemory lbs -> Wai.responseLBS ok200
                [("Content-Type", contentType (takeExtension fp))]
                lbs

    Just (RoutePackageDocs pkgId _fp)
        | Set.member pkgId (ctxHackage ctx)
        -> res $ Wai.responseLBS
          status302
          [ ("Content-Type", "text/html; ; charset=utf-8")
          , ("Location", loc)
          ]
          (Lucid.renderBS $ redirectPage loc)
      where
        loc = fromString  "https://hackage.haskell.org" <> Wai.rawPathInfo req

    Just (RouteStatic file)
        | Just bs <- Map.lookup ("/" ++ (toUnrootedFilePath file)) staticFiles
        -> res $ Wai.responseLBS ok200
          [ ("Content-Type", contentType (takeExtension file)) ]
          (toLazy bs)

    _ -> res notFoundResponse
  where
    indexResponse      = page ok200 $ indexPage (Map.keys (ctxPackages ctx))
    searchResponse     = page ok200 $ searchPage (Map.keys (ctxPackages ctx)) (ctxDatabase ctx) q
    notFoundResponse   = page notFound404 $ notFoundPage (Wai.pathInfo req)
    packageResponse dc = page ok200 $ packagePage (Map.keys (ctxPackages ctx)) dc

    contentType (Just (FileExt "html")) = "text/html; charset=utf-8"
    contentType (Just (FileExt "css"))  = "text/css"
    contentType (Just (FileExt "js"))   = "text/javascript"
    contentType (Just (FileExt "png"))  = "image/png"
    contentType _                       = "application/octet-stream"

    q :: Maybe String
    q = fromUTF8BS <$> join (L.lookup "q" (Wai.queryString req))

internalErrorResponse :: Exception e => e -> Wai.Response
internalErrorResponse e = page status500 (internalErrorPage e)

page :: Status -> Lucid.Html () -> Wai.Response
page s l = Wai.responseLBS
    s
    [("Content-Type", "text/html; charset=utf-8")]
    (Lucid.renderBS l)
