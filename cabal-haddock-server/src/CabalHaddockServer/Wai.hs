{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module CabalHaddockServer.Wai (
    application,
    Ctx (..),
    internalErrorResponse,
) where

import Peura

import Network.HTTP.Types.Status (Status, notFound404, ok200, status302, status500)

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Lucid
import qualified Network.Wai     as Wai

import CabalHaddockServer.DocsContents
import CabalHaddockServer.Pages.Error
import CabalHaddockServer.Pages.Index
import CabalHaddockServer.Pages.NotFound
import CabalHaddockServer.Pages.Package
import CabalHaddockServer.Pages.Redirect
import CabalHaddockServer.Routes

data Ctx = Ctx
    { ctxPackages :: Map PackageIdentifier DocsContents
    , ctxHackage  :: Set PackageIdentifier
    }

application :: Ctx -> Wai.Application
application ctx req res = case parseRoute (Wai.pathInfo req) of
    Just RouteIndex -> res indexResponse

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
          [ ("Content-Type", "text/html")
          , ("Location", loc)
          ]
          (Lucid.renderBS $ redirectPage loc)
      where
        loc = fromString  "https://hackage.haskell.org" <> Wai.rawPathInfo req

    _ -> res notFoundResponse
  where
    indexResponse      = page ok200 $ indexPage (Map.keys (ctxPackages ctx))
    notFoundResponse   = page notFound404 $ notFoundPage (Wai.pathInfo req)
    packageResponse dc = page ok200 $ packagePage dc

    contentType (Just (FileExt "html")) = "text/html"
    contentType (Just (FileExt "css"))  = "text/css"
    contentType (Just (FileExt "js"))   = "text/javascript"
    contentType (Just (FileExt "png"))  = "image/png"
    contentType _                       = "application/octet-stream"

internalErrorResponse :: Exception e => e -> Wai.Response
internalErrorResponse e = page status500 (internalErrorPage e)

page :: Status -> Lucid.Html () -> Wai.Response
page s l = Wai.responseLBS
    s
    [("Content-Type", "text/html")]
    (Lucid.renderBS l)
