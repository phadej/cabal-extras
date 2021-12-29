{-# LANGUAGE OverloadedStrings #-}
module CabalHaddockServer.Routes (
    Route (..),
    parseRoute,
    dispRoute,
    route_,
) where

import Peura

import Data.List           (intercalate)
import Distribution.Parsec (eitherParsec)

import qualified Data.Text  as T
import qualified Lucid.Base as L

-- | Routes in the web application.
data Route
    = RouteIndex
      -- ^ @/@ index page.
    | RouteSearch
      -- ^ @/search@ search page
    | RoutePackageId PackageIdentifier
      -- ^ @/package/<pkg-id>@
    | RoutePackageDocs PackageIdentifier (Path Unrooted)
      -- ^ @/package/<pkg-id>/docs/<path>@ some file
    | RouteStatic (Path Unrooted)
      -- ^ @/static/a.file.css@ static files
  deriving Show

parseRoute :: [Text] -> Maybe Route
parseRoute [] = Just RouteIndex
parseRoute ["search"] = Just RouteSearch
parseRoute ["package", pi]
    | Right pi' <- eitherParsec (T.unpack pi)
    = Just (RoutePackageId pi')
parseRoute ("package" :  pi : "docs" : rest)
    | Right pi' <- eitherParsec (T.unpack pi)
    , not (null rest)
    , let p = fromUnrootedFilePath $ intercalate "/" $ map T.unpack rest
    = Just (RoutePackageDocs pi' p)
parseRoute ["static", file]
    = Just (RouteStatic (fromUnrootedFilePath (T.unpack file)))
parseRoute _  = Nothing

dispRoute :: Route -> Text
dispRoute RouteIndex =
    "/"
dispRoute RouteSearch =
    "/search"
dispRoute (RoutePackageId pkgId) =
    "/package/" <> fromString (prettyShow pkgId)
dispRoute (RoutePackageDocs pkgId p) =
    "/package/" <> fromString (prettyShow pkgId) <>
    "/docs/" <> fromString (toUnrootedFilePath p)
dispRoute (RouteStatic file) =
    "/static/" <> fromString (toUnrootedFilePath file)

route_ :: Route -> L.Attribute
route_ = L.makeAttribute "href" . dispRoute
