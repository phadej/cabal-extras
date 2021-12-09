{-# LANGUAGE OverloadedStrings #-}
module CabalHaddockServer.Pages.Package (
    packagePage,
) where

import Lucid hiding (for_)
import Peura

import qualified Data.Map.Strict         as Map
import qualified Data.Set                as Set
import qualified Distribution.ModuleName as C

import CabalHaddockServer.DocsContents
import CabalHaddockServer.Hoogle
import CabalHaddockServer.Routes

packagePage :: DocsContents -> Html ()
packagePage dc = doctypehtml_ $ do
    head_ $ do
        title_ $ toHtml $ prettyShow pkgId ++ " - Haddock Server"
        when (Set.member (fromUnrootedFilePath "quick-jump.css") (docsContentsFiles dc)) $ do
            link_ [ rel_ "stylesheet", type_ "text/css", href_ $ "/package/" <> fromString (prettyShow pkgId) <> "/docs/quick-jump.css" ]

    body_ $ do
        h1_ $ toHtml $ prettyShow pkgId ++ " - cabal-haddock-server"

        ul_ $ for_ (Map.keys $ apiModules $ docsContentsApi dc) $ \mn -> li_ $
            a_ [ route_ $ RoutePackageDocs pkgId $ mnToPath mn ] $ toHtml $ prettyShow mn

        when (Set.member (fromUnrootedFilePath "quick-jump.min.js") (docsContentsFiles dc)) $ do
            script_
                [ src_ $ "/package/" <> fromString (prettyShow pkgId) <> "/docs/quick-jump.min.js"
                , type_ "text/javascript"
                ]
                ("" :: Text)

            script_ [ type_ "text/javascript" ] $
                "quickNav.init('/package/" ++ prettyShow pkgId ++ "/docs', function(toggle) {var t = document.getElementById('quickjump-trigger');if (t) {t.onclick = function(e) { e.preventDefault(); toggle(); };}});"
  where

    pkgId = apiPackageId $ docsContentsApi dc

    mnToPath :: C.ModuleName -> Path Unrooted
    mnToPath mn = fromUnrootedFilePath $
        map f (prettyShow mn) ++ ".html"
      where
        f '.' = '-'
        f c   = c
