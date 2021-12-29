{-# LANGUAGE OverloadedStrings #-}
module CabalHaddockServer.Pages.Index (
    indexPage,
) where

import Lucid hiding (for_)
import Peura

import CabalHaddockServer.Routes
import CabalHaddockServer.TopPage

indexPage :: [PackageIdentifier] -> Html ()
indexPage pis = doctypehtml_ $ do
    head_ $ do
        link_ [ rel_ "stylesheet", href_ $ dispRoute (RouteStatic (fromUnrootedFilePath "bootstrap.min.css")) ]
        title_ "Haddock Server"

    page_ $ do
        h2_ "Local packages"

        ul_ $ for_ pis $ \pi -> li_ $
            a_ [ route_ $ RoutePackageId pi ] $ toHtml $ prettyShow pi
