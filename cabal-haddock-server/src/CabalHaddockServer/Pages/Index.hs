{-# LANGUAGE OverloadedStrings #-}
module CabalHaddockServer.Pages.Index (
    indexPage,
) where

import Lucid hiding (for_)
import Peura

import CabalHaddockServer.Routes

indexPage :: [PackageIdentifier] -> Html ()
indexPage pis = doctypehtml_ $ do
    head_ $ do
        title_ "Haddock Server"

    body_ $ do
        h1_ "cabal-haddock-server"

        ul_ $ for_ pis $ \pi -> li_ $
            a_ [ route_ $ RoutePackageId pi ] $ toHtml $ prettyShow pi
