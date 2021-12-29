{-# LANGUAGE OverloadedStrings #-}
module CabalHaddockServer.Pages.NotFound (
    notFoundPage,
) where

import Lucid
import Peura

import CabalHaddockServer.Routes
import CabalHaddockServer.TopPage

notFoundPage :: [Text] -> Html ()
notFoundPage pi = doctypehtml_ $ do
    head_ $ do
        link_ [ rel_ "stylesheet", href_ $ dispRoute (RouteStatic (fromUnrootedFilePath "bootstrap.min.css")) ]
        title_ "Not found"

    page_ $ do
        p_ $ do
            code_ $ toHtml $ foldMap ("/" <>) pi
            " not found"

        p_ $ a_ [ route_ RouteIndex ] $ "To main page"

