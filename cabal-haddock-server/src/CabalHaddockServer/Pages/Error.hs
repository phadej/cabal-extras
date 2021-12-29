{-# LANGUAGE OverloadedStrings #-}
module CabalHaddockServer.Pages.Error (
    internalErrorPage,
) where

import Lucid
import Peura

import CabalHaddockServer.Routes
import CabalHaddockServer.TopPage

internalErrorPage :: Exception e => e -> Html ()
internalErrorPage e = doctypehtml_ $ do
    link_ [ rel_ "stylesheet", href_ $ dispRoute (RouteStatic (fromUnrootedFilePath "bootstrap.min.css")) ]
    head_ $ do
        title_ "Exception"

    page_ $ do
        p_ $ pre_ $ code_ $ toHtml $ displayException e
        p_ $ a_ [ route_ RouteIndex ] $ "To main page"
