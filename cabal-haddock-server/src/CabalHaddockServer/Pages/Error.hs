{-# LANGUAGE OverloadedStrings #-}
module CabalHaddockServer.Pages.Error (
    internalErrorPage,
) where

import Lucid
import Peura

import CabalHaddockServer.Routes

internalErrorPage :: Exception e => e -> Html ()
internalErrorPage e = doctypehtml_ $ do
    head_ $ do
        title_ "Exception"

    body_ $ do
        p_ $ pre_ $ code_ $ toHtml $ displayException e
        p_ $ a_ [ route_ RouteIndex ] $ "To main page"
