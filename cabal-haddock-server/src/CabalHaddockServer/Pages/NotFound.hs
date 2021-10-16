{-# LANGUAGE OverloadedStrings #-}
module CabalHaddockServer.Pages.NotFound (
    notFoundPage,
) where

import Lucid
import Peura

import CabalHaddockServer.Routes

notFoundPage :: [Text] -> Html ()
notFoundPage pi = doctypehtml_ $ do
    head_ $ do
        title_ "Not found"

    body_ $ do
        p_ $ do
            code_ $ toHtml $ foldMap ("/" <>) pi
            " not found"

        p_ $ a_ [ route_ RouteIndex ] $ "To main page"

