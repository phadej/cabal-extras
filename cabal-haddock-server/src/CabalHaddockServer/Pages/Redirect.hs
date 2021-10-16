{-# LANGUAGE OverloadedStrings #-}
module CabalHaddockServer.Pages.Redirect (
    redirectPage,
) where

import Lucid
import Peura

redirectPage :: ByteString -> Html ()
redirectPage bs = doctypehtml_ $ do
    let t = decodeUtf8Lenient bs

    head_ $ do
        title_ "Redirecting..."

    body_ $ p_ $ do
        "Redirecting to "
        a_ [ href_ t ] $ toHtml t

