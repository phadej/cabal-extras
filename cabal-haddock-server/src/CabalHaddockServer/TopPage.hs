module CabalHaddockServer.TopPage where

import Lucid
import Peura

import CabalHaddockServer.Routes

page_ :: Html () -> Html ()
page_ body = body_ [ class_ "bg-light" ] $ do
    nav_ [ class_ "navbar navbar-light bg-dark mb-3" ] $ div_ [ class_ "container-fluid" ] $ do
        a_ [ class_ "navbar-brand text-light", href_ (dispRoute RouteIndex) ] "Cabal Haddock Server"
    
        form_ [ class_ "d-flex", action_ (dispRoute RouteSearch), method_ "GET"] $ do
            input_ [ name_ "q", class_ "form-control me-2", type_ "search", placeholder_ "search" ] -- aria-label="Search"
            button_ [ class_ "btn btn-primary", type_ "submit" ] "Search"

    div_ [ class_ "container" ] $ div_ [ class_ "row" ] $ div_ [ class_ "col-md-12" ] body
