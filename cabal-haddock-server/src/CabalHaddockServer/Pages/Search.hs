{-# LANGUAGE OverloadedStrings #-}
module CabalHaddockServer.Pages.Search (
    searchPage,
) where

import Lucid hiding (for_)
import Peura
import Prelude (even)

import qualified Hooglite
import qualified Language.Haskell.Lexer as L

import CabalHaddockServer.Routes
import CabalHaddockServer.TopPage

searchPage :: [PackageIdentifier] -> Hooglite.Database -> Maybe String -> Html ()
searchPage pis db q = doctypehtml_ $ do
    head_ $ do
        link_ [ rel_ "stylesheet", href_ $ dispRoute (RouteStatic (fromUnrootedFilePath "bootstrap.min.css")) ]
        title_ "Haddock Server"

    page_ $ do
        for_ q $ \q' -> do
            let q'' = Hooglite.parseQuery q'
            h2_ "Search"

            form_ [ class_ "row mb-3", action_ (dispRoute RouteSearch), method_ "GET"] $ do
                div_ [ class_ "col-md-10"] $ input_ [ name_ "q", class_ "form-control me-2", type_ "search", placeholder_ "search", value_ (fromString q') ] -- aria-label="Search"
                div_ [ class_ "col-md-2" ] $ button_ [ class_ "btn btn-primary", type_ "submit" ] "Search"

            p_ $ "Query was parsed as " *> code_ (toHtml (Hooglite.pretty q''))

            ifor_ (take 200 $ Hooglite.query db q'') $ \i (Hooglite.Entry pn ver mn name decl) -> do
                let pkgId = PackageIdentifier pn ver

                let dotToDash '.' = '-'
                    dotToDash c   = c

                let mkLink :: Maybe String -> Html () -> Html ()
                    mkLink anchor' t = a_ [ href_ $ dispRoute $ RoutePackageDocs pkgId $ fromUnrootedFilePath $ "/" ++ map dotToDash (prettyShow mn) ++ ".html" ++ maybe "" ("#"++) anchor'] t

                let anchor = case decl of
                        Hooglite.SigD {} -> "v:" ++ Hooglite.pretty name
                        Hooglite.ConD {} -> "v:" ++ Hooglite.pretty name
                        _                -> "t:" ++ Hooglite.pretty name
                      
                div_ [ class_ $ "row mb-1 border-bottom " <> if even i then "bg-light" else "bg-white" ] $ do
                    div_ [ class_ "col-md-8" ] $ do
                        for_ (L.lexerPass0 $ Hooglite.declarationSrc decl) $ \(tk, (_, str)) -> case tk of
                            L.Reservedid -> b_ $ toHtml str
                            _            -> case str of
                                _ | str == Hooglite.pretty name
                                         -> mkLink (Just anchor) $ toHtml str
                                -- selected pieces from https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/unicode_syntax.html
                                "->"     -> "→"
                                "=>"     -> "⇒"
                                "forall" -> "∀"
                                _        -> toHtml str

                    div_ [ class_ "col-md-4" ] $ small_ $ do
                        b_ $ a_ [ href_ $ dispRoute $ RoutePackageId pkgId ] $ toHtml $ prettyShow pkgId
                        toHtmlRaw (" &mdash; " :: String)
                        mkLink Nothing $ toHtml (prettyShow mn)
            
        h2_ "Local packages"

        ul_ $ for_ pis $ \pi -> li_ $
            a_ [ route_ $ RoutePackageId pi ] $ toHtml $ prettyShow pi

-------------------------------------------------------------------------------
-- Pretty-print haskell code
-------------------------------------------------------------------------------


