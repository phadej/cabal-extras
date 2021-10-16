{-# LANGUAGE OverloadedStrings #-}
module CabalHaddockServer.Pages.Package (
    packagePage,
) where

import Lucid hiding (for_)
import Peura

import qualified Data.Map.Strict         as Map
import qualified Distribution.ModuleName as C

import CabalHaddockServer.DocsContents
import CabalHaddockServer.Hoogle
import CabalHaddockServer.Routes

packagePage :: DocsContents -> Html ()
packagePage dc = doctypehtml_ $ do
    head_ $ do
        title_ $ toHtml $ prettyShow pkgId ++ " - Haddock Server"

    body_ $ do
        h1_ $ toHtml $ prettyShow pkgId ++ " - cabal-haddock-server"

        ul_ $ for_ (Map.keys $ apiModules $ docsContentsApi dc) $ \mn -> li_ $
            a_ [ route_ $ RoutePackageDocs pkgId $ mnToPath mn ] $ toHtml $ prettyShow mn
  where
    pkgId = apiPackageId $ docsContentsApi dc

    mnToPath :: C.ModuleName -> Path Unrooted
    mnToPath mn = fromUnrootedFilePath $
        map f (prettyShow mn) ++ ".html"
      where
        f '.' = '-'
        f c   = c
