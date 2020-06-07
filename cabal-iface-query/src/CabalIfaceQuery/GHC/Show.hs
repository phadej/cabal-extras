module CabalIfaceQuery.GHC.Show (
    ghcShow,
) where

import Prelude    (String, (.))

import DynFlags   (DynFlags)
import Outputable (Outputable, ppr, showSDocOneLine)

ghcShow :: Outputable t => DynFlags -> t -> String
ghcShow dflags = showSDocOneLine dflags . ppr
