module CabalIfaceQuery.GHC.Show (
    ghcShow,
) where

import Prelude (String, (.))

import GHC.Driver.Session   (DynFlags, initSDocContext)
import GHC.Utils.Outputable (Outputable, defaultDumpStyle, ppr, showSDocOneLine)

ghcShow :: Outputable t => DynFlags -> t -> String
ghcShow dflags = showSDocOneLine (initSDocContext dflags defaultDumpStyle) . ppr
