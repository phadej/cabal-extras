{-# LANGUAGE CPP #-}
module CabalIfaceQuery.GHC.DynFlags (
    DynFlags,
    getDynFlags,
) where

import Peura

import GHC.Driver.Session (DynFlags, defaultDynFlags)
import GHC.SysTools       (initSysTools)

-- | Get 'DynFlags' given 'GhcInfo' for this GHC.
getDynFlags :: TracerPeu r w -> GhcInfo -> Peu r DynFlags
getDynFlags tracer ghcInfo = do
    unless (VERSION_ghc == prettyShow (ghcVersion ghcInfo)) $ do
        die tracer $ "Compiler version mismatch: " ++
            VERSION_ghc ++ " /= " ++ prettyShow (ghcVersion ghcInfo)

    let libDir = toFilePath $ ghcLibDir ghcInfo
    settings <- liftIO $ initSysTools libDir
    return $ defaultDynFlags settings
