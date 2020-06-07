{-# LANGUAGE CPP #-}
module CabalIfaceQuery.GHC.DynFlags (
    DynFlags,
    getDynFlags,
) where

import Peura

import DynFlags (DynFlags, defaultDynFlags)

#if MIN_VERSION_ghc(8,10,0)
import SysTools   (lazyInitLlvmConfig, initSysTools)
#elif MIN_VERSION_ghc(8,6,0)
import SysTools   (initLlvmConfig, initSysTools)
#else
import SysTools   (initLlvmTargets, initSysTools)
#endif

-- | Get 'DynFlags' given 'GhcInfo' for this GHC.
getDynFlags :: GhcInfo -> Peu r DynFlags
getDynFlags ghcInfo = do
    unless (VERSION_ghc == prettyShow (ghcVersion ghcInfo)) $ do
        putError $ "Compiler version mismatch: " ++
            VERSION_ghc ++ " /= " ++ prettyShow (ghcVersion ghcInfo)
        exitFailure

#if MIN_VERSION_ghc(8,8,0)
    let libDir = toFilePath $ ghcLibDir ghcInfo
#else
    let libDir = Just $ toFilePath $ ghcLibDir ghcInfo
#endif
    settings <- liftIO $ initSysTools libDir
#if MIN_VERSION_ghc(8,10,0)
    llvmConfig <- liftIO $ lazyInitLlvmConfig libDir
#elif MIN_VERSION_ghc(8,6,0)
    llvmConfig <- liftIO $ initLlvmConfig libDir
#else
    llvmConfig <- liftIO $ initLlvmTargets libDir
#endif
    return $ defaultDynFlags settings llvmConfig
