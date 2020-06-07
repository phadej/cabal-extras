{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE RankNTypes   #-}
{-# OPTIONS -Wno-missing-fields #-}
module CabalIfaceQuery.GHC (
    -- * DynFlags
    getDynFlags,
    -- * Other
    easyReadBinIface,
    makeNameCacheUpdater,
    ghcShow,
    ghcShowIfaceClsInst,
) where

import Peura

import CabalIfaceQuery.GHC.DynFlags
import CabalIfaceQuery.GHC.Show
import CabalIfaceQuery.GHC.NameCacheUpdater
import CabalIfaceQuery.GHC.ReadBinIface

import qualified CabalIfaceQuery.GHC.All as GHC

-------------------------------------------------------------------------------
-- "Easy" interface
-------------------------------------------------------------------------------

easyReadBinIface :: DynFlags -> NameCacheUpdater -> Path Absolute -> IO GHC.ModIface
easyReadBinIface dflags ncu path =
    readBinIface_  dflags CheckHiWay QuietBinIFaceReading (toFilePath path) ncu

-------------------------------------------------------------------------------
-- Showing
-------------------------------------------------------------------------------

ghcShowIfaceClsInst :: DynFlags -> GHC.IfaceClsInst -> String
ghcShowIfaceClsInst dflags ifci = unwords $
    "instance" :
    ghcShow dflags (GHC.ifInstCls ifci) :
    [ maybe "_" (ghcShow dflags) tyCon
    | tyCon <- GHC.ifInstTys ifci
    ] ++
    extras
  where
    extras = case GHC.ifInstTys ifci of
        (Just (GHC.IfaceTyCon n _) : _) ->
            maybe [] (\m -> ["(from " ++ ghcShow dflags m ++ ")"]) (GHC.nameModule_maybe n)
        _                           -> []

