{-# LANGUAGE CPP          #-}
{-# LANGUAGE RankNTypes   #-}
{-# OPTIONS -Wno-missing-fields #-}
module CabalIfaceQuery.GHC (
    -- * DynFlags
    getDynFlags,
    -- * Other
    easyReadBinIface,
    ghcShow,
    ghcShowIfaceClsInst,
) where

import Peura

import GHC.Driver.Session       (targetProfile)
import GHC.Iface.Binary         (CheckHiWay (CheckHiWay), TraceBinIFace (QuietBinIFace), readBinIface)
import GHC.Iface.Syntax         (IfaceClsInst (..), IfaceTyCon (..))
import GHC.Types.Name           (nameModule_maybe)
import GHC.Types.Name.Cache     (NameCache)
import GHC.Unit.Module.ModIface (ModIface)

import CabalIfaceQuery.GHC.DynFlags
import CabalIfaceQuery.GHC.Show

-------------------------------------------------------------------------------
-- "Easy" interface
-------------------------------------------------------------------------------

easyReadBinIface :: DynFlags -> NameCache -> Path Absolute -> IO ModIface
easyReadBinIface dflags nc path =
    readBinIface (targetProfile dflags) nc CheckHiWay QuietBinIFace (toFilePath path)

-------------------------------------------------------------------------------
-- Showing
-------------------------------------------------------------------------------

ghcShowIfaceClsInst :: DynFlags -> IfaceClsInst -> String
ghcShowIfaceClsInst dflags ifci = unwords $
    "instance" :
    ghcShow dflags (ifInstCls ifci) :
    [ maybe "_" (ghcShow dflags) tyCon
    | tyCon <- ifInstTys ifci
    ] ++
    extras
  where
    extras = case ifInstTys ifci of
        (Just (IfaceTyCon n _) : _) ->
            maybe [] (\m -> ["(from " ++ ghcShow dflags m ++ ")"]) (nameModule_maybe n)
        _                           -> []

