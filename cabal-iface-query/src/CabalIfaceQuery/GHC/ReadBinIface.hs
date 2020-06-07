{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
module CabalIfaceQuery.GHC.ReadBinIface (
    -- * Reading
    readBinIface_,
    -- * Some types
    TraceBinIFaceReading (..),
    CheckHiWay (..),
) where


#if MIN_VERSION_ghc(8,11,0)
#error "Too recent GHC"

#elif MIN_VERSION_ghc(8,10,0)
import Prelude hiding ((<>))
import BinIface

import IfaceEnv
import HscTypes
import DynFlags
import Panic
import Binary
import SrcLoc
import ErrUtils
import Outputable
import GHC.Platform
import Constants

import Data.Word
import Control.Monad

readBinIface_ :: DynFlags -> CheckHiWay -> TraceBinIFaceReading -> FilePath
              -> NameCacheUpdater
              -> IO ModIface
readBinIface_ dflags checkHiWay traceBinIFaceReading hi_path ncu = do
    let printer :: SDoc -> IO ()
        printer = case traceBinIFaceReading of
                      TraceBinIFaceReading -> \sd ->
                          putLogMsg dflags
                                    NoReason
                                    SevOutput
                                    noSrcSpan
                                    (defaultDumpStyle dflags)
                                    sd
                      QuietBinIFaceReading -> \_ -> return ()

        wantedGot :: String -> a -> a -> (a -> SDoc) -> IO ()
        wantedGot what wanted got ppr' =
            printer (text what <> text ": " <>
                     vcat [text "Wanted " <> ppr' wanted <> text ",",
                           text "got    " <> ppr' got])

        errorOnMismatch :: (Eq a, Show a) => String -> a -> a -> IO ()
        errorOnMismatch what wanted got =
            -- This will be caught by readIface which will emit an error
            -- msg containing the iface module name.
            when (wanted /= got) $ throwGhcExceptionIO $ ProgramError
                         (what ++ " (wanted " ++ show wanted
                               ++ ", got "    ++ show got ++ ")")
    bh <- Binary.readBinMem hi_path

    -- Read the magic number to check that this really is a GHC .hi file
    -- (This magic number does not change when we change
    --  GHC interface file format)
    magic <- get bh
    wantedGot "Magic" (binaryInterfaceMagic dflags) magic ppr
    errorOnMismatch "magic number mismatch: old/corrupt interface file?"
        (binaryInterfaceMagic dflags) magic

    -- Note [dummy iface field]
    -- read a dummy 32/64 bit value.  This field used to hold the
    -- dictionary pointer in old interface file formats, but now
    -- the dictionary pointer is after the version (where it
    -- should be).  Also, the serialisation of value of type "Bin
    -- a" used to depend on the word size of the machine, now they
    -- are always 32 bits.
    if wORD_SIZE dflags == 4
        then do _ <- Binary.get bh :: IO Word32; return ()
        else do _ <- Binary.get bh :: IO Word64; return ()

    -- Check the interface file version and ways.
    check_ver  <- get bh
    let our_ver = show hiVersion
    wantedGot "Version" our_ver check_ver text
    errorOnMismatch "mismatched interface file versions" our_ver check_ver

    check_way <- get bh
    let way_descr = getWayDescr dflags
    wantedGot "Way" way_descr check_way ppr
    when (checkHiWay == CheckHiWay) $
        errorOnMismatch "mismatched interface file ways" way_descr check_way
    getWithUserData ncu bh

binaryInterfaceMagic :: DynFlags -> Word32
binaryInterfaceMagic dflags
 | target32Bit (targetPlatform dflags) = 0x1face
 | otherwise                           = 0x1face64

getWayDescr :: DynFlags -> String
getWayDescr dflags
  | platformUnregisterised (targetPlatform dflags) = 'u':tag
  | otherwise                                      =     tag
  where tag = buildTag dflags
        -- if this is an unregisterised build, make sure our interfaces
        -- can't be used by a registerised build.

#elif MIN_VERSION_ghc(8,8,0)
import Prelude hiding ((<>))
import BinIface

import IfaceEnv
import HscTypes
import DynFlags
import Panic
import Binary
import SrcLoc
import ErrUtils
import Outputable
import Platform
import Constants

import Data.Word
import Control.Monad

readBinIface_ :: DynFlags -> CheckHiWay -> TraceBinIFaceReading -> FilePath
              -> NameCacheUpdater
              -> IO ModIface
readBinIface_ dflags checkHiWay traceBinIFaceReading hi_path ncu = do
    let printer :: SDoc -> IO ()
        printer = case traceBinIFaceReading of
                      TraceBinIFaceReading -> \sd ->
                          putLogMsg dflags
                                    NoReason
                                    SevOutput
                                    noSrcSpan
                                    (defaultDumpStyle dflags)
                                    sd
                      QuietBinIFaceReading -> \_ -> return ()
        wantedGot :: Outputable a => String -> a -> a -> IO ()
        wantedGot what wanted got =
            printer (text what <> text ": " <>
                     vcat [text "Wanted " <> ppr wanted <> text ",",
                           text "got    " <> ppr got])

        errorOnMismatch :: (Eq a, Show a) => String -> a -> a -> IO ()
        errorOnMismatch what wanted got =
            -- This will be caught by readIface which will emit an error
            -- msg containing the iface module name.
            when (wanted /= got) $ throwGhcExceptionIO $ ProgramError
                         (what ++ " (wanted " ++ show wanted
                               ++ ", got "    ++ show got ++ ")")
    bh <- Binary.readBinMem hi_path

    -- Read the magic number to check that this really is a GHC .hi file
    -- (This magic number does not change when we change
    --  GHC interface file format)
    magic <- get bh
    wantedGot "Magic" (binaryInterfaceMagic dflags) magic
    errorOnMismatch "magic number mismatch: old/corrupt interface file?"
        (binaryInterfaceMagic dflags) magic

    -- Note [dummy iface field]
    -- read a dummy 32/64 bit value.  This field used to hold the
    -- dictionary pointer in old interface file formats, but now
    -- the dictionary pointer is after the version (where it
    -- should be).  Also, the serialisation of value of type "Bin
    -- a" used to depend on the word size of the machine, now they
    -- are always 32 bits.
    if wORD_SIZE dflags == 4
        then do _ <- Binary.get bh :: IO Word32; return ()
        else do _ <- Binary.get bh :: IO Word64; return ()

    -- Check the interface file version and ways.
    check_ver  <- get bh
    let our_ver = show hiVersion
    wantedGot "Version" our_ver check_ver
    errorOnMismatch "mismatched interface file versions" our_ver check_ver

    check_way <- get bh
    let way_descr = getWayDescr dflags
    wantedGot "Way" way_descr check_way
    when (checkHiWay == CheckHiWay) $
        errorOnMismatch "mismatched interface file ways" way_descr check_way
    getWithUserData ncu bh

binaryInterfaceMagic :: DynFlags -> Word32
binaryInterfaceMagic dflags
 | target32Bit (targetPlatform dflags) = 0x1face
 | otherwise                           = 0x1face64

getWayDescr :: DynFlags -> String
getWayDescr dflags
  | platformUnregisterised (targetPlatform dflags) = 'u':tag
  | otherwise                                      =     tag
  where tag = buildTag dflags
        -- if this is an unregisterised build, make sure our interfaces
        -- can't be used by a registerised build.


#elif MIN_VERSION_ghc(8,6,0)
import Prelude hiding ((<>))
import BinIface

import IfaceEnv
import HscTypes
import DynFlags
import Panic
import Binary
import SrcLoc
import ErrUtils
import Outputable
import Platform
import Constants

import Data.Word
import Control.Monad

readBinIface_ :: DynFlags -> CheckHiWay -> TraceBinIFaceReading -> FilePath
              -> NameCacheUpdater
              -> IO ModIface
readBinIface_ dflags checkHiWay traceBinIFaceReading hi_path ncu = do
    let printer :: SDoc -> IO ()
        printer = case traceBinIFaceReading of
                      TraceBinIFaceReading -> \sd ->
                          putLogMsg dflags
                                    NoReason
                                    SevOutput
                                    noSrcSpan
                                    (defaultDumpStyle dflags)
                                    sd
                      QuietBinIFaceReading -> \_ -> return ()
        wantedGot :: Outputable a => String -> a -> a -> IO ()
        wantedGot what wanted got =
            printer (text what <> text ": " <>
                     vcat [text "Wanted " <> ppr wanted <> text ",",
                           text "got    " <> ppr got])

        errorOnMismatch :: (Eq a, Show a) => String -> a -> a -> IO ()
        errorOnMismatch what wanted got =
            -- This will be caught by readIface which will emit an error
            -- msg containing the iface module name.
            when (wanted /= got) $ throwGhcExceptionIO $ ProgramError
                         (what ++ " (wanted " ++ show wanted
                               ++ ", got "    ++ show got ++ ")")
    bh <- Binary.readBinMem hi_path

    -- Read the magic number to check that this really is a GHC .hi file
    -- (This magic number does not change when we change
    --  GHC interface file format)
    magic <- get bh
    wantedGot "Magic" (binaryInterfaceMagic dflags) magic
    errorOnMismatch "magic number mismatch: old/corrupt interface file?"
        (binaryInterfaceMagic dflags) magic

    -- Note [dummy iface field]
    -- read a dummy 32/64 bit value.  This field used to hold the
    -- dictionary pointer in old interface file formats, but now
    -- the dictionary pointer is after the version (where it
    -- should be).  Also, the serialisation of value of type "Bin
    -- a" used to depend on the word size of the machine, now they
    -- are always 32 bits.
    if wORD_SIZE dflags == 4
        then do _ <- Binary.get bh :: IO Word32; return ()
        else do _ <- Binary.get bh :: IO Word64; return ()

    -- Check the interface file version and ways.
    check_ver  <- get bh
    let our_ver = show hiVersion
    wantedGot "Version" our_ver check_ver
    errorOnMismatch "mismatched interface file versions" our_ver check_ver

    check_way <- get bh
    let way_descr = getWayDescr dflags
    wantedGot "Way" way_descr check_way
    when (checkHiWay == CheckHiWay) $
        errorOnMismatch "mismatched interface file ways" way_descr check_way
    getWithUserData ncu bh

binaryInterfaceMagic :: DynFlags -> Word32
binaryInterfaceMagic dflags
 | target32Bit (targetPlatform dflags) = 0x1face
 | otherwise                           = 0x1face64

getWayDescr :: DynFlags -> String
getWayDescr dflags
  | platformUnregisterised (targetPlatform dflags) = 'u':tag
  | otherwise                                      =     tag
  where tag = buildTag dflags
        -- if this is an unregisterised build, make sure our interfaces
        -- can't be used by a registerised build.

#elif MIN_VERSION_ghc(8,4,0)
import Prelude hiding ((<>), mod)
import BinIface

import IfaceEnv
import HscTypes
import Module
import Name
import DynFlags
import UniqSupply
import Panic
import Binary
import SrcLoc
import ErrUtils
import Outputable
import NameCache
import Platform
import Constants

import Data.Array.ST
import Data.Array.Unsafe
import Data.Word
import Data.Foldable
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.State.Strict as State


readBinIface_ :: DynFlags -> CheckHiWay -> TraceBinIFaceReading -> FilePath
              -> NameCacheUpdater
              -> IO ModIface
readBinIface_ dflags checkHiWay traceBinIFaceReading hi_path ncu = do
    let printer :: SDoc -> IO ()
        printer = case traceBinIFaceReading of
                      TraceBinIFaceReading -> \sd ->
                          putLogMsg dflags
                                    NoReason
                                    SevOutput
                                    noSrcSpan
                                    (defaultDumpStyle dflags)
                                    sd
                      QuietBinIFaceReading -> \_ -> return ()
        wantedGot :: Outputable a => String -> a -> a -> IO ()
        wantedGot what wanted got =
            printer (text what <> text ": " <>
                     vcat [text "Wanted " <> ppr wanted <> text ",",
                           text "got    " <> ppr got])

        errorOnMismatch :: (Eq a, Show a) => String -> a -> a -> IO ()
        errorOnMismatch what wanted got =
            -- This will be caught by readIface which will emit an error
            -- msg containing the iface module name.
            when (wanted /= got) $ throwGhcExceptionIO $ ProgramError
                         (what ++ " (wanted " ++ show wanted
                               ++ ", got "    ++ show got ++ ")")
    bh <- Binary.readBinMem hi_path

    -- Read the magic number to check that this really is a GHC .hi file
    -- (This magic number does not change when we change
    --  GHC interface file format)
    magic <- get bh
    wantedGot "Magic" (binaryInterfaceMagic dflags) magic
    errorOnMismatch "magic number mismatch: old/corrupt interface file?"
        (binaryInterfaceMagic dflags) magic

    -- Note [dummy iface field]
    -- read a dummy 32/64 bit value.  This field used to hold the
    -- dictionary pointer in old interface file formats, but now
    -- the dictionary pointer is after the version (where it
    -- should be).  Also, the serialisation of value of type "Bin
    -- a" used to depend on the word size of the machine, now they
    -- are always 32 bits.
    if wORD_SIZE dflags == 4
        then do _ <- Binary.get bh :: IO Word32; return ()
        else do _ <- Binary.get bh :: IO Word64; return ()

    -- Check the interface file version and ways.
    check_ver  <- get bh
    let our_ver = show hiVersion
    wantedGot "Version" our_ver check_ver
    errorOnMismatch "mismatched interface file versions" our_ver check_ver

    check_way <- get bh
    let way_descr = getWayDescr dflags
    wantedGot "Way" way_descr check_way
    when (checkHiWay == CheckHiWay) $
        errorOnMismatch "mismatched interface file ways" way_descr check_way

    -- Read the dictionary
    -- The next word in the file is a pointer to where the dictionary is
    -- (probably at the end of the file)
    dict_p <- Binary.get bh
    data_p <- tellBin bh          -- Remember where we are now
    seekBin bh dict_p
    dict   <- getDictionary bh
    seekBin bh data_p             -- Back to where we were before

    -- Initialise the user-data field of bh
    bh2 <- do
        bh1 <- return $ setUserData bh $ newReadState (error "getSymtabName")
                                                     (getDictFastString dict)
        symtab_p <- Binary.get bh1      -- Get the symtab ptr
        data_p1 <- tellBin bh1          -- Remember where we are now
        seekBin bh1 symtab_p
        symtab <- getSymbolTable bh1 ncu
        seekBin bh1 data_p1             -- Back to where we were before

        -- It is only now that we know how to get a Name
        return $ setUserData bh1 $ newReadState (getSymtabName ncu dict symtab)
                                                (getDictFastString dict)

    -- Read the interface file
    get bh2

getSymbolTable :: BinHandle -> NameCacheUpdater -> IO SymbolTable
getSymbolTable bh ncu = do
    sz <- get bh
    od_names <- sequence (replicate sz (get bh))
    updateNameCache ncu $ \namecache ->
        runST $ flip State.evalStateT namecache $ do
            mut_arr <- lift $ newSTArray_ (0, sz-1)
            for_ (zip [0..] od_names) $ \(i, odn) -> do
                (nc, !n) <- State.gets $ \nc -> fromOnDiskName nc odn
                lift $ writeArray mut_arr i n
                State.put nc
            arr <- lift $ unsafeFreeze mut_arr
            namecache' <- State.get
            return (namecache', arr)
  where
    -- This binding is required because the type of newArray_ cannot be inferred
    newSTArray_ :: forall s. (Int, Int) -> ST s (STArray s Int Name)
    newSTArray_ = newArray_

type OnDiskName = (UnitId, ModuleName, OccName)

fromOnDiskName :: NameCache -> OnDiskName -> (NameCache, Name)
fromOnDiskName nc (pid, mod_name, occ) =
    let mod   = mkModule pid mod_name
        cache = nsNames nc
    in case lookupOrigNameCache cache  mod occ of
           Just name -> (nc, name)
           Nothing   ->
               let (uniq, us) = takeUniqFromSupply (nsUniqs nc)
                   name       = mkExternalName uniq mod occ noSrcSpan
                   new_cache  = extendNameCache cache mod occ name
               in ( nc{ nsUniqs = us, nsNames = new_cache }, name )

binaryInterfaceMagic :: DynFlags -> Word32
binaryInterfaceMagic dflags
 | target32Bit (targetPlatform dflags) = 0x1face
 | otherwise                           = 0x1face64

getWayDescr :: DynFlags -> String
getWayDescr dflags
  | platformUnregisterised (targetPlatform dflags) = 'u':tag
  | otherwise                                      =     tag
  where tag = buildTag dflags
        -- if this is an unregisterised build, make sure our interfaces
        -- can't be used by a registerised build.

#else
#error "Unsupported ghc version"
#endif
