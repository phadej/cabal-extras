{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception  (bracket)
import System.Environment (getArgs)
import System.IO          (Handle, IOMode (ReadWriteMode), hClose, openFile)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import Lukko

#ifdef HAS_OFD_LOCKING
import qualified Lukko.OFD as OFD
#endif

#ifdef HAS_FLOCK
import qualified Lukko.FLock as FLock
#endif

main :: IO ()
main = withArgs $ \withLock -> do
    putStrLn "starting..."
    withLock $ do
        contents <- BS.readFile "test-actual"
        threadDelay 10000 -- 10 ms
        BS.writeFile "test-actual" $ BS.append contents $ BS8.pack "another line\n"

withArgs
    :: ((forall r. IO r -> IO r) -> IO ())
    -> IO ()
withArgs k = do
    args <- getArgs
    case args of
        ["default"] -> k (genWithLock hLock hUnlock "test-lock")
#ifdef HAS_OFD_LOCKING
        ["ofd"]     -> k (genWithLock OFD.hLock OFD.hUnlock "test-lock")
#endif
#ifdef HAS_FLOCK
        ["flock"]   -> k (genWithLock FLock.hLock FLock.hUnlock "test-lock")
#endif
        ["noop"]    -> k (genWithLock noOpLock noOpUnlock "test-lock")
        _           -> putStrLn "Unknown paramters. Doing nothing."

-------------------------------------------------------------------------------
-- copy pasted
-------------------------------------------------------------------------------

noOpLock :: Handle -> LockMode -> IO ()
noOpLock _ _ = return ()

noOpUnlock :: Handle -> IO ()
noOpUnlock _ = return ()

genWithLock
    :: (Handle -> LockMode -> IO ())
    -> (Handle -> IO ())
    -> FilePath
    -> IO a
    -> IO a
genWithLock implLock implUnlock fp action =
    bracket takeLock releaseLock (const action)
  where
    takeLock = do
        h <- openFile fp ReadWriteMode
        implLock h ExclusiveLock
        return h

    releaseLock :: Handle -> IO ()
    releaseLock h = do
        implUnlock h
        hClose h
