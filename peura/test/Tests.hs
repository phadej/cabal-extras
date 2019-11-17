{-# LANGUAGE CPP #-}
module Main (main) where

import Control.Concurrent       (threadDelay)
import Control.Concurrent.Async (forConcurrently_)
import Control.Exception        (bracket)
import Data.IORef
import System.FilePath          ((</>))
import System.IO
       (Handle, IOMode (ReadWriteMode), hClose, openFile)
import System.IO.Temp           (withSystemTempDirectory)
import Test.Tasty               (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit         (testCase, (@=?))

import Lukko

#ifdef HAS_OFD_LOCKING
import qualified Lukko.OFD as OFD
#endif

#ifdef HAS_FLOCK
import qualified Lukko.FLock as FLock
#endif

main :: IO ()
main = defaultMain $ testGroup "lukko" $
    [ testGroup "Lukko default" $ testSuite fdLock fdUnlock
    | fileLockingSupported
    ]
#ifdef HAS_OFD_LOCKING
    ++ [ testGroup "Lukko.OFD" $ testSuite OFD.fdLock OFD.fdUnlock ]
#endif
#ifdef HAS_FLOCK
    ++ [ testGroup "Lukko.FLock" $ testSuite FLock.fdLock FLock.fdUnlock ]
#endif

testSuite
    :: (FD -> LockMode -> IO ())
    -> (FD -> IO ())
    -> [TestTree]
testSuite implLock implUnlock =
    [ testCase "concurrent threads" $ do
        let n = 10 :: Int
        ref <- newIORef 0

        withSystemTempDirectory "handle-lock-tests" $ \tmpDir -> do
            -- print tmpDir
            forConcurrently_ [1 :: Int .. n] $ \_ ->
                withLock (tmpDir </> "lock") $ do
                    val <- readIORef ref
                    threadDelay 10000 -- 10ms
                    writeIORef ref (succ val)

        val <- readIORef ref
        val @=? n
    ]
  where
    withLock = genWithLock implLock implUnlock

genWithLock
    :: (FD -> LockMode -> IO ())
    -> (FD -> IO ())
    -> FilePath
    -> IO a
    -> IO a
genWithLock implLock implUnlock fp action =
    bracket takeLock releaseLock (const action)
  where
    takeLock = do
        fd <- fdOpen fp
        implLock fd ExclusiveLock
        return fd

    releaseLock :: FD -> IO ()
    releaseLock fd = do
        implUnlock fd
        fdClose fd
