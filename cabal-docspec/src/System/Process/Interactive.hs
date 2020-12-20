{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Process.Interactive (
    -- * Creation
    withInteractiveProcess,
    IPH,
    -- * Interaction
    sendTo,
    sendCtrlC,
    readOut,
    unreadOut,
    readErr,
    unreadErr,
    getIntercativeProcessExitCode,
) where

import Prelude

import Control.Concurrent       (threadDelay)
import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.STM   (STM, TVar, atomically, modifyTVar', newTVarIO, readTVar, writeTVar)
import Control.Monad            (void)
import Control.Monad.IO.Class   (MonadIO (..))
import Control.Monad.IO.Unlift  (MonadUnliftIO (..))
import Data.ByteString          (ByteString)
import Data.Sequence            (Seq)
import Foreign.C.Error          (Errno (..), ePIPE)
import System.Exit              (ExitCode)
import System.IO                (BufferMode (..), Handle, hClose, hSetBuffering)
import System.Process           (CreateProcess, ProcessHandle)

import qualified Control.Exception as E
import qualified Data.ByteString   as BS
import qualified Data.Sequence     as Seq
import qualified GHC.IO.Exception  as GHC
import qualified System.Process    as Proc

import Peura.Trace (putStrErr)

-- | Interactive process handle
data IPH = IPH
    { iphHandle :: ProcessHandle
    , iphStdin  :: Handle
    , iphOutQ   :: TVar (Seq ByteString)
    , iphErrQ   :: TVar (Seq ByteString)
    }

-- | Create interactive process.
withInteractiveProcess
    :: forall r m. MonadUnliftIO m
    => CreateProcess
    -> (IPH -> m r)
    -> m r
withInteractiveProcess pr kont0 =
    withRunInIO $ \runInIO ->
    Proc.withCreateProcess pr' $ \h1 h2 h3 ph ->
    impl (runInIO . kont0) h1 h2 h3 ph
  where
    pr' :: CreateProcess
    pr' = pr
        { Proc.std_in       = Proc.CreatePipe
        , Proc.std_out      = Proc.CreatePipe
        , Proc.std_err      = Proc.CreatePipe
        , Proc.create_group = True
        }

    impl :: (IPH -> IO r) -> Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO r
    impl kont (Just inpH) (Just outH) (Just errH) ph = do
        -- set buffering to receive stuff in line chunks.
        hSetBuffering outH NoBuffering
        hSetBuffering errH NoBuffering
        hSetBuffering inpH LineBuffering

        -- create queues
        outQ <- newTVarIO Seq.empty
        errQ <- newTVarIO Seq.empty

        -- start reading loops
        withAsync (readHandle outH outQ) $ \outA ->
            withAsync (readHandle errH errQ) $ \errA -> do
                let iph :: IPH
                    iph = IPH
                        { iphHandle = ph
                        , iphStdin  = inpH
                        , iphOutQ   = outQ
                        , iphErrQ   = errQ
                        }

                res <- kont iph `E.onException` do
                    -- on exception:
                    --
                    -- - terminate process
                    -- - and wait for it to terminate!
                    --
                    -- This seems to cleanup GHCi after Ctrl-C cancellations.
                    putStrErr "proci: canceling\n"
                    Proc.interruptProcessGroupOf ph
                    threadDelay 100000
                    putStrErr "proci: terminating\n"
                    pid <- Proc.getPid ph
                    putStrErr $ "proci: pid "++ show pid ++ "\n"
                    Proc.terminateProcess ph
                    threadDelay 100000
                    putStrErr "proci: waiting for termination\n"
                    void $ Proc.waitForProcess ph

                -- close input handle
                ignoreSigPipe $ hClose inpH

                -- wait for the output
                wait outA
                wait errA

                -- wait for the process
                _ec <- Proc.waitForProcess ph

                -- close handles -- withCreateProcess closes them?
                hClose outH
                hClose errH

                -- return result
                return res

    impl _ _ _ _ _ = fail "withInteractiveProcess: withCreateProcess returned nonsense."

-- | Send input to the interactive process.
--
sendTo :: MonadIO m => IPH -> ByteString -> m ()
sendTo iph bs = liftIO $ do
    BS.hPutStr (iphStdin iph) bs

-- | Send Control-C signal.
--
-- Calls 'Process.interruptProcessGroupOf'.
--
sendCtrlC :: MonadIO m => IPH -> m ()
sendCtrlC iph = liftIO $ do
    Proc.interruptProcessGroupOf (iphHandle iph)

readOut :: IPH -> STM (Seq ByteString)
readOut iph = do
    s <- readTVar (iphOutQ iph)
    writeTVar (iphOutQ iph) Seq.empty
    return s

unreadOut :: IPH -> Seq ByteString -> STM ()
unreadOut iph bss = do
    s <- readTVar (iphOutQ iph)
    writeTVar (iphOutQ iph) (bss <> s)

readErr :: IPH -> STM (Seq ByteString)
readErr iph = do
    s <- readTVar (iphErrQ iph)
    writeTVar (iphErrQ iph) Seq.empty
    return s

unreadErr :: IPH -> Seq ByteString -> STM ()
unreadErr iph bss = do
    s <- readTVar (iphErrQ iph)
    writeTVar (iphErrQ iph) (bss <> s)

getIntercativeProcessExitCode :: MonadIO m => IPH -> m (Maybe ExitCode)
getIntercativeProcessExitCode = liftIO . Proc.getProcessExitCode . iphHandle

-------------------------------------------------------------------------------
-- Implementation bits
-------------------------------------------------------------------------------

ignoreSigPipe :: IO () -> IO ()
ignoreSigPipe = E.handle $ \e -> case e of
    GHC.IOError { GHC.ioe_type  = GHC.ResourceVanished, GHC.ioe_errno = Just ioe }
        | Errno ioe == ePIPE -> return ()
    _ -> E.throwIO e

readHandle :: Handle -> TVar (Seq ByteString) -> IO ()
readHandle h lqVar = loop where
    loop = do
        c <- BS.hGetSome h 4086
        if BS.null c
        then return ()
        else do
            atomically $ modifyTVar' lqVar (Seq.|> c)
            loop
