-- |
-- SPDX-License-Identifier: GPL-2.0-or-later
-- Copyright:               Oleg Grenrus <oleg.grenrus@iki.fi>
module Peura.Process (
    runProcess,
    runProcessCheck,
    runProcessOutput,
    MakeProcessTracer (..),
    TraceProcess (..),
    ) where

import Control.Concurrent.Async (wait, withAsync)
import Foreign.C.Error          (Errno (..), ePIPE)
import System.Clock
       (Clock (Monotonic), TimeSpec, diffTimeSpec, getTime)
import System.IO                (Handle, hClose)
import System.Process           (cwd, proc)

import qualified Control.Exception             as E
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy.Internal as LBS
                 (ByteString (..), defaultChunkSize)
import qualified GHC.IO.Exception              as GHC
import qualified System.Process                as Proc

import Peura.Exports
import Peura.Monad
import Peura.Paths
import Peura.Tracer

runProcess
    :: (HasCallStack, MakeProcessTracer t)
    => Tracer (Peu r) t
    -> Path Absolute -- ^ Working directory
    -> String        -- ^ Command
    -> [String]      -- ^ Arguments
    -> ByteString    -- ^ Stdin
    -> Peu r (ExitCode, LazyByteString, LazyByteString)
runProcess tracer cwd' cmd args stdin = do
    tracer' <- makeProcessTracer tracer
    traceWith tracer' (TraceProcessStart cwd' cmd args)
    runProcessImpl tracer' p stdin
  where
    p0 = proc cmd args
    p  = p0
        { cwd = Just (toFilePath cwd')
        }

-- | 'runProcess', but check the exitcode and return stdin.
runProcessCheck
    :: (HasCallStack, MakeProcessTracer t, MakePeuTracer t)
    => Tracer (Peu r) t
    -> Path Absolute -- ^ Working directory
    -> String        -- ^ Command
    -> [String]      -- ^ Arguments
    -> Peu r LazyByteString
runProcessCheck tracer cwd' cmd args = do
    tracer' <- makeProcessTracer tracer
    traceWith tracer' (TraceProcessStart cwd' cmd args)
    (ec, out, err) <- runProcessImpl tracer' p mempty
    case ec of
        ExitSuccess   -> return out
        ExitFailure c -> do
            traceWith tracer' (TraceProcessFailedCheck c out err)
            die tracer "process check failed"
  where
    p0 = proc cmd args
    p  = p0
        { cwd = Just (toFilePath cwd')
        }

-- | 'runProcess' but stream the output directly to the output.
runProcessOutput
    :: (HasCallStack, MakeProcessTracer t)
    => Tracer (Peu r) t
    -> Path Absolute -- ^ Working directory
    -> String        -- ^ Command
    -> [String]      -- ^ Arguments
    -> Peu r ExitCode
runProcessOutput tracer cwd' cmd args = do
    tracer' <- makeProcessTracer tracer
    traceWith tracer' (TraceProcessStart cwd' cmd args)
    runProcessOutputImpl tracer' p
  where
    p0 = proc cmd args
    p  = p0
        { cwd = Just (toFilePath cwd')
        }

-------------------------------------------------------------------------------
-- Trace
-------------------------------------------------------------------------------

data TraceProcess
    = TraceProcessStart (Path Absolute) String [String]
    | TraceProcessRunTime TimeSpec
    | TraceProcessFailedCheck Int LazyByteString LazyByteString
  deriving (Show)

class MakeProcessTracer t where
    makeProcessTracer :: Tracer (Peu r) t -> Peu r (Tracer (Peu r) TraceProcess)

instance MakeProcessTracer TraceProcess where
    makeProcessTracer = return

-------------------------------------------------------------------------------
-- Internal implementation
-------------------------------------------------------------------------------

runProcessImpl
    :: Tracer (Peu r) TraceProcess
    -> Proc.CreateProcess
    -> ByteString
    -> Peu r (ExitCode, LazyByteString, LazyByteString)
runProcessImpl tracer cp input = withRunInIO $ \runInIO -> do
    startTime <- getTime Monotonic
    Proc.withCreateProcess cp' $ \mi mo me ph -> case (mi, mo, me) of
        (Just inh, Just outh, Just errh) ->
            -- spawn workers to read stdout and stderr
            withAsync (getLBSContents outh) $ \outA ->
            withAsync (getLBSContents errh) $ \errA -> do
                -- write the input
                unless (BS.null input) $ BS.hPutStr inh input
                ignoreSigPipe $ hClose inh

                -- wait for the output
                out <- wait outA
                err <- wait errA

                -- wait for the process
                ec <- Proc.waitForProcess ph

                endTime <- getTime Monotonic
                runInIO $ displayRunTime tracer startTime endTime

                return (ec, out, err)

        (Nothing,_,_) -> fail "runProcess: Failed to get a stdin handle."
        (_,Nothing,_) -> fail "runProcess: Failed to get a stdout handle."
        (_,_,Nothing) -> fail "runProcess: Failed to get a stderr handle."

  where
    cp' :: Proc.CreateProcess
    cp' = cp
        { Proc.std_in  = Proc.CreatePipe
        , Proc.std_out = Proc.CreatePipe
        , Proc.std_err = Proc.CreatePipe
        }

runProcessOutputImpl
    :: Tracer (Peu r) TraceProcess
    -> Proc.CreateProcess
    -> Peu r ExitCode
runProcessOutputImpl tracer cp = withRunInIO $ \runInIO -> do
    startTime <- getTime Monotonic
    Proc.withCreateProcess cp' $ \mi _ _ ph -> do
        -- close the input immediately.
        for_ mi $ \inh -> ignoreSigPipe $ hClose inh

        ec <- Proc.waitForProcess ph

        endTime <- getTime Monotonic
        runInIO $ displayRunTime tracer startTime endTime

        return ec
  where
    cp' :: Proc.CreateProcess
    cp' = cp
        -- We need to open some pipe, otherwise configure scripts will barf.
        -- Silly configure assumes it has attached stdin.
        { Proc.std_in        = Proc.CreatePipe
        , Proc.std_out       = Proc.Inherit
        , Proc.std_err       = Proc.Inherit
        , Proc.delegate_ctlc = True
        }
-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

displayRunTime :: Tracer m TraceProcess -> TimeSpec -> TimeSpec -> m ()
displayRunTime tracer start end = do
    traceWith tracer (TraceProcessRunTime (diffTimeSpec end start))

ignoreSigPipe :: IO () -> IO ()
ignoreSigPipe = E.handle $ \e -> case e of
    GHC.IOError { GHC.ioe_type  = GHC.ResourceVanished, GHC.ioe_errno = Just ioe }
        | Errno ioe == ePIPE -> return ()
    _ -> E.throwIO e

getLBSContents :: Handle -> IO LazyByteString
getLBSContents = hGetContentsN LBS.defaultChunkSize

-- No unsafeInterleaveIO
hGetContentsN :: Int -> Handle -> IO LazyByteString
hGetContentsN k h = loop `E.finally` hClose h where
    loop = do
        c <- BS.hGetSome h k -- only blocks if there is no data available
        if BS.null c
        then return LBS.Empty
        else LBS.Chunk c <$> loop
