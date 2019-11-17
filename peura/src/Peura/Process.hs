-- |
-- SPDX-License-Identifier: GPL-2.0-or-later
-- Copyright:               Oleg Grenrus <oleg.grenrus@iki.fi>
module Peura.Process (
    runProcess,
    runProcessCheck,
    runProcessOutput,
    ) where

import Control.Concurrent.Async  (wait, withAsync)
import Foreign.C.Error           (Errno (..), ePIPE)
import System.Console.Concurrent (lockOutput)
import System.IO                 (Handle, hClose)
import System.Process            (cwd, proc)

import qualified Control.Exception             as E
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy.Internal as LBS
                 (ByteString (..), defaultChunkSize)
import qualified GHC.IO.Exception              as GHC
import qualified System.Process                as Proc

import Peura.Exports
import Peura.Monad
import Peura.Paths

runProcess
    :: Path Absolute -- ^ Working directory
    -> String        -- ^ Command
    -> [String]      -- ^ Arguments
    -> ByteString    -- ^ Stdin
    -> Peu r (ExitCode, LazyByteString, LazyByteString)
runProcess cwd' cmd args stdin = do
    putDebug $ "runProcess " ++ unwords (("cwd=" ++ toFilePath cwd') : cmd : args)
    liftIO $ runProcessImpl p stdin
  where
    p0 = proc cmd args
    p  = p0
        { cwd = Just (toFilePath cwd')
        }

-- | 'runProcess', but check the exitcode and return stdin.
runProcessCheck
    :: Path Absolute -- ^ Working directory
    -> String        -- ^ Command
    -> [String]      -- ^ Arguments
    -> Peu r LazyByteString
runProcessCheck cwd' cmd args = do
    (ec, out, err) <- runProcess cwd' cmd args mempty
    case ec of
        ExitSuccess   -> return out
        ExitFailure c -> do
            putError $ "ExitFailure " ++ show c
            putError "stdout"
            output out
            putError "stderr"
            output err
            exitFailure

-- | 'runProcess' but stream the output directly to the output.
runProcessOutput
    :: Path Absolute -- ^ Working directory
    -> String        -- ^ Command
    -> [String]      -- ^ Arguments
    -> Peu r ExitCode
runProcessOutput cwd' cmd args = do
    putDebug $ "runProcessOutput " ++ unwords (("cwd=" ++ toFilePath cwd') : cmd : args)
    liftIO $ runProcessOutputImpl p
  where
    p0 = proc cmd args
    p  = p0
        { cwd = Just (toFilePath cwd')
        }

-------------------------------------------------------------------------------
-- Internal implementation
-------------------------------------------------------------------------------

runProcessImpl
    :: Proc.CreateProcess
    -> ByteString
    -> IO (ExitCode, LazyByteString, LazyByteString)
runProcessImpl cp input =
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
    :: Proc.CreateProcess
    -> IO ExitCode
runProcessOutputImpl cp =
    Proc.withCreateProcess cp' $ \_ _ _ ph ->
        Proc.waitForProcess ph
  where
    cp' :: Proc.CreateProcess
    cp' = cp
        { Proc.std_in        = Proc.NoStream
        , Proc.std_out       = Proc.Inherit
        , Proc.std_err       = Proc.Inherit
        , Proc.delegate_ctlc = True
        }

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

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
