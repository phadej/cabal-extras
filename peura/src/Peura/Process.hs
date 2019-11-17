-- |
-- SPDX-License-Identifier: GPL-2.0-or-later
-- Copyright:               Oleg Grenrus <oleg.grenrus@iki.fi>
module Peura.Process where

import System.Process            (cwd, proc)
import System.Process.ByteString (readCreateProcessWithExitCode)

import Peura.Exports
import Peura.Monad
import Peura.Paths

import qualified Data.ByteString as BS

runProcess
    :: Path Absolute -- ^ Working directory
    -> String        -- ^ Command
    -> [String]      -- ^ Arguments
    -> ByteString    -- ^ Stdin
    -> Peu r (ExitCode, ByteString, ByteString)
runProcess cwd' cmd args stdin = do
    putDebug $ "runProcess " ++ unwords (("cwd=" ++ toFilePath cwd') : cmd : args)
    liftIO $ readCreateProcessWithExitCode p stdin
  where
    p0 = proc cmd args
    p  = p0
        { cwd = Just (toFilePath cwd')
        }

runProcessCheck
    :: Path Absolute -- ^ Working directory
    -> String        -- ^ Command
    -> [String]      -- ^ Arguments
    -> Peu r ()
runProcessCheck cwd' cmd args = do
    (ec, out, err) <- runProcess cwd' cmd args mempty
    case ec of
        ExitFailure c -> do
            putError $ "ExitFailure " ++ show c
            putError "stdout"
            liftIO $ BS.putStr out
            putError "stderr"
            liftIO $ BS.putStr err 
            exitFailure

        ExitSuccess -> return ()
