module CabalDocspec.Man where

import Data.Foldable (for_)
import Prelude       (IO, ($))
import System.Exit   (exitWith)
import System.IO     (hClose, hPutStr)

import qualified System.Process as Proc

import CabalDocspec.Man.Content (manContent)

man :: IO ()
man = do
    let cmd  = "man"
        args = ["-l", "-"]

    (mb_in, _, _, ph) <- Proc.createProcess (Proc.proc cmd args)
        { Proc.std_in  = Proc.CreatePipe
        , Proc.std_out = Proc.Inherit
        , Proc.std_err = Proc.Inherit
        }

    -- put contents
    for_ mb_in $ \hin -> do
        hPutStr hin manContent
        hClose hin

    -- wait for process to exit, propagate exit code
    ec <- Proc.waitForProcess ph
    exitWith ec
