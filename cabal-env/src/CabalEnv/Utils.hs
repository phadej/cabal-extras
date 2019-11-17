module CabalEnv.Utils (
die,
readProcessWithExitCode'
) where

import CabalEnv.Prelude

import System.Process.ByteString (readProcessWithExitCode)

import qualified Data.ByteString as BS

die :: String -> IO a
die msg = do
    hPutStrLn stderr $ "Panic: " ++ msg
    exitFailure

{-
runProcess
    :: FilePath      -- ^ Working directory
    -> String        -- ^ Command
    -> [String]      -- ^ Arguments
    -> ByteString    -- ^ Stdin
    -> IO (ExitCode, ByteString, ByteString)
runProcess cwd cmd args stdin =
    readCreateProcessWithExitCode p stdin
  where
    p0 = proc cmd args
    p  = p0
        { cwd = Just cwd
        }
-}

readProcessWithExitCode' :: FilePath -> [String] -> IO ByteString
readProcessWithExitCode' cmd args = do
    (ec, out, err) <- readProcessWithExitCode cmd args BS.empty
    case ec of
        ExitSuccess   -> return out
        ExitFailure _ -> do
            hPutStrLn stderr $ "Error running " ++ cmd ++ " " ++ unwords args
            BS.hPutStr stderr err
            exitFailure
