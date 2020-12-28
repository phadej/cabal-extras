module CabalDocspec.GHCi (
    withInteractiveGhc,
    GHCi,
    setupGhci,
    sendExpressions,
    Result (..),
) where

import Peura

import Control.Concurrent     (threadDelay)
import Control.Concurrent.STM (readTVar, registerDelay, retry)

import qualified Data.ByteString            as BS
import qualified System.Process             as Proc
import qualified System.Process.Interactive as Proci
import qualified System.Random.SplitMix     as SM

import CabalDocspec.Trace
import CabalDocspec.Warning

-- | Handle to GHCi process.
newtype GHCi = GHCi Proci.IPH

-- | Run interactive GHCi
withInteractiveGhc
    :: TracerPeu r Tr
    -> GhcInfo
    -> Path Absolute
    -> [String]
    -> (GHCi -> Peu r a)
    -> Peu r a
withInteractiveGhc tracer ghcInfo cwd args kont = do
    traceApp tracer $ TraceGHCi (ghcPath ghcInfo) args'

    Proci.withInteractiveProcess pc1 $ \iph -> do
            let ghci = GHCi iph
            setupGhci tracer ghcInfo ghci
            kont ghci
  where
    pc0 = Proc.proc (ghcPath ghcInfo) args'
    pc1 = pc0
        { Proc.cwd = Just (toFilePath cwd)
        }

    args' = ["--interactive", "-ignore-dot-ghci"] ++ args

setupGhci :: TracerPeu r Tr -> GhcInfo -> GHCi -> Peu r ()
setupGhci tracer _ghcInfo ghci@(GHCi iph) = do
    -- turn off prompt
    -- it is fine to send these, even if they may not work.
    Proci.sendTo iph ":set prompt \"\"\n"
    Proci.sendTo iph ":set prompt2 \"\"\n"     -- GHC-7.8+
    Proci.sendTo iph ":set prompt-cont \"\"\n" -- GHC-8.2+

    -- wait a little. I'm not sure if we need this delay
    liftIO $ threadDelay 10000

    res <- waitGhci tracer ghci Nothing 3000000 -- TODO: make this delay configurable
    case res of
        Timeout    -> do
            (err,out) <- liftIO $ atomically $ liftA2 (,) (Proci.readErr iph) (Proci.readOut iph)
            putDebug tracer $ "stderr:\n" ++ foldMap fromUTF8BS err
            putDebug tracer $ "stdout:\n" ++ foldMap fromUTF8BS out
            die tracer "Timeout while starting GHCi"
        Exited ec  -> do
            (err,out) <- liftIO $ atomically $ liftA2 (,) (Proci.readErr iph) (Proci.readOut iph)
            putDebug tracer $ "stderr:\n" ++ foldMap fromUTF8BS err
            putDebug tracer $ "stdout:\n" ++ foldMap fromUTF8BS out
            die tracer $ "Failure while starting GHCi: " ++ show ec
        Result _ _ -> return ()

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

data Result
    = Result BS.ByteString BS.ByteString
    | Exited ExitCode
    | Timeout

waitGhci :: TracerPeu r Tr -> GHCi -> Maybe String -> Int -> Peu r Result
waitGhci _tracer (GHCi iph) mitVar microsecs = do
    -- send separator
    separator <- show <$> genString
    Proci.sendTo iph $ fromString $ separator ++ " :: Data.String.String\n"

    for_ mitVar $ \itVar -> do
        Proci.sendTo iph $ fromString $ "let it = " ++ itVar ++ "\n"

    -- Make timeout
    timeoutVar <- liftIO $ registerDelay microsecs

    -- read input until there is separator
    res <- liftIO $ atomically $ do
        timeout <- readTVar timeoutVar
        if timeout
        then do
            return Timeout

        else do
            input <- Proci.readOut iph
            let input' = foldMap id input
            let (before, after) = BS.breakSubstring (fromString $ separator ++ "\n") input'
            if BS.null after
            then do
                retry
            else do
                -- if we found output clear stderr
                errput <- Proci.readErr iph
                return (Result before (foldMap id errput))

    case res of
        Timeout -> do
            ec <- Proci.getIntercativeProcessExitCode iph
            return $ maybe Timeout Exited ec

        _ -> return res

-- | Send expressions (individual lines)
-- wait for combined response.
sendExpressions :: TracerPeu r Tr -> GHCi -> Bool -> Int -> [String] -> Peu r Result
sendExpressions tracer ghci@(GHCi iph) preserveIt timeout exprs = do
    -- send expressions
    for_ exprs $ \expr -> do
        Proci.sendTo iph (toUTF8BS expr <> "\n")

    -- save @it@
    mitVar <- if not preserveIt then return Nothing else Just <$> do
        itVar <- ("it_" ++) <$> genString
        Proci.sendTo iph $ fromString $ "let " ++ itVar ++ " = it\n"
        return itVar

    -- wait for responses
    res <- waitGhci tracer ghci mitVar (max 100000 timeout) -- 0.1 sec minimum

    case res of
        Timeout -> do
            putWarning tracer WTimeout "timeout..."
            -- send ctrl-c and wait again
            Proci.sendCtrlC iph
            Proci.sendTo iph "\n"
            res' <- waitGhci tracer ghci Nothing 10000000 -- 10 sec
            case res' of
                Timeout -> die tracer "Timeout while recovering from timeout"
                Exited _ -> return res'
                Result _out _err -> do
                    -- putDebug tracer (fromUTF8BS err)
                    -- putDebug tracer (fromUTF8BS out)
                    return Timeout

        -- done
        _ -> pure res

-------------------------------------------------------------------------------
-- Separator, some random chars
-------------------------------------------------------------------------------

genString :: Peu r String
genString = liftIO $ do
    g <- SM.newSMGen
    return $ go 48 g
  where
    go :: Int -> SM.SMGen -> String
    go n g
        | n < 0 = []
        | otherwise = let (w, g') = SM.bitmaskWithRejection64' 0x1f g in toChar w : go (n - 1) g'

toChar :: Word64 -> Char
toChar 0x00 = 'a'
toChar 0x01 = 'b'
toChar 0x02 = 'c'
toChar 0x03 = 'd'
toChar 0x04 = 'e'
toChar 0x05 = 'f'
toChar 0x06 = 'g'
toChar 0x07 = 'h'
toChar 0x08 = 'i'
toChar 0x09 = 'j'
toChar 0x0a = 'k'
toChar 0x0b = 'l'
toChar 0x0c = 'n'
toChar 0x0d = 'n'
toChar 0x0e = 'o'
toChar 0x0f = 'p'
toChar 0x10 = 'q'
toChar 0x11 = 'r'
toChar 0x12 = 's'
toChar 0x13 = 't'
toChar 0x14 = 'u'
toChar 0x15 = 'v'
toChar 0x16 = 'w'
toChar 0x17 = 'x'
toChar 0x18 = 'y'
toChar 0x19 = 'z'
toChar 0x1a = '2'
toChar 0x1b = '3'
toChar 0x1c = '4'
toChar 0x1d = '5'
toChar 0x1e = '6'
toChar _    = '7'
