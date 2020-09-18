{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Peura.Monad (
    Peu (..),
    runPeu,
    runPeu',
    changePeu,
    -- * Output
    Output (..),
    output,
    -- ** Functions
    TracerPeu,
    putDebug,
    putInfo,
    putWarning,
    putError,
    die,
    -- ** Warning class
    Warning (..),
    -- * Control.Exception
    evaluate,
    evaluateForce,
    -- * System.Environment
    getArgs,
    lookupEnv,
    -- * System.Exit
    exitFailure,
    ) where

import Control.Monad.Reader.Class (MonadReader (..))
import Data.Typeable              (cast)
import GHC.Stack                  (HasCallStack)
import System.Clock
       (Clock (Monotonic), TimeSpec (TimeSpec), diffTimeSpec, getTime)
import System.Console.Concurrent
       (errorConcurrent, outputConcurrent, withConcurrentOutput)
import System.Console.Regions     (displayConsoleRegions)
import System.Path (toFilePath)
import System.IO                  (stderr)
import Text.Printf                (printf)

import qualified Control.Exception    as X
import qualified Data.ByteString.Lazy as LBS
import qualified System.Console.ANSI  as ANSI
import qualified System.Environment   as X
import qualified System.Exit          as X

import Peura.Exports
import Peura.Tracer
import Peura.Trace

newtype Peu r a = Peu { unPeu :: r -> IO a }
  deriving stock Functor

runPeu :: Warning w => r -> (Tracer (Peu r) (Trace w) -> Peu r a) -> IO a
runPeu r m = do
    supportsAnsi <- ANSI.hSupportsANSI stderr
    now <- getTime Monotonic
    let tracer = defaultTracer supportsAnsi now
    runPeu' tracer r (m tracer)

runPeu' :: forall w r a. Tracer (Peu r) (Trace w) -> r -> Peu r a -> IO a
runPeu' tracer r m = withConcurrentOutput $ displayConsoleRegions $ do
    res <- unPeu m r `catch` handleException
    unPeu (putDebug tracer "runPeu completed successfully") r
    return res
  where
    handleException :: SomeException -> IO a
    handleException (X.SomeException exc) =
        case cast exc of
            Just (ec :: ExitCode) ->
                X.throwIO ec
            Nothing -> do
                unPeu (putError tracer $ "Exception " ++ typeNameOf exc) r
                errorConcurrent $ X.displayException exc ++ "\n"
                unPeu exitFailure r

    typeNameOf :: forall x. Typeable x => x -> String
    typeNameOf _ = show $ typeRep (Proxy :: Proxy x)

changePeu :: (r -> s) -> Peu s a -> Peu r a
changePeu f (Peu m) = Peu $ \r -> m (f r)

instance Applicative (Peu r) where
    pure = \x -> Peu (\_ -> pure x)
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance Monad (Peu r) where
    return = pure

    m >>= k = Peu $ \e -> unPeu m e >>= \x -> unPeu (k x) e
    {-# INLINE (>>=) #-}

instance MonadIO (Peu r) where
    liftIO m = Peu $ \_ -> m
    {-# INLINE liftIO #-}

instance MonadUnliftIO (Peu r) where
    withRunInIO f = Peu $ \r -> f $ \m -> unPeu m r

instance MonadReader r (Peu r) where
    ask      = Peu $ \r -> return r
    reader f = Peu $ \r -> return (f r)

    local f (Peu g) = Peu $ \r -> g (f r)

instance MonadThrow (Peu r) where
    throwM e = Peu $ \_ -> throwM e

instance MonadCatch (Peu r) where
    catch (Peu m) c = Peu $ \r -> m r `catch` \e -> unPeu (c e) r

instance MonadMask (Peu r) where
    mask a = Peu $ \e -> mask $ \u -> unPeu (a $ q u) e
      where
        q :: (IO a -> IO a) -> Peu r a -> Peu r a
        q u (Peu b) = Peu (u . b)

    uninterruptibleMask a =
        Peu $ \e -> uninterruptibleMask $ \u -> unPeu (a $ q u) e
      where
        q :: (IO a -> IO a) -> Peu r a -> Peu r a
        q u (Peu b) = Peu (u . b)

    generalBracket acquire release use = Peu $ \r -> generalBracket
        (unPeu acquire r)
        (\resource exitCase -> unPeu (release resource exitCase) r)
        (\resource -> unPeu (use resource) r)

-- instance LiftRegion

-------------------------------------------------------------------------------
-- Output
-------------------------------------------------------------------------------

class Output str where
    outputSgr :: TracerPeu r w -> [ANSI.SGR] -> str -> Peu r ()
    outputErr :: TracerPeu r w ->               str -> Peu r ()

instance Char ~ a => Output [a] where
    outputSgr tracer s msg = traceWith tracer (TraceStdout s msg)
    outputErr tracer   msg = traceWith tracer (TraceStderr msg)

instance Output ByteString where
    outputSgr tracer s = outputSgr tracer s . fromUTF8BS
    outputErr tracer   = outputErr tracer   . fromUTF8BS

instance Output LazyByteString where
    outputSgr tracer s = outputSgr tracer s . LBS.toStrict
    outputErr tracer   = outputErr tracer . LBS.toStrict

output :: Output str => Tracer (Peu r) (Trace w) -> str -> Peu r ()
output tracer = outputSgr tracer []

-------------------------------------------------------------------------------
-- Configuration
-------------------------------------------------------------------------------

class Warning w where
    warningToFlag :: w -> String

instance Warning Void where
    warningToFlag = absurd

-------------------------------------------------------------------------------
-- Diagnostics
-------------------------------------------------------------------------------

defaultTracer
    :: Warning w
    => Bool      -- ^ supports ansi
    -> TimeSpec  -- ^ start clock
    -> Tracer (Peu r) (Trace w)
defaultTracer supportsAnsi startClock = Tracer $ \_cs tr -> liftIO $ do
    now <- getTime Monotonic
    let ts = diffTimeSpec now startClock
    let off = printf "[%10.5f] " (timespecToDurr ts)
    let setSgr | supportsAnsi = ANSI.setSGRCode
               | otherwise    = const ""

    case tr of
        TraceNoop           -> return ()

        TraceStdout sgr msg -> outputConcurrent (setSgr sgr ++ msg ++ "\n")
        TraceStderr     msg -> errorConcurrent (msg ++ "\n")

        TraceWarning w msg -> do
            let sgr :: [ANSI.SGR]
                sgr =
                    [ ANSI.SetConsoleIntensity ANSI.BoldIntensity
                    , ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Magenta
                    ]
            errorConcurrent $ concat
                [ off
                , setSgr sgr
                , "warning"
                , setSgr []
                , "["
                , setSgr sgr
                , "-W"
                , warningToFlag w
                , setSgr []
                , "]: "
                , msg
                , "\n"
                ]

        -- Generic trace messages
        TraceError msg -> traceImpl setSgr ANSI.Red   off "error" msg
        TraceDebug msg -> traceImpl setSgr ANSI.Blue  off "debug" msg
        TraceInfo  msg -> traceImpl setSgr ANSI.Green off "info"  msg

        TraceProcess pid (TraceProcessStart cwd cmd args) -> traceImpl setSgr ANSI.Blue off "process.call" $ unwords $
             pid' : cwd' : cmd : args
           where
             pid' = show pid
             cwd' = "cwd=" ++ toFilePath cwd

        TraceProcess pid (TraceProcessRunTime ts'@(TimeSpec secs _))
            | secs < 10 -> return ()
            | otherwise -> traceImpl setSgr ANSI.Blue off "process.time" $ printf "%d %.03f seconds" pid (timespecToDurr ts')

timespecToDurr :: TimeSpec -> Double
timespecToDurr (TimeSpec s ns) = fromIntegral s + fromIntegral ns / 1e9

traceImpl
    :: ([ANSI.SGR] -> String)
    -> ANSI.Color
    -> String
    -> String
    -> String
    -> IO ()
traceImpl setSgr clr off pfx msg = do
    let sgr :: [ANSI.SGR]
        sgr =
            [ ANSI.SetConsoleIntensity ANSI.BoldIntensity
            , ANSI.SetColor ANSI.Foreground ANSI.Dull clr
            ]
    errorConcurrent $ concat
        [ off
        , setSgr sgr
        , pfx
        , ": "
        , setSgr []
        , msg
        , "\n"
        ]

type TracerPeu r w = Tracer (Peu r) (Trace w)

putDebug :: HasCallStack => TracerPeu r w -> String -> Peu r ()
putDebug tracer msg = traceWith tracer (TraceDebug msg)

putInfo :: HasCallStack => TracerPeu r w -> String -> Peu r ()
putInfo tracer msg = traceWith tracer (TraceInfo msg)

putWarning :: Warning w => TracerPeu r w -> w -> String -> Peu r ()
putWarning tracer w msg = traceWith tracer (TraceWarning w msg)

putError :: HasCallStack => TracerPeu r w -> String -> Peu r ()
putError tracer msg = traceWith tracer (TraceError msg)

die :: TracerPeu r w -> String -> Peu r a
die tracer msg = putError tracer msg *> exitFailure

-------------------------------------------------------------------------------
-- Control.Exception
-------------------------------------------------------------------------------

evaluate :: NFData a => a -> Peu r a
evaluate = liftIO . X.evaluate

evaluateForce :: NFData a => a -> Peu r a
evaluateForce = evaluate . force

-------------------------------------------------------------------------------
-- System.Environment
-------------------------------------------------------------------------------

getArgs :: Peu r [String]
getArgs = liftIO X.getArgs

lookupEnv :: String -> Peu r (Maybe String)
lookupEnv = liftIO . X.lookupEnv

-------------------------------------------------------------------------------
-- System.Exit
-------------------------------------------------------------------------------

exitFailure :: Peu r a
exitFailure = liftIO X.exitFailure
