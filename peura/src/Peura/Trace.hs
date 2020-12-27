{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
module Peura.Trace (
    Trace (..),
    IsPeuraTrace (..),
    TracerPeu,
    makeTracerPeu,
    traceApp,
    -- * Tracer options
    TracerOptions (..),
    defaultTracerOptions,
    tracerOptionsParser,
    -- *
    putDebug,
    putInfo,
    putWarning,
    putError,
) where

import Data.Foldable             (asum)
import Data.IORef                (IORef, atomicModifyIORef', newIORef)
import Data.List                 (intercalate, stripPrefix)
import System.Clock              (Clock (Monotonic), TimeSpec (..), diffTimeSpec, getTime)
import System.Console.Concurrent (errorConcurrent, outputConcurrent)
import System.IO                 (stderr)
import System.IO.Unsafe          (unsafePerformIO)
import Text.Printf               (printf)

import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import qualified Options.Applicative as O
import qualified System.Console.ANSI as ANSI
import qualified Text.EditDistance   as ED

import Peura.Cabal
import Peura.Exports
import Peura.GHC
import Peura.Monad
import Peura.Paths
import Peura.Process
import Peura.Tracer
import Peura.Warning

-------------------------------------------------------------------------------
-- Trace
-------------------------------------------------------------------------------

-- | Combining 'Trace' for all @peura@ trace messages
data Trace tr
    = TraceStdout [ANSI.SGR] String
    | TraceStderr String

    | TraceDebug String
    | TraceInfo String
    | TraceError String

    | TraceWarning (TraceW tr) String

    | TracePeu TracePeu
    | TraceProcess TracePID TraceProcess
    | TraceCabal TraceCabal
    | TraceGhc TraceGhc

    | TraceApp tr

deriving instance (Show (TraceW tr), Show tr) => Show (Trace tr)

-------------------------------------------------------------------------------
-- Process
-------------------------------------------------------------------------------

instance MakeProcessTracer (Trace tr) where
    makeProcessTracer tracer = do
        pid <- newPid
        return (contramap (TraceProcess pid) tracer)

-- | Process ids used to "Peura.Process" traces 'TraceProcess'.
type TracePID = Natural

pidRef :: IORef TracePID
pidRef = unsafePerformIO (newIORef 0)
{-# NOINLINE pidRef #-}

newPid :: Peu r TracePID
newPid = liftIO $ atomicModifyIORef' pidRef $ \pid -> (pid + 1, pid)

-------------------------------------------------------------------------------
-- Cabal
-------------------------------------------------------------------------------

instance MakeCabalTracer (Trace tr) where
    makeCabalTracer = return . contramap TraceCabal

-------------------------------------------------------------------------------
-- Peu
-------------------------------------------------------------------------------

instance MakePeuTracer (Trace tr) where
    makePeuTracer = contramap TracePeu

-------------------------------------------------------------------------------
-- GHC
-------------------------------------------------------------------------------

instance MakeGhcTracer (Trace tr) where
    makeGhcTracer = return . contramap TraceGhc

-------------------------------------------------------------------------------
-- Warning
-------------------------------------------------------------------------------

class Warning (TraceW tr) => IsPeuraTrace tr where
    type TraceW tr :: Type

    -- | Way to show tracer, used by console printer
    showTrace :: tr -> (ANSI.Color, [String], String)

instance IsPeuraTrace Void where
    type TraceW Void = Void
    showTrace = absurd

instance Warning w => IsPeuraTrace (V1 w) where
    type TraceW (V1 w) = w
    showTrace x = case x of {}

traceApp :: TracerPeu r tr -> tr -> Peu r ()
traceApp tracer tr = traceWith tracer (TraceApp tr)

-------------------------------------------------------------------------------
-- Trace options
-------------------------------------------------------------------------------

data TracerOptions w = TracerOptions
    { tracerOptionsEnabledWarnings :: Set w
    , tracerOptionsProcess         :: Bool
    }
  deriving (Show)

defaultTracerOptions :: Warning w => TracerOptions w
defaultTracerOptions = TracerOptions
    { tracerOptionsEnabledWarnings = Set.fromList universeF
    , tracerOptionsProcess         = True
    }

tracerOptionsParser :: forall w. Warning w => O.Parser (TracerOptions w -> TracerOptions w)
tracerOptionsParser = fmap (foldr (flip (.)) id) $ many $ asum $
    warnings : traces
  where
    traces =
        [ O.flag'
            (\opts -> opts { tracerOptionsProcess = True })
            (O.long "trace-process" <> O.help "Trace process executions")
        , O.flag'
            (\opts -> opts { tracerOptionsProcess = False })
            (O.long "no-trace-process" <> O.hidden)
        ]

    warnings = O.option (O.eitherReader warningE) (O.short 'W' <> O.metavar "warning")

    warningE :: String -> Either String (TracerOptions w -> TracerOptions w)
    warningE s
        | Just w <- Map.lookup s allws
        = Right (\opts -> opts { tracerOptionsEnabledWarnings = Set.insert w (tracerOptionsEnabledWarnings opts) })

        | Just s' <- stripPrefix "no-" s
        , Just w <- Map.lookup s' allws
        = Right (\opts -> opts { tracerOptionsEnabledWarnings = Set.delete w (tracerOptionsEnabledWarnings opts) })

        | otherwise
        = Left $ "Unknown warning flag: " ++ s ++ suggestion
      where
        matchingW :: Map Int (NonEmpty String)
        matchingW = Map.fromListWith (<>)
            [ (ED.levenshteinDistance ED.defaultEditCosts x s, x :| [])
            | w <- universeF
            , let x' = warningToFlag (w :: w)
            , x <- [x', "no-" ++ x']
            ]

        suggestion = case Map.minView matchingW of
            Nothing      -> ""
            Just (ws, _) -> ". Did you mean one of: " ++ unwords (toList ws)

    allws :: Map String w
    allws = Map.fromList
        [ (warningToFlag w , w)
        | w <- universeF
        ]

-------------------------------------------------------------------------------
-- Make tracer
-------------------------------------------------------------------------------

makeTracerPeu
    :: forall tr m. (MonadIO m, IsPeuraTrace tr)
    => TracerOptions (TraceW tr)
    -> IO (Tracer m (Trace tr))
makeTracerPeu TracerOptions {..} = do
    supportsAnsi <- ANSI.hSupportsANSI stderr
    startClock   <- getTime Monotonic

    return $ Tracer $ \_cs tr0 -> liftIO $ do
        now <- getTime Monotonic
        let ts = diffTimeSpec now startClock
        let off = printf "[%10.5f] " (timespecToDurr ts)
        let setSgr | supportsAnsi = ANSI.setSGRCode
                   | otherwise    = const ""

        case tr0 of
            TraceStdout sgr msg -> outputConcurrent (setSgr sgr ++ msg ++ "\n")
            TraceStderr     msg -> errorConcurrent (msg ++ "\n")

            TraceWarning w msg -> when (w `Set.member` tracerOptionsEnabledWarnings) $ do
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

            TraceApp tr -> case showTrace tr of
                (colour, components, msg) -> traceImpl setSgr colour off components msg

            -- Generic trace messages
            TraceError msg -> traceImpl setSgr ANSI.Red   off ["error"] msg
            TraceDebug msg -> traceImpl setSgr ANSI.Blue  off ["debug"] msg
            TraceInfo  msg -> traceImpl setSgr ANSI.Green off ["info" ] msg

            TraceProcess pid (TraceProcessStart cwd cmd args) ->
                traceImpl setSgr ANSI.Blue off ["process", show pid, "start"] $ unwords $
                cwd' : cmd : args
              where
                cwd' = "cwd=" ++ toFilePath cwd

            TraceProcess pid (TraceProcessRunTime ts'@(TimeSpec secs _))
                | secs < 10 -> return ()
                | otherwise -> when tracerOptionsProcess $
                    traceImpl setSgr ANSI.Blue off ["process", show pid, "time"] $ printf "%.03f seconds" (timespecToDurr ts')

            TraceProcess pid (TraceProcessFailedCheck ec out err) -> when tracerOptionsProcess $ do
                traceImpl setSgr ANSI.Red off ["process", show pid, "failed"] $ "Exitcode " ++ show ec
                errorConcurrent ("========= stdout =========" :: String)
                errorConcurrent (fromUTF8BS (toStrict out))
                errorConcurrent ("========= stderr =========" :: String)
                errorConcurrent (fromUTF8BS (toStrict err))

            TracePeu TracePeuCompleted -> when tracerOptionsProcess $
                traceImpl setSgr ANSI.Green off ["peu", "completed"] "OK"

            TracePeu (TracePeuDie msg) ->
                traceImpl setSgr ANSI.Red off ["peu", "die"] msg

            TracePeu (TracePeuException exc) -> do
                traceImpl setSgr ANSI.Red off ["peu", "exception"] (typeNameOf exc)
                errorConcurrent $ displayException exc ++ "\n"

            -- TODO: summarise plan?
            TraceCabal (TraceCabalEphemeralPlan _pi) -> do
                traceImpl setSgr ANSI.Blue off ["cabal","plan"] "... plan input summary TODO ..."

            TraceCabal TraceCabalHackageIndexMetadata -> do
                traceImpl setSgr ANSI.Green off ["cabal","hackage"] "Reading Hackage index metadata"

            TraceGhc (TraceGhcReadPackageDb p) -> do
                traceImpl setSgr ANSI.Blue off ["ghc", "read-package-db"] $
                    toFilePath p

            TraceGhc (TraceGhcGetInfo ghc) -> do
                traceImpl setSgr ANSI.Blue off ["ghc", "info"] ghc

            TraceGhc (TraceGhcFindGhcPkg ghcInfo) -> do
                traceImpl setSgr ANSI.Blue off ["ghc", "find-ghc-pkg"] (show ghcInfo) -- TODO: pretty

            TraceGhc (TraceGhcFindGhcPkgResult ghcPkg) -> do
                traceImpl setSgr ANSI.Blue off ["ghc", "find-ghc-pkg", "result"] ghcPkg


-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

typeNameOf :: forall x. Typeable x => x -> String
typeNameOf _ = show $ (typeRep :: TypeRep x)

timespecToDurr :: TimeSpec -> Double
timespecToDurr (TimeSpec s ns) = fromIntegral s + fromIntegral ns / 1e9

-------------------------------------------------------------------------------
-- Trace implementation
-------------------------------------------------------------------------------

traceImpl
    :: ([ANSI.SGR] -> String)
    -> ANSI.Color
    -> String
    -> [String]
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
        , intercalate "." pfx
        , ": "
        , setSgr []
        , msg
        , "\n"
        ]

-------------------------------------------------------------------------------
-- Output
-------------------------------------------------------------------------------

type TracerPeu r tr = Tracer (Peu r) (Trace tr)

putDebug :: HasCallStack => Tracer m (Trace tr) -> String -> m ()
putDebug tracer msg = traceWith tracer (TraceDebug msg)

putInfo :: HasCallStack => TracerPeu r tr -> String -> Peu r ()
putInfo tracer msg = traceWith tracer (TraceInfo msg)

putWarning :: IsPeuraTrace tr => TracerPeu r tr -> TraceW tr -> String -> Peu r ()
putWarning tracer w msg = traceWith tracer (TraceWarning w msg)

putError :: HasCallStack => TracerPeu r tr -> String -> Peu r ()
putError tracer msg = traceWith tracer (TraceError msg)
