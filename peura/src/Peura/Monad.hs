{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Peura.Monad (
    -- * Peu monad
    Peu (..),
    runPeu,
    -- * Combinators
    changePeu,
    -- * Trace
    TracePeu (..),
    MakePeuTracer (..),
    -- * Exiting
    die,
    exitFailure,
    ) where

import Control.Monad.Reader.Class (MonadReader (..))
import Data.Typeable              (cast)

import qualified Control.Exception as X
import qualified System.Exit       as X

import Peura.Exports
import Peura.Tracer

#ifdef MIN_VERSION_concurrent_output
import System.Console.Concurrent (withConcurrentOutput)
import System.Console.Regions    (displayConsoleRegions)
#else

withConcurrentOutput :: IO a -> IO a
withConcurrentOutput = id

displayConsoleRegions :: IO a -> IO a
displayConsoleRegions = id
#endif

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

-- | 'Peu' monad is a (custom) reader over 'IO'.
newtype Peu r a = Peu { unPeu :: r -> IO a }
  deriving stock Functor

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

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

-- TODO instance LiftRegion

-------------------------------------------------------------------------------
-- Combinators
-------------------------------------------------------------------------------

changePeu :: (r -> s) -> Peu s a -> Peu r a
changePeu f (Peu m) = Peu $ \r -> m (f r)

-------------------------------------------------------------------------------
-- System.Exit
-------------------------------------------------------------------------------

-- | Leave a suicide message and die.
die :: (MakePeuTracer t, HasCallStack) => Tracer (Peu r) t -> String -> Peu r a
die tracer msg = do
    traceWith (makePeuTracer tracer) (TracePeuDie msg)
    exitFailure

-- | Prefer 'die'.
exitFailure :: Peu r a
exitFailure = liftIO X.exitFailure

-------------------------------------------------------------------------------
-- Running
-------------------------------------------------------------------------------

-- | Trace of 'Peu' execution.
data TracePeu
    = TracePeuCompleted
    | TracePeuException SomeException
    | TracePeuDie String
  deriving (Show)

class MakePeuTracer t where
    makePeuTracer :: Tracer (Peu r) t -> Tracer (Peu r) TracePeu

instance MakePeuTracer TracePeu where
    makePeuTracer = id

runPeu
    :: forall r a t. (MakePeuTracer t, HasCallStack)
    => Tracer (Peu r) t -> r -> Peu r a -> IO a
runPeu tracer r m = withConcurrentOutput $ displayConsoleRegions $ do
    res <- unPeu m r `catch` handleException
    unPeu (traceWith (makePeuTracer tracer) TracePeuCompleted) r
    return res
  where
    handleException :: SomeException -> IO a
    handleException sexc@(X.SomeException exc) =
        case cast exc of
            Just (ec :: ExitCode) ->
                X.throwIO ec
            Nothing -> do
                unPeu (traceWith (makePeuTracer tracer) (TracePeuException sexc)) r
                X.exitFailure
