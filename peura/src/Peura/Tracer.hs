{-# LANGUAGE RankNTypes #-}
module Peura.Tracer (
    -- * Tracer
    Tracer (..),
    nullTracer,
    traceWith,
    hoistTracer,
) where

import GHC.Stack (callStack, HasCallStack, CallStack)

import Peura.Exports

-------------------------------------------------------------------------------
-- Tracer
-------------------------------------------------------------------------------

newtype Tracer m a = Tracer { traceWithCallStack :: CallStack -> a -> m () }

instance Contravariant (Tracer m) where
    contramap f (Tracer g) = Tracer (\cs -> g cs . f)

nullTracer :: Applicative m => Tracer m a
nullTracer = Tracer (\_ _ -> pure ())

traceWith :: HasCallStack => Tracer m a -> a -> m ()
traceWith (Tracer g) = g callStack

hoistTracer :: (forall x. m x -> n x) -> Tracer m a -> Tracer n a
hoistTracer nt (Tracer f) = Tracer $ \cs x -> nt (f cs x)
