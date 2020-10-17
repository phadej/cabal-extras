{-# LANGUAGE FlexibleInstances #-}
-- | TODO
module Peura.Run (
    -- * Output
    Output (..),
    output,
    -- * Control.Exception
    evaluate,
    evaluateForce,
    -- * System.Environment
    getArgs,
    lookupEnv,
) where

import qualified Control.Exception    as X
import qualified Data.ByteString.Lazy as LBS
import qualified System.Console.ANSI  as ANSI
import qualified System.Environment   as X

import Peura.Exports
import Peura.Monad
import Peura.Trace
import Peura.Tracer

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
