-- |
-- SPDX-License-Identifier: GPL-2.0-or-later OR BSD-3-Clause
-- Copyright:               Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Debug and development methods
--
module Peura.Debug (
    error,
    undefined,
    trace,
    traceShow,
    traceShowId,
    ) where

import qualified Prelude
import qualified Debug.Trace

undefined :: a
undefined = Prelude.undefined
{-# DEPRECATED undefined "Don't leave me in the code" #-}

error :: Prelude.String -> a
error = Prelude.error
{-# DEPRECATED error "Don't leave me in the code" #-}

trace :: Prelude.String -> b -> b
trace = Debug.Trace.trace
{-# DEPRECATED trace "Don't leave me in the code" #-}

traceShow :: Prelude.Show a => a -> b -> b
traceShow = Debug.Trace.traceShow
{-# DEPRECATED traceShow "Don't leave me in the code" #-}

traceShowId :: Prelude.Show a => a -> a
traceShowId = Debug.Trace.traceShowId
{-# DEPRECATED traceShowId "Don't leave me in the code" #-}
