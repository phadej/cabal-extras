-- |
-- SPDX-License-Identifier: GPL-2.0-or-later
-- Copyright:               Oleg Grenrus <oleg.grenrus@iki.fi>
module Peura.Async (
    Async.Async,
    withAsync,
    wait,
    async,
    ) where

import qualified Control.Concurrent.Async as Async

import Peura.Exports
import Peura.Monad

withAsync :: Peu r a -> (Async.Async a -> Peu r b) -> Peu r b
withAsync action inner = withRunInIO $ \runInIO ->
    Async.withAsync (runInIO action) $ \a -> runInIO (inner a)

-- | Use 'withAsync' is possible.
async :: Peu r a -> Peu r (Async.Async a)
async m = withRunInIO $ \runInIO -> Async.async (runInIO m)

wait :: Async.Async a -> Peu r a
wait = liftIO . Async.wait
