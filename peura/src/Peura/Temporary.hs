-- |
-- SPDX-License-Identifier: GPL-2.0-or-later
-- Copyright:               Oleg Grenrus <oleg.grenrus@iki.fi>
module Peura.Temporary (
    withSystemTempDirectory,
    ) where

import qualified System.IO.Temp as Temp

import Peura.Exports
import Peura.Monad
import Peura.Paths

withSystemTempDirectory :: String -> (Path Absolute -> Peu r a) -> Peu r a
withSystemTempDirectory tmpl f = Temp.withSystemTempDirectory tmpl $ \p -> do
    a <- makeAbsoluteFilePath p
    f a
