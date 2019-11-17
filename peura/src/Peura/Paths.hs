-- |
-- SPDX-License-Identifier: GPL-2.0-or-later OR BSD-3-Clause
-- Copyright:               Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Additions to @paths@ interface.
-- Hopefully something can be upstreamed.
--
module Peura.Paths (
    module System.Path,
    makeAbsolute,
    root,
    -- * Directory
    getCurrentDirectory,
    makeAbsoluteFilePath,
    getAppUserDataDirectory,
    doesFileExist,
    createDirectoryIfMissing,
    -- * Roots
    Tar,
    fromTarPath,
    XdgCache,
    XdgConfig,
    ) where

import System.Path
       (Absolute, FsPath (..), Path, Unrooted, fromFilePath,
       fromUnrootedFilePath, takeDirectory, takeFileName, toFilePath,
       toUnrootedFilePath, (</>))
import System.Path.Unsafe (Path (..))

import qualified System.Directory as D
import qualified System.Path      as P
import qualified System.Path.IO   as P

import Peura.Exports
import Peura.Monad

-------------------------------------------------------------------------------
-- Directory
-------------------------------------------------------------------------------

getCurrentDirectory :: Peu r (Path Absolute)
getCurrentDirectory = liftIO P.getCurrentDirectory

makeAbsoluteFilePath :: FilePath -> Peu r (Path Absolute)
makeAbsoluteFilePath = liftIO . P.makeAbsolute . fromFilePath

getAppUserDataDirectory :: String -> Peu r (Path Absolute)
getAppUserDataDirectory app = do
    x <- liftIO $ D.getAppUserDataDirectory app
    makeAbsoluteFilePath x

doesFileExist :: Path Absolute -> Peu r Bool
doesFileExist = liftIO . D.doesFileExist . toFilePath

createDirectoryIfMissing :: Bool -> Path Absolute -> Peu r ()
createDirectoryIfMissing b = liftIO . D.createDirectoryIfMissing b . toFilePath

-------------------------------------------------------------------------------
-- Tar root
-------------------------------------------------------------------------------

data Tar

fromTarPath :: Path Tar -> FilePath
fromTarPath (Path p) = p

-------------------------------------------------------------------------------
-- XDG Cache
-------------------------------------------------------------------------------

data XdgCache

instance P.FsRoot XdgCache where
    toAbsoluteFilePath (Path p) = D.getXdgDirectory D.XdgCache p

data XdgConfig

instance P.FsRoot XdgConfig where
    toAbsoluteFilePath (Path p) = D.getXdgDirectory D.XdgConfig p

-------------------------------------------------------------------------------
-- orphans
-------------------------------------------------------------------------------

root :: P.FsRoot root => Path root
root = Path ""

makeAbsolute :: FsPath -> Peu r (Path Absolute)
makeAbsolute = liftIO . P.makeAbsolute
