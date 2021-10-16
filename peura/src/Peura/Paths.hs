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
    fromAbsoluteFilePathMaybe,
    root,
    -- * Directory
    getCurrentDirectory,
    getTemporaryDirectory,
    getAppUserDataDirectory,
    makeAbsoluteFilePath,
    doesPathExist,
    doesFileExist,
    doesDirectoryExist,
    pathIsSymbolicLink,
    createDirectoryIfMissing,
    listDirectory,
    removePathForcibly,
    canonicalizePath,
    getFileSize,
    copyFile,
    -- * Symbolic links
    createFileLink,
    getSymbolicLinkTarget,
    -- * Roots
    Tar,
    fromTarPath,
    XdgCache,
    XdgConfig,
    -- * Extras
    recursiveListDirectoryFiles,
    ) where

import System.Path
       (Absolute, FsPath (..), Path, Unrooted, fromFilePath,
       fromUnrootedFilePath, takeDirectory, takeFileName, toFilePath,
       takeExtension, FileExt (..),
       toUnrootedFilePath, (</>))
import System.Path.Unsafe (Path (..))

import qualified System.FilePath as FP
import qualified System.Directory as D
import qualified System.Path      as P
import qualified System.Path.IO   as P

import Peura.Exports
import Peura.Monad

fromAbsoluteFilePathMaybe :: FilePath -> Maybe (Path Absolute)
fromAbsoluteFilePathMaybe fp
    | FP.isAbsolute fp = Just (P.fromAbsoluteFilePath fp)
    | otherwise        = Nothing

-------------------------------------------------------------------------------
-- Directory
-------------------------------------------------------------------------------

getCurrentDirectory :: Peu r (Path Absolute)
getCurrentDirectory = liftIO P.getCurrentDirectory

getTemporaryDirectory :: Peu r (Path Absolute)
getTemporaryDirectory = liftIO P.getTemporaryDirectory

makeAbsoluteFilePath :: FilePath -> Peu r (Path Absolute)
makeAbsoluteFilePath = liftIO . P.makeAbsolute . fromFilePath

getAppUserDataDirectory :: String -> Peu r (Path Absolute)
getAppUserDataDirectory app = do
    x <- liftIO $ D.getAppUserDataDirectory app
    makeAbsoluteFilePath x

doesPathExist :: Path Absolute -> Peu r Bool
doesPathExist = liftIO . D.doesPathExist . toFilePath

doesFileExist :: Path Absolute -> Peu r Bool
doesFileExist = liftIO . D.doesFileExist . toFilePath

doesDirectoryExist :: Path Absolute -> Peu r Bool
doesDirectoryExist = liftIO . D.doesDirectoryExist . toFilePath

pathIsSymbolicLink :: Path Absolute -> Peu r Bool
pathIsSymbolicLink = liftIO . D.pathIsSymbolicLink . toFilePath

createDirectoryIfMissing :: Bool -> Path Absolute -> Peu r ()
createDirectoryIfMissing b = liftIO . D.createDirectoryIfMissing b . toFilePath

listDirectory :: Path Absolute -> Peu r [Path Unrooted]
listDirectory = fmap (map fromUnrootedFilePath) . liftIO . D.listDirectory . toFilePath

removePathForcibly :: Path Absolute -> Peu r ()
removePathForcibly = liftIO . D.removePathForcibly . toFilePath

canonicalizePath :: Path Absolute -> Peu r (Path Absolute)
canonicalizePath = liftIO . fmap P.fromAbsoluteFilePath . D.canonicalizePath . toFilePath

getFileSize :: Path Absolute -> Peu r Integer
getFileSize = liftIO . D.getFileSize . toFilePath

copyFile :: Path Absolute -> Path Absolute -> Peu r ()
copyFile src tgt = liftIO $ D.copyFile (toFilePath src) (toFilePath tgt)

-------------------------------------------------------------------------------
-- Symbolic links
-------------------------------------------------------------------------------

createFileLink :: Path Absolute -> Path Absolute -> Peu r ()
createFileLink target link = liftIO $ D.createFileLink (toFilePath target) (toFilePath link)

getSymbolicLinkTarget :: Path Absolute -> Peu r (Path Absolute)
getSymbolicLinkTarget p =
    liftIO . fmap mk . D.getSymbolicLinkTarget . toFilePath $ p
  where
    dir = takeDirectory p
    mk p' | FP.isAbsolute p' = P.fromAbsoluteFilePath p'
          | otherwise        = dir </> fromUnrootedFilePath p'

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

class MakeAbsolute p where
    makeAbsolute :: p -> Peu r (Path Absolute)

instance MakeAbsolute FsPath where 
    makeAbsolute = liftIO . P.makeAbsolute

instance P.FsRoot root => MakeAbsolute (Path root) where
    makeAbsolute = makeAbsolute . P.FsPath

-------------------------------------------------------------------------------
-- Recursive traversal
-------------------------------------------------------------------------------

recursiveListDirectoryFiles :: Path Absolute -> Peu r [Path Unrooted]
recursiveListDirectoryFiles r = do
    contents <- listDirectory r
    go id contents
  where
    go :: ([Path Unrooted] -> [Path Unrooted]) -> [Path Unrooted] -> Peu r [Path Unrooted]
    go !acc []     = return (acc [])
    go !acc (p:ps) = do
        dir <- doesDirectoryExist (r </> p) 
        if dir
        then do
            contents <- listDirectory r
            go acc (map (p </>) contents ++ ps)
        else do
            file <- doesFileExist (r </> p)
            if file
            then go (acc . (p :)) ps
            else go acc ps
