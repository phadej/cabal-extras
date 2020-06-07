module Peura.Glob (
-- * Globbing
globDir1,
globDir1First,
-- * Exceptions
GlobCompileException (..),
GlobNoMatchException (..),
) where

import qualified System.FilePath.Glob as Glob

import Peura.Monad
import Peura.Paths
import Peura.Exports

-- | Glob files in a directory
globDir1 :: String -> Path Absolute -> Peu r [Path Absolute]
globDir1 pat path = do
    pat' <- either (throwM . GlobCompileException) pure $
        Glob.tryCompileWith Glob.compDefault pat
    matches <- liftIO $ Glob.globDir1 pat' (toFilePath path)
    traverse makeAbsoluteFilePath matches

-- | Look for a single file using glob.
globDir1First :: String -> Path Absolute -> Peu r (Path Absolute)
globDir1First pat path = do
    pat' <- either (throwM . GlobCompileException) pure $
        Glob.tryCompileWith Glob.compDefault pat
    matches <- liftIO $ Glob.globDir1 pat' (toFilePath path)
    case matches of
        (p:_) -> makeAbsoluteFilePath p
        []    -> throwM $ GlobNoMatchException pat path

-------------------------------------------------------------------------------
-- Exceptions
-------------------------------------------------------------------------------

newtype GlobCompileException = GlobCompileException String
  deriving Show

instance Exception GlobCompileException

data GlobNoMatchException = GlobNoMatchException String (Path Absolute)
  deriving Show

instance Exception GlobNoMatchException
