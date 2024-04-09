{-# LANGUAGE CPP  #-}
{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- compat layer
module System.Path.Internal.Compat
    ( Applicative(..)
    , (<$>)
    , dirMakeAbsolute
    , posixIsExtensionOf
    ) where

import           Control.Applicative
import           Data.List             (isSuffixOf)
import qualified System.Directory      as Dir
import qualified System.FilePath       as FP.Native
import qualified System.FilePath.Posix as FP.Posix

dirMakeAbsolute :: FilePath -> IO FilePath
#if MIN_VERSION_directory(1,2,2)
dirMakeAbsolute = Dir.makeAbsolute
#else
-- copied implementation from the directory package
dirMakeAbsolute = (FP.Native.normalise <$>) . absolutize
  where
    absolutize path -- avoid the call to `getCurrentDirectory` if we can
      | FP.Native.isRelative path
                  = (FP.Native.</> path)
                  . FP.Native.addTrailingPathSeparator <$>
                    Dir.getCurrentDirectory
      | otherwise = return path
#endif


posixIsExtensionOf :: String -> FilePath -> Bool
#if MIN_VERSION_filepath(1,4,2)
posixIsExtensionOf = FP.Posix.isExtensionOf
#else
posixIsExtensionOf ext@('.':_) = isSuffixOf ext . FP.Posix.takeExtensions
posixIsExtensionOf ext         = isSuffixOf ('.':ext) . FP.Posix.takeExtensions
#endif
