{-# LANGUAGE CPP #-}

module System.Path.Internal.Native
    ( posixToNative
    , posixFromNative
    ) where

import qualified System.FilePath       as FP.Native
import qualified System.FilePath.Posix as FP.Posix

{-# INLINE isPosix #-}
isPosix :: Bool
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
isPosix = False
#else
isPosix = True
#endif

posixToNative :: FilePath -> FilePath
posixToNative
  | isPosix   = id
  | otherwise = FP.Native.joinPath . FP.Posix.splitDirectories

posixFromNative :: FilePath -> FilePath
posixFromNative
  | isPosix   = id
  | otherwise = FP.Posix.joinPath . FP.Native.splitDirectories
