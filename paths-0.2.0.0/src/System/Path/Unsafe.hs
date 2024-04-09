{-# LANGUAGE Unsafe #-}

-- | This module gives access to internals and operation which can
-- subvert the type-safety of 'Path'.
--
-- @since 0.2.0.0
module System.Path.Unsafe
    ( Path(Path)
    , castRoot
    , rootPath
    ) where

import           System.Path.Internal

-- | Reinterpret an unrooted path (/UNSAFE/)
--
-- This is an alias for 'castRoot'; see comments there.
--
rootPath :: Path Unrooted -> Path root
rootPath (Path fp) = Path fp

-- | Reinterpret the root of a path
--
-- This literally just changes the type-level tag; use with caution!
castRoot :: Path root -> Path root'
castRoot (Path fp) = Path fp
