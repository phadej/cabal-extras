-- |
-- SDPX-License-Identifier: GPL-2.0-or-later
-- Copyright: Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Collection of exports from the dependencies.
--
module Peura.Exports (
    module Gentle.Introduction,
    -- ** MTL
    MonadCatch (..),
    MonadMask (..),
    MonadReader (..),
    MonadThrow (..),
    -- ** Serialisation
    Binary,
    Serialise,
    -- * Types
    ExitCode (..),
    LazyByteString,
    PackageName,
    PackageIdentifier (..),
    Version,
    VersionRange,
    UnitId,
    -- * Exceptions
    IOException,
    finally,
    onException,
    -- * Individual functions
    -- ** Control.Monad.Catch
    handle,
    bracket,
    -- ** Data.Maybe
    mapMaybe,
    catMaybes,
    -- ** Data.List
    sortBy, sortOn, ordNub,
    -- * Cabal
    prettyShow,
    mkPackageName,
    mkVersion,
    -- * GHC.Generics
    V1,
    -- * Optics
    (%),
    (^.), (^?),
    (.~), (?~), (%~),
    _1, _2,
    Ixed (..), At (..),
    coerced,
    folded,
    setOf,
    _Just,
    view,
    preview,
    review,
    matching,
    prism',
    ) where

import Gentle.Introduction hiding (error, traceShow, traceShowId, undefined)

import Codec.Serialise                 (Serialise)
import Control.Exception               (IOException)
import Control.Monad.Catch
       (MonadCatch (..), MonadMask (..), MonadThrow (..), bracket, finally, handle, onException)
import Control.Monad.Reader.Class      (MonadReader (ask, local))
import Data.Binary                     (Binary)
import Data.List                       (sortBy, sortOn)
import Data.Maybe                      (catMaybes, mapMaybe)
import Distribution.Pretty             (prettyShow)
import Distribution.Simple.Utils       (ordNub)
import Distribution.Types.PackageId    (PackageIdentifier (..))
import Distribution.Types.PackageName  (PackageName, mkPackageName)
import Distribution.Types.UnitId       (UnitId)
import Distribution.Types.Version      (Version, mkVersion)
import Distribution.Types.VersionRange (VersionRange)
import GHC.Generics                    (V1)
import System.Exit                     (ExitCode (..))

import qualified Data.ByteString.Lazy as LBS

import Data.Set.Optics (setOf)
import Optics.Core     (At (..), Ixed (..), coerced, folded, (%), (%~), (.~), (?~), (^.), (^?), _1, _2, _Just)
import Optics.Extra    (matching, preview, prism', review, view)

type LazyByteString = LBS.ByteString
