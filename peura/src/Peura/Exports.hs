-- |
-- SDPX-License-Identifier: GPL-2.0-or-later
-- Copyright: Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Collection of exports from the dependencies.
--
module Peura.Exports (
    module A,
    -- * Classes
    Alternative (..),
    Bifoldable (..),
    Bifunctor (..),
    Bitraversable (..),
    Coercible,
    Contravariant (..),
    Generic,
    Generic1,
    IsString (..),
    NFData (..),
    Semigroup (..),
    Strict (..),
    Typeable,
    -- ** MTL
    MonadCatch (..),
    MonadFail (..),
    MonadIO (..),
    MonadMask (..),
    MonadReader (..),
    MonadThrow (..),
    MonadUnliftIO (..),
    -- ** Serialisation
    Binary,
    Serialise,
    -- * Types
    ByteString,
    ExitCode (..),
    LazyByteString,
    Map,
    NonEmpty (..),
    PackageName,
    PackageIdentifier (..),
    Set,
    Text,
    These (..),
    Version,
    VersionRange,
    Void,
    UnitId,
    -- ** Numerals
    Int8, Int16, Int32, Int64,
    Word8, Word16, Word32, Word64,
    Natural,
    -- * Functors
    Proxy (..),
    Const (..),
    Identity (..),
    -- * Exceptions
    Exception (..),
    IOException,
    SomeException,
    finally,
    onException,
    -- * Individual functions
    -- * Control.Applicative
    optional,
    -- * Control.DeepSeq
    force,
    -- ** Control.Monad
    ap,
    foldM,
    unless,
    when,
    (<$!>),
    -- ** Control.Monad.Catch
    handle,
    bracket,
    -- ** Data.Bitraversable
    bimapDefault,  bifoldMapDefault,
    -- ** Data.Coercible
    coerce,
    -- ** Data.Foldable
    for_,
    traverse_,
    -- ** Data.Functor
    void,
    (<&>),
    -- ** Data.Maybe
    fromMaybe,
    mapMaybe,
    catMaybes,
    -- ** Data.List
    sortBy, sortOn, ordNub,
    -- ** Data.List.NonEmpty
    head, last, groupBy,
    -- ** Data.Traversable
    for,
    -- ** Data.Typeable
    typeRep,
    -- ** Data.Void
    absurd,
    -- * Cabal
    prettyShow,
    mkPackageName,
    mkVersion,
    -- ** UTF8
    fromUTF8BS, toUTF8BS,
    -- * Optics
    itraverse,
    itraverse_,
    ifor,
    ifor_,
    ix,
    at,
    -- * Optics operators
    view,
    preview,
    ) where

-- to get all members of Foldable
import Data.Foldable as A (Foldable (..), notElem)

-- Basic prelude stuff
import Prelude as A
       (Applicative (..), Bool (..), Bounded (..), Char, Either (..), Enum (..),
       Eq (..), FilePath, Functor (..), IO, Maybe (..), Monad (return, (>>=)),
       Monoid (..), Ord (..), Show (..), String, Traversable (..), all, and,
       any, concat, concatMap, const, curry, dropWhile, either, filter, flip,
       fst, id, map, maybe, not, or, otherwise, replicate, return, reverse, snd,
       span, take, takeWhile, uncurry, unlines, unwords, zipWith, ($!), ($),
       (&&), (++), (.), (<$>), (||))

-- numerics
import Prelude as A
       (Double, Fractional (..), Int, Integer, Integral (..), Num (..),
       Rational, Real (..), RealFrac (..), Word, fromIntegral, realToFrac)

import Codec.Serialise                 (Serialise)
import Control.Applicative             (Alternative (..), Const (..), optional)
import Control.DeepSeq                 (NFData (..), force)
import Control.Exception
       (Exception (..), IOException, SomeException)
import Control.Monad                   (ap, foldM, unless, when, (<$!>))
import Control.Monad.Catch
       (MonadCatch (..), MonadMask (..), MonadThrow (..), bracket, finally,
       handle, onException)
import Control.Monad.Fail              (MonadFail (..))
import Control.Monad.IO.Class          (MonadIO (..))
import Control.Monad.IO.Unlift         (MonadUnliftIO (withRunInIO))
import Control.Monad.Reader.Class      (MonadReader (ask, local))
import Data.Bifoldable                 (Bifoldable (..))
import Data.Bifunctor                  (Bifunctor (..))
import Data.Binary                     (Binary)
import Data.Bitraversable
       (Bitraversable (..), bifoldMapDefault, bimapDefault)
import Data.ByteString                 (ByteString)
import Data.Coerce                     (Coercible, coerce)
import Data.Foldable                   (for_, traverse_)
import Data.Functor.Compat             (void, (<&>))
import Data.Functor.Contravariant      (Contravariant (..))
import Data.Functor.Identity           (Identity (..))
import Data.Int                        (Int16, Int32, Int64, Int8)
import Data.List                       (sortBy, sortOn)
import Data.List.NonEmpty              (NonEmpty (..), groupBy, head, last)
import Data.Map.Strict                 (Map)
import Data.Maybe                      (catMaybes, fromMaybe, mapMaybe)
import Data.Proxy                      (Proxy (..))
import Data.Semigroup                  (Semigroup (..))
import Data.Set                        (Set)
import Data.Strict.Classes             (Strict (..))
import Data.String                     (IsString (..))
import Data.Text                       (Text)
import Data.These                      (These (..))
import Data.Traversable                (for)
import Data.Typeable                   (Typeable, typeRep)
import Data.Void                       (Void, absurd)
import Data.Word                       (Word16, Word32, Word64, Word8)
import Distribution.Pretty             (prettyShow)
import Distribution.Simple.Utils       (fromUTF8BS, ordNub, toUTF8BS)
import Distribution.Types.PackageId    (PackageIdentifier (..))
import Distribution.Types.PackageName  (PackageName, mkPackageName)
import Distribution.Types.UnitId       (UnitId)
import Distribution.Types.Version      (Version, mkVersion)
import Distribution.Types.VersionRange (VersionRange)
import GHC.Generics                    (Generic, Generic1)
import Numeric.Natural                 (Natural)
import Optics.Extra
       (at, ifor, ifor_, itraverse, itraverse_, ix, preview, view)
import System.Exit                     (ExitCode (..))

import qualified Data.ByteString.Lazy as LBS

type LazyByteString = LBS.ByteString
