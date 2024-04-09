{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Lenses in the style of [System.FilePath.Lens](https://hackage.haskell.org/package/lens/docs/System-FilePath-Lens.html).
--
-- @since 0.2.0.0
module System.Path.Lens
    ( -- * Operators
      (</>~)
    , (<.>~)
      -- * Lenses
    , basename
    , directory
    , extension
    , filename
    ) where

import           Data.Functor          as Fun
import           Data.Functor.Identity
import           System.Path

----------------------------------------------------------------------------

infixr 4 </>~
(</>~) :: ASetter s t (Path a) (Path a) -> (Path Unrooted) -> s -> t
l </>~ n = overSafe l (</> n)

infixr 4 <.>~
(<.>~) :: ASetter s t (Path a) (Path a) -> FileExt -> s -> t
l <.>~ n = overSafe l (<.> n)

----------------------------------------------------------------------------

basename :: Lens' (Path a) (Path Unrooted)
basename f p = (<.?> takeExtension p) . (takeDirectory p </>) Fun.<$> f (takeBaseName p)

-- local helper
(<.?>) :: Path a -> Maybe FileExt -> Path a
fp <.?> Nothing = fp
fp <.?> Just fe = fp <.> fe

directory :: Lens' (Path a) (Path a)
directory f p = (</> takeFileName p) <$> f (takeDirectory p)

extension :: Lens' (Path a) (Maybe FileExt)
extension f p = (n <.?>) <$> f e
  where
    (n, e) = splitExtension p

filename :: Lens' (Path a) (Path Unrooted)
filename f p = (takeDirectory p </>) <$> f (takeFileName p)

----------------------------------------------------------------------------
-- internal lens-api definitions
type ASetter s t a b = (a -> Identity b) -> s -> Identity t
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

{-# INLINE overSafe #-}
overSafe :: ASetter s t a b -> (a -> b) -> s -> t
overSafe l f = runIdentity `g` (l (Identity `h` f))
  where
    h _ = (Identity .)
    g _ = (runIdentity .)

{- unsafe/efficient variant of 'overSafe'

over :: ASetter s t a b -> (a -> b) -> s -> t
over l f = runIdentity #. l (Identity #. f)

infixr 9 #.
(#.) :: Coercible c b => (b -> c) -> (a -> b) -> (a -> c)
(#.) _ = coerce (\x -> x :: b) :: forall a b. Coercible b a => a -> b

-}
