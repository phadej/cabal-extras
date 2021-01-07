{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
module CabalDocspec.Located where

import Peura

import Language.Haskell.Lexer (Pos)

type Located = GenLocated Pos

data GenLocated l e = L l e
  deriving (Eq, Show, Functor, Foldable, Traversable)

unLoc :: GenLocated l e -> e
unLoc (L _  e) = e
