{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
module CabalDocspec.Located where

import Peura

import Language.Haskell.Lexer (Pos, line, column)

type Located = GenLocated Pos

data GenLocated l e = L l e
  deriving (Eq, Show, Functor, Foldable, Traversable)

unLoc :: GenLocated l e -> e
unLoc (L _  e) = e

prettyPos :: Pos -> String
prettyPos pos = "in comment at " ++ show (line pos) ++ ":" ++ show (column pos)
