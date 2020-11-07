{-# LANGUAGE DeriveFunctor #-}
module CabalBundler.ExeOption where

import Peura
import Prelude ()

import Distribution.Types.UnqualComponentName (UnqualComponentName)


data ExeOption a
    = ExeOptionPkg a                 -- ^ derive from package identifier
    | ExeOptionAll                   -- ^ all executables
    | ExeOption UnqualComponentName  -- ^ given name
  deriving (Functor)
