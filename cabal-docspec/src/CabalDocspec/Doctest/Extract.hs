{-# LANGUAGE DeriveFunctor #-}
module CabalDocspec.Doctest.Extract where

import Peura

import qualified Distribution.ModuleName as C

-------------------------------------------------------------------------------
-- Datatype from doctest Extract module
-------------------------------------------------------------------------------

-- | Documentation for a module grouped together with the modules name.
data Module a = Module
    { moduleName    :: C.ModuleName
    , moduleSetup   :: Maybe a
    , moduleContent :: [a]
    }
  deriving (Eq, Show, Functor)
