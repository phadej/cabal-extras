{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module CabalHasklint.Trace where

import Peura

import qualified Cabal.Plan              as Plan
import qualified Data.Text               as T
import qualified Distribution.ModuleName as C
import qualified System.Console.ANSI     as ANSI

import CabalHasklint.Warning

data Tr
    = TraceComponent PackageIdentifier Plan.CompName
    | TraceParse C.ModuleName (Path Absolute)
    | TraceLint C.ModuleName {- (Path Absolute) -}
  deriving Show

instance IsPeuraTrace Tr where
    type TraceW Tr = W

    showTrace (TraceComponent pid cn) = (ANSI.Green, ["hasklint","component"], prettyShow pid ++ " " ++ T.unpack (Plan.dispCompName cn))
    showTrace (TraceParse n p)        = (ANSI.Green, ["hasklint","parse"], prettyShow n ++ ": " ++ toFilePath p)
    showTrace (TraceLint n)           = (ANSI.Green, ["hasklint","lint"], prettyShow n)
