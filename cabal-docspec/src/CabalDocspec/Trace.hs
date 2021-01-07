{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module CabalDocspec.Trace where

import Peura

import Text.Printf (printf)

import qualified Cabal.Plan              as Plan
import qualified Data.Text               as T
import qualified Distribution.ModuleName as C
import qualified System.Console.ANSI     as ANSI

import CabalDocspec.Summary
import CabalDocspec.Warning

data Tr
    = TraceComponent PackageIdentifier Plan.CompName
    | TracePhase1 C.ModuleName (Path Absolute)
    | TracePhase2 C.ModuleName
    | TraceGHCi FilePath [String]
    | TraceGHCiInput String
    | TraceSummary Summary
  deriving Show

instance IsPeuraTrace Tr where
    type TraceW Tr = W

    showTrace (TraceComponent pid cn)     = (ANSI.Green, ["docspec","component"], prettyShow pid ++ " " ++ T.unpack (Plan.dispCompName cn))
    showTrace (TracePhase1 n p)           = (ANSI.Green, ["docspec","phase1"], prettyShow n ++ ": " ++ toFilePath p)
    showTrace (TracePhase2 n)             = (ANSI.Green, ["docspec","phase2"], prettyShow n)
    showTrace (TraceGHCi p args)          = (ANSI.Blue, ["ghci"], unwords (p : args))
    showTrace (TraceGHCiInput input)      = (ANSI.Blue, ["ghci", "input"], input)
    showTrace (TraceSummary Summary {..}) = (ANSI.Green, ["doctest.summary"], str) where
        str = unlines $
            [ ""
            , showSs "Total:     " total
            , showSs "Examples:  " sExamples
            ]
            ++ [ showSs "Properties:" sProperties  | sProperties /= mempty ]
            ++ [ showSs "Setup:     " sSetup       | sSetup      /= mempty ]

        total :: SubSummary
        total = sSetup <> sExamples <> sProperties

        showSs :: String -> SubSummary -> String
        showSs n SubSummary {..} = printf "%s %d; Tried: %d; Skipped: %d; Success: %d; Errors: %d; Failures %d"
            n
            _ssTotal
            _ssTried
            _ssSkipped
            _ssSuccess
            _ssErrors
            _ssFailures
