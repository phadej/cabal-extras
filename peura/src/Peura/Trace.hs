module Peura.Trace (
    Trace (..),
    TracePID,
    TraceProcess (..),
) where

import System.Clock (TimeSpec)
import System.Path (Path, Absolute)

import qualified System.Console.ANSI  as ANSI

import Peura.Exports
-- import Peura.Paths

-- | Process ids useing by "Peura.Process" traces 'TraceProcess'.
type TracePID = Natural

-- this should be in own type.
data Trace w
    = TraceNoop

    | TraceStdout [ANSI.SGR] String
    | TraceStderr String

    | TraceDebug String
    | TraceInfo String
    | TraceWarning w String
    | TraceError String

    | TraceProcess TracePID TraceProcess

  deriving (Show)

data TraceProcess
    = TraceProcessStart (Path Absolute) String [String]
    | TraceProcessRunTime TimeSpec
  deriving (Show)
