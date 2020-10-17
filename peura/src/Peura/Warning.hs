module Peura.Warning where

import Peura.Exports

class (Finite w, Ord w) => Warning w where
    warningToFlag :: w -> String

instance Warning Void where
    warningToFlag = absurd
