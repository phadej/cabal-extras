module CabalEnv.Warning where

import Peura

data W = WMissingCabalEnvData

instance Warning W where
    warningToFlag WMissingCabalEnvData = "missing-cabal-envdata"

die :: String -> Peu r a
die str = putError str *> exitFailure
