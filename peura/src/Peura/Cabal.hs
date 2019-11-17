-- |
-- SPDX-License-Identifier: GPL-2.0-or-later
-- Copyright:               Oleg Grenrus <oleg.grenrus@iki.fi>
module Peura.Cabal (
    CabalPlanConvert (..),
    ) where

import qualified Distribution.Package           as C
import qualified Distribution.Version           as C
import qualified Cabal.Plan                     as P
import qualified Data.Text                      as T

-- | Convert between @Cabal@ and @cabal-plan@ types.
class CabalPlanConvert p c | p -> c, c -> p where
    toCabal   :: p -> c
    fromCabal :: c -> p

instance CabalPlanConvert P.Ver C.Version where
    toCabal (P.Ver vs) = C.mkVersion vs
    fromCabal v        = P.Ver (C.versionNumbers v)

instance CabalPlanConvert P.PkgName C.PackageName where
    toCabal (P.PkgName n) = C.mkPackageName (T.unpack n)
    fromCabal pn          = P.PkgName (T.pack (C.unPackageName pn))

instance CabalPlanConvert P.PkgId C.PackageIdentifier where
    toCabal (P.PkgId pn v) = C.PackageIdentifier (toCabal pn) (toCabal v)
    fromCabal (C.PackageIdentifier pn v) = P.PkgId (fromCabal pn) (fromCabal v)
