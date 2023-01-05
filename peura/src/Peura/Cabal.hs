-- |
-- SPDX-License-Identifier: GPL-2.0-or-later
-- Copyright:               Oleg Grenrus <oleg.grenrus@iki.fi>
module Peura.Cabal (
    -- * Conversion between Cabal and cabal-plan types
    CabalPlanConvert (..),
    -- * Getting PlanJson for ephemeral input
    PlanInput (..),
    emptyPlanInput,
    ephemeralPlanJson,
    ephemeralPlanJson',
    -- * Index
    cachedHackageMetadata,
    -- * Trace
    TraceCabal (..),
    MakeCabalTracer (..),
    ) where

import Peura.ByteString
import Peura.Exports
import Peura.Monad
import Peura.Paths
import Peura.Process
import Peura.Temporary
import Peura.Tracer

import Text.PrettyPrint ((<+>))

import qualified Cabal.Index                as I
import qualified Cabal.Plan                 as P
import qualified Data.Aeson                 as A
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import qualified Distribution.Fields.Pretty as C
import qualified Distribution.Package       as C
import qualified Distribution.Pretty        as C
import qualified Distribution.Types.Flag    as C
import qualified Distribution.Version       as C
import qualified Text.PrettyPrint           as PP

-------------------------------------------------------------------------------
-- Convert
-------------------------------------------------------------------------------

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

instance CabalPlanConvert P.UnitId C.UnitId where
    toCabal (P.UnitId u) = C.mkUnitId (T.unpack u)
    fromCabal u          = P.UnitId (T.pack (C.unUnitId u))

instance CabalPlanConvert P.FlagName C.FlagName where
    toCabal (P.FlagName u) = C.mkFlagName (T.unpack u)
    fromCabal u            = P.FlagName (T.pack (C.unFlagName u))

-------------------------------------------------------------------------------
-- Index
-------------------------------------------------------------------------------

cachedHackageMetadata
    :: MakeCabalTracer t
    => Tracer (Peu r) t
    -> Peu r (Map PackageName I.PackageInfo)
cachedHackageMetadata tracer = do
    tracer' <- makeCabalTracer tracer
    traceWith tracer' TraceCabalHackageIndexMetadata
    (_, meta) <- liftIO I.cachedHackageMetadata
    return meta

-------------------------------------------------------------------------------
-- plan.json input
-------------------------------------------------------------------------------

data PlanInput = PlanInput
    { piLibraries   :: Map PackageName VersionRange
    , piExecutables :: Map PackageName (VersionRange, Set String)
    , piPreferences :: Map PackageName VersionRange -- TODO: allow flags and installed
    , piConstraints :: Map PackageName VersionRange
    , piAllowNewer  :: Map PackageIdentifier (Set PackageName)
    , piCompiler    :: Maybe FilePath
    , piTarballs    :: [Path Absolute]
    , piDryRun      :: Bool
      -- ^ dry-run, whether only solve, or also build the dependencies.
    }
  deriving (Show)

emptyPlanInput :: PlanInput
emptyPlanInput = PlanInput
    { piLibraries   = Map.empty
    , piExecutables = Map.empty
    , piPreferences = Map.empty
    , piConstraints = Map.empty
    , piAllowNewer  = Map.empty
    , piCompiler    = Nothing
    , piTarballs    = []
    , piDryRun      = True
    }

-------------------------------------------------------------------------------
-- Trace
-------------------------------------------------------------------------------

data TraceCabal
    = TraceCabalEphemeralPlan PlanInput
    | TraceCabalHackageIndexMetadata
  deriving (Show)

class MakeCabalTracer t where
    makeCabalTracer :: Tracer (Peu r) t -> Peu r (Tracer (Peu r) TraceCabal)

instance MakeCabalTracer TraceCabal where
    makeCabalTracer = return

-------------------------------------------------------------------------------
-- Procedure to get PlanInput
-------------------------------------------------------------------------------

-- | Solve for a ephemeral plan input.
ephemeralPlanJson
    :: (MakeCabalTracer t, MakeProcessTracer t, MakePeuTracer t)
    => Tracer (Peu r) t
    -> PlanInput
    -> Peu r (Maybe P.PlanJson)
ephemeralPlanJson tracer = fmap (fmap snd) . ephemeralPlanJson' tracer

-- | Like 'ephemeralPlanJson', but also return the @plan.json@ original contents.
ephemeralPlanJson'
    :: (MakeCabalTracer t, MakeProcessTracer t, MakePeuTracer t)
    => Tracer (Peu r) t
    -> PlanInput
    -> Peu r (Maybe (ByteString, P.PlanJson))
ephemeralPlanJson' tracer pi = do
    tracer' <- makeCabalTracer tracer
    traceWith tracer' (TraceCabalEphemeralPlan pi)

    let cabalFile :: String
        cabalFile = fakePackage pi

    let projectFile :: String
        projectFile = cabalProject pi

    withSystemTempDirectory "peura-XXXX" $ \tmpDir -> do
        writeByteString (tmpDir </> fromUnrootedFilePath "fake-package.cabal") $ toUTF8BS cabalFile
        writeByteString (tmpDir </> fromUnrootedFilePath "cabal.project") $ toUTF8BS projectFile

        ec <- runProcessOutput tracer tmpDir "cabal" $
            ["v2-build", "all", "--builddir=dist-newstyle"] ++ ["--dry-run" | piDryRun pi ]

        case ec of
            ExitFailure _ -> return Nothing
            ExitSuccess   -> do
                planPath  <- liftIO $ P.findPlanJson (P.InBuildDir $ toFilePath $ tmpDir </> fromUnrootedFilePath "dist-newstyle")
                planPath' <- makeAbsoluteFilePath planPath
                planBS    <- readByteString planPath'
                plan      <- case A.eitherDecodeStrict' planBS of
                    Right x  -> return x
                    Left err -> die tracer $ "Cannot parse plan.json: " ++ err

                return $ Just (planBS, plan)

-------------------------------------------------------------------------------
-- FakePackage
-------------------------------------------------------------------------------

fakePackage :: PlanInput -> String
fakePackage pi = C.showFields (const C.NoComment)
    [ fi "cabal-version" $ PP.text "2.4"
    , fi "name"          $ PP.text "fake-package"
    , fi "version"       $ PP.text "0"

    , C.PrettySection () "library" []
        [ fi "default-language" "Haskell2010"
        , fi "build-depends" $ PP.text "base"
        , fi "build-depends" $ PP.vcat
            [ PP.comma <+> C.pretty pn <+> C.pretty vr
            | (pn, vr) <- Map.toList (piLibraries pi)
            ]
        , fi "build-tool-depends" $ PP.vcat
            [ PP.comma <+> C.pretty pn PP.<> PP.colon PP.<> PP.text exe <+> C.pretty vr
            | (pn, (vr, exes)) <- Map.toList (piExecutables pi)
            , exe <- Set.toList exes
            ]
        ]
    ]
  where
    fi = C.PrettyField ()

-------------------------------------------------------------------------------
-- FakeProject
-------------------------------------------------------------------------------

cabalProject :: PlanInput -> String
cabalProject pi = C.showFields (const C.NoComment) $
    [ fi "packages" $ PP.text "."
    ] ++
    [ fi "packages" $ PP.text (toFilePath path)
    | path <- piTarballs pi
    ] ++
    [ fi "constraints" $ PP.text "any." PP.<> C.pretty pn <+> C.pretty vr
    | (pn, vr) <- Map.toList (piConstraints pi)
    ] ++
    [ fi "allow-newer" $ C.pretty pid <> PP.char ':' <> C.pretty pn
    | (pid, pns) <- Map.toList (piAllowNewer pi)
    , pn <- Set.toList pns
    ] ++
    [ fi "with-compiler" $ PP.text compiler
    | compiler <- toList (piCompiler pi)
    ]
  where
    fi = C.PrettyField ()
