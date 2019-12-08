module CabalBundler.NixSingle (
    generateDerivationNix,
    )  where

import Peura

import Data.Maybe (maybeToList)

import qualified Cabal.Index          as I
import qualified Cabal.Plan           as P
import qualified Data.List.NonEmpty   as NE
import qualified Data.Map.Strict      as M
import qualified Data.Set             as S
import qualified Distribution.Package as C
import qualified Topograph            as TG

import CabalBundler.NixBase32
import CabalBundler.NixSingle.Input
import CabalBundler.NixSingle.Template

-------------------------------------------------------------------------------
-- generating output
-------------------------------------------------------------------------------

generateDerivationNix
    :: PackageName
    -> String
    -> P.PlanJson
    -> Map PackageName I.PackageInfo
    -> Peu r String
generateDerivationNix packageName exeName plan meta = do
    let units :: Map P.UnitId P.Unit
        units = P.pjUnits plan

    let pkgs :: Map P.PkgId (Set P.UnitId)
        pkgs = M.fromListWith S.union
            [ (P.uPId unit, S.singleton (P.uId unit))
            | unit <- M.elems units
            ]

    pkgIds <- packageGranularity units pkgs

    zhsdeps <- for (NE.sort pkgIds) $ \pkgId' -> do
        let PackageIdentifier pn ver = toCabal pkgId'
        pi <- strictLookup pn UnknownPackageName meta
        ri <- strictLookup ver (UnknownPackageVersion pn) (I.piVersions pi)
        return ZDep
            { zdepName     = prettyShow pn
            , zdepVersion  = prettyShow ver
            , zdepSha256   = encodeHash (I.riTarball ri)
            , zdepRevision = case I.riRevision ri of
                0 -> "{}"
                r -> "{ rev = " ++ show r ++ "; sha256 = \"" ++ encodeHash (I.riCabal ri) ++ "\"; }"
            }

    return $ render Z
        { zDerivationName = exeName
        , zComponentName  = C.unPackageName packageName ++ ":exe:" ++ exeName
        , zExecutableName = exeName
        , zCdeps          = ["zlib", "zlib.dev"]
        , zHsdeps         = zhsdeps
        }
  where
    encodeHash = encodeBase32 . I.getSHA256

-------------------------------------------------------------------------------
-- Exceptions
-------------------------------------------------------------------------------

data MetadataException
    = UnknownPackageName PackageName
    | UnknownPackageVersion PackageName Version
  deriving Show

instance Exception MetadataException

-------------------------------------------------------------------------------
-- package granularity
-------------------------------------------------------------------------------

packageGranularity
    :: Map P.UnitId P.Unit
    -> Map P.PkgId (Set P.UnitId)
    -> Peu r (NonEmpty P.PkgId)
packageGranularity units pkgs = do
    let am' :: Map P.PkgId (Set P.PkgId)
        am' = planJsonPkgGraph units pkgs

        nonLocal' :: P.PkgId -> Bool
        nonLocal' pid
            | Just uids <- M.lookup pid pkgs
            = all (\uid -> fmap P.uType (M.lookup uid units) == Just P.UnitTypeGlobal) uids

            | otherwise
            = False

        nonLocal :: P.PkgId -> Set P.PkgId -> Maybe (Set P.PkgId)
        nonLocal pid vs
            | nonLocal' pid
            = Just (S.filter nonLocal' vs)

            | otherwise
            = Nothing

        am :: Map P.PkgId (Set P.PkgId)
        am = M.mapMaybeWithKey nonLocal am'

    either (throwM . PackageLoop . fmap toCabal) id $ TG.runG am $ \g ->
        case TG.gVertices g of
            []   -> throwM EmptyGraph
            x:xs -> return (TG.gFromVertex g x :| map (TG.gFromVertex g) xs)

data PlanConstructionException
    = PackageLoop [PackageIdentifier]
    | EmptyGraph
  deriving Show

instance Exception PlanConstructionException

-------------------------------------------------------------------------------
-- Graph stuff
-------------------------------------------------------------------------------

planJsonPkgGraph
    :: Map P.UnitId P.Unit
    -> Map P.PkgId (Set P.UnitId)
    -> Map P.PkgId (Set P.PkgId)
planJsonPkgGraph units = M.mapWithKey $ \pid uids ->
    -- remove package from own depss (e.g. exe depending on lib)
    S.delete pid $ S.fromList
        [ P.uPId depUnit
        | uid     <- S.toList uids
        , unit    <- maybeToList (M.lookup uid units)
        , ci      <- M.elems (P.uComps unit)
        , depUid  <- S.toList (P.ciLibDeps ci)
        , depUnit <- maybeToList (M.lookup depUid units)
        ]

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

strictLookup :: (Exception e, Ord k) => k -> (k -> e) -> Map k v -> Peu r v
strictLookup k mkExc = maybe (throwM (mkExc k)) return . M.lookup k
