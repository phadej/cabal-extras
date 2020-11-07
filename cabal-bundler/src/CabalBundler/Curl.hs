module CabalBundler.Curl (
    generateCurl,
    )  where

import Peura

import Data.Maybe (maybeToList)

import qualified Cabal.Index                            as I
import qualified Cabal.Plan                             as P
import qualified Data.List.NonEmpty                     as NE
import qualified Data.Map.Strict                        as M
import qualified Data.Set                               as S
import qualified Distribution.Package                   as C
import qualified Distribution.Types.UnqualComponentName as C
import qualified System.FilePath                        as FP
import qualified Topograph                              as TG

import CabalBundler.ExeOption

-------------------------------------------------------------------------------
-- generating output
-------------------------------------------------------------------------------

-- TODO: use ExeOption
generateCurl
    :: TracerPeu r w
    -> PackageName
    -> ExeOption C.UnqualComponentName
    -> P.PlanJson
    -> Map PackageName I.PackageInfo
    -> Peu r String
generateCurl _tracer _packageName _exeName plan meta = do
    let units :: Map P.UnitId P.Unit
        units = P.pjUnits plan

    let pkgs :: Map P.PkgId (Set P.UnitId)
        pkgs = M.fromListWith S.union
            [ (P.uPId unit, S.singleton (P.uId unit))
            | unit <- M.elems units
            -- TODO: better check. this can fail.
            , case P.uPkgSrc unit of
                Just (P.RepoTarballPackage _) -> True
                _                             -> False
            ]

    pkgIds <- packageGranularity units pkgs

    files <- fmap concat $ for (toList $ NE.sort pkgIds) $ \pkgId' -> do
        let pkgId@(PackageIdentifier pn ver) = toCabal pkgId'
        pi <- strictLookup pn UnknownPackageName meta
        ri <- strictLookup ver (UnknownPackageVersion pn) (I.piVersions pi)
        let tarGz = DownloadFile (prettyShow pkgId ++ ".tar.gz") (hackagePackageUrl pkgId) (I.riTarball ri)
        return $ case I.riRevision ri of
            0 -> [ tarGz ]
            r -> do
                let cabal = DownloadFile (prettyShow pkgId ++ ".cabal") (hackageRevisionUrl pkgId r) (I.riCabal ri)
                [ tarGz, cabal ]

    return $ concat
        [ unlines
            [ "#!/bin/sh"
            , "# This file is generated with cabal-bundler"
            , ""
            , "set -ex"
            , ""
            , "cat <<EOF > SHA256SUMS"
            ]
        , sha256sumsFile files
        , unlines
            [ "EOF"
            , ""
            ]
        , curlScript files
        , unlines
            [ ""
            , "sha256sum -c SHA256SUMS"
            ]
        ]

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

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

curlScript :: [DownloadFile] -> String
curlScript files = unlines
    [ "curl --silent --location --output " ++ fn ++ " '" ++ url ++ "'"
    | DownloadFile fn url _ <- sortOn dwFileName files
    ]

sha256sumsFile :: [DownloadFile] -> String
sha256sumsFile files = unlines
    [ prettyShow shasum ++ "  " ++ fn
    | DownloadFile fn _ shasum <- sortOn dwFileName files
    ]

hackagePackageBaseUrl :: String
hackagePackageBaseUrl = "http://hackage.haskell.org/package"

hackagePackageUrl :: C.PackageIdentifier -> String
hackagePackageUrl pid = hackagePackageBaseUrl
    FP.</> prettyShow pid
    FP.</> prettyShow pid ++ ".tar.gz"

hackageRevisionUrl :: C.PackageIdentifier -> Word32 -> String
hackageRevisionUrl pid rev = hackagePackageBaseUrl
    FP.</> prettyShow pid
    FP.</> "revision"
    FP.</> show rev ++ ".cabal"

data DownloadFile = DownloadFile
    { dwFileName :: FilePath
    , dwUrl      :: String
    , dwSha256   :: I.SHA256
    }
  deriving Show
