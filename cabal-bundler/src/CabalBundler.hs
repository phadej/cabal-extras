{-# LANGUAGE DeriveGeneric          #-}
module CabalBundler where

import Peura   (CabalPlanConvert (..))
import Prelude hiding (pi)

import Control.Exception  (Exception, throwIO)
import Control.Monad      (forM, forM_)
import Data.Foldable      (toList)
import Data.List          (sort, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map           (Map)
import Data.Maybe         (mapMaybe, maybeToList)
import Data.Set           (Set)
import Data.Word          (Word)
import GHC.Generics       (Generic)

import qualified Cabal.Index                    as I
import qualified Cabal.Plan                     as P
import qualified Data.ByteString                as BS
import qualified Data.List.NonEmpty             as NE
import qualified Data.Map.Strict                as M
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Distribution.Package           as C
import qualified Distribution.Pretty            as C
import qualified Distribution.Types.PackageName as C
import qualified Distribution.Version           as C
import qualified System.FilePath                as FP
import qualified Topograph                      as TG
import qualified Zinza

import CabalBundler.NixBase32

test :: IO ()
test = do
    plan <- P.findAndDecodePlanJson (P.ExactPath "cabal-bundler/fixtures/cabal-fmt.plan.json")

    let units :: Map P.UnitId P.Unit
        units = P.pjUnits plan

    let pkgs :: Map P.PkgId (Set P.UnitId)
        pkgs = M.fromListWith S.union
            [ (P.uPId unit, S.singleton (P.uId unit))
            | unit <- M.elems units
            ]

    pkgIds <- packageGranularity units pkgs

    tmpl <- Zinza.parseAndCompileTemplateIO "cabal-bundler/data/single.nix"

    meta <- I.cachedHackageMetadata
    zhsdeps <- forM (NE.sort pkgIds) $ \pkgId' -> do
        let pkgId@(C.PackageIdentifier pn ver) = toCabal pkgId'
        pi <- strictLookup pn UnknownPackageName meta
        ri <- strictLookup ver (UnknownPackageVersion pn) (I.piVersions pi)
        return ZDep
            { zdepName = C.prettyShow pn
            , zdepVersion = C.prettyShow ver
            , zdepSha256 = encodeBase32 $ I.getSHA256 $ I.riTarball ri
            }
        -- let tarball = DownloadFile (C.prettyShow pkgId ++ ".tar.gz") (hackagePackageUrl pkgId) (I.riTarball ri)

{-
        case I.riRevision ri of
            0 -> return [tarball]
            r -> return
                [ tarball
                , DownloadFile (C.prettyShow pkgId ++ ".cabal") (hackageRevisionUrl pkgId (I.riRevision ri)) (I.riCabal ri)
                ]
-}


    rendered <- tmpl Z
        { zDerivationName = "cabal-fmt"
        , zComponentName  = "cabal-fmt:exe:cabal-fmt"
        , zExecutableName = "cabal-fmt"
        , zCdeps          = ["zlib", "zlib.dev"]
        , zHsdeps         = zhsdeps
        }

    putStrLn rendered
    writeFile "cabal-bundler/fixtures/derivation.nix" rendered

{-

    files <- fmap concat $ forM (toList pkgIds) $ \pkgId' -> do
        let pkgId@(C.PackageIdentifier pn ver) = toCabal pkgId'
        pi <- strictLookup pn UnknownPackageName meta
        ri <- strictLookup ver (UnknownPackageVersion pn) (I.piVersions pi)
        let tarball = DownloadFile (C.prettyShow pkgId ++ ".tar.gz") (hackagePackageUrl pkgId) (I.riTarball ri)
        case I.riRevision ri of
            0 -> return [tarball]
            r -> return
                [ tarball
                , DownloadFile (C.prettyShow pkgId ++ ".cabal") (hackageRevisionUrl pkgId (I.riRevision ri)) (I.riCabal ri)
                ]

    putStrLn $ curlScript files

    putStrLn $ sha256sumsFile files

    forM_ (NE.reverse pkgIds) $ \pkgId -> do
        print pkgId
-}

data Z = Z
    { zDerivationName :: String
    , zComponentName  :: String
    , zExecutableName :: String
    , zCdeps          :: [String]
    , zHsdeps         :: NonEmpty ZDep
    }
  deriving (Show, Generic)

instance Zinza.Zinza Z where
    toType  = Zinza.genericToTypeSFP
    toValue = Zinza.genericToValueSFP

data ZDep = ZDep
    { zdepName    :: String
    , zdepVersion :: String
    , zdepSha256  :: String
    }
  deriving (Show, Generic)

instance Zinza.Zinza ZDep where
    toType  = Zinza.genericToTypeSFP
    toValue = Zinza.genericToValueSFP

curlScript :: [DownloadFile] -> String
curlScript files = unlines
    [ "curl --output " ++ fn ++ " '" ++ url ++ "'"
    | DownloadFile fn url _ <- sortOn dwFileName files
    ]

sha256sumsFile :: [DownloadFile] -> String
sha256sumsFile files = unlines
    [ C.prettyShow shasum ++ "  " ++ fn
    | DownloadFile fn _ shasum <- sortOn dwFileName files
    ]

hackagePackageBaseUrl :: String
hackagePackageBaseUrl = "http://hackage.haskell.org/package"

hackagePackageUrl :: C.PackageIdentifier -> String
hackagePackageUrl pid = hackagePackageBaseUrl
    FP.</> C.prettyShow pid
    FP.</> C.prettyShow pid ++ ".tar.gz"

hackageRevisionUrl :: C.PackageIdentifier -> Word -> String
hackageRevisionUrl pid rev = hackagePackageBaseUrl
    FP.</> C.prettyShow pid
    FP.</> "revision"
    FP.</> show rev ++ ".cabal"

data DownloadFile = DownloadFile
    { dwFileName :: FilePath
    , dwUrl      :: String
    , dwSha256   :: I.SHA256
    }
  deriving Show

data MetadataException
    = UnknownPackageName C.PackageName
    | UnknownPackageVersion C.PackageName C.Version
  deriving Show

instance Exception MetadataException

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

strictLookup :: (Exception e, Ord k) => k -> (k -> e) -> Map k v -> IO v
strictLookup k mkExc = maybe (throwIO (mkExc k)) return . M.lookup k

-------------------------------------------------------------------------------
-- package granularity
-------------------------------------------------------------------------------

packageGranularity
    :: Map P.UnitId P.Unit
    -> Map P.PkgId (Set P.UnitId)
    -> IO (NonEmpty P.PkgId)
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

    either (throwIO . PackageLoop . fmap toCabal) id $ TG.runG am $ \g ->
        case TG.gVertices g of
            []   -> throwIO EmptyGraph
            x:xs -> return (TG.gFromVertex g x :| map (TG.gFromVertex g) xs)

data PlanConstructionException
    = PackageLoop [C.PackageIdentifier]
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
