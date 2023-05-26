module CabalBundler.OpenBSD (
    generateOpenBSD,
)  where

import Peura

import Data.List (intercalate, partition)

import qualified Cabal.Index                            as I
import qualified Cabal.Plan                             as P
import qualified Data.Map.Strict                        as M
import qualified Data.Set                               as S
import qualified Data.Text                              as T
import qualified Distribution.Types.PackageName         as C
import qualified Distribution.Types.UnqualComponentName as C
import qualified Distribution.Types.Version             as C
import qualified Topograph                              as TG

import CabalBundler.ExeOption

generateOpenBSD
    :: TracerPeu r w
    -> PackageName
    -> ExeOption C.UnqualComponentName
    -> P.PlanJson
    -> Map PackageName I.PackageInfo
    -> Peu r String
generateOpenBSD tracer packageName exeName' plan meta = do
    exeName <- case exeName' of
        ExeOptionPkg x -> return $ C.unUnqualComponentName x
        ExeOption x    -> return $ C.unUnqualComponentName x
        ExeOptionAll   -> die tracer "--exe-all isn't supported for openbsd output"

    let units :: Map P.UnitId P.Unit
        units = P.pjUnits plan

    case findExe packageName exeName units of
        [(uid0, pkgId0)] -> do
            usedUnits <- bfs tracer units uid0
            deps <- unitsToDeps meta usedUnits
            case partition ((pkgId0 == ) . depPkgId) deps of
                (mainPackage : _, depUnits) -> do
                    return $ unlines $ makefileLines mainPackage depUnits
                ([], _) -> do
                    die tracer $ "Expected to find main package " <>
                        show (pkgId0, depPkgId <$> deps)
        uids ->
              throwM $ UnknownExecutable exeName (fst <$> uids)

unitsToDeps :: Map PackageName I.PackageInfo -> [P.Unit] -> Peu r [Dep]
unitsToDeps meta units = fmap concat $ for units $ \unit -> do
    let P.PkgId (P.PkgName tpkgname) (P.Ver verdigits) = P.uPId unit

    let cpkgname :: C.PackageName
        cpkgname = C.mkPackageName (T.unpack tpkgname)

    let cversion :: C.Version
        cversion = C.mkVersion verdigits

    rev <- case P.uType unit of
        P.UnitTypeBuiltin -> pure Nothing
        P.UnitTypeLocal   -> pure $ Just  0  -- Revision unavailable for local packages
        t -> do
            case P.uSha256 unit of
                Just _  -> do
                    pkgInfo <- maybe (throwM $ UnknownPackageName cpkgname) return $
                        M.lookup cpkgname meta
                    relInfo <- maybe (throwM $ UnknownPackageVersion cpkgname cversion) return $
                        M.lookup cversion $ I.piVersions pkgInfo

                    pure $ Just $ fromIntegral (I.riRevision relInfo)

                Nothing -> throwM $ UnknownUnitType cpkgname t

    let depForRev r = [Dep {
          depPackageName = cpkgname
        , depVersion     = cversion
        , depRevision    = r
        , depPkgId       = P.uPId unit
        }]
    pure $ maybe [] depForRev rev

data MetadataException
    = UnknownPackageName C.PackageName
    | UnknownExecutable String [P.UnitId]
    | UnknownPackageVersion C.PackageName C.Version
    | UnknownUnitType C.PackageName P.UnitType
  deriving Show

instance Exception MetadataException

bfs :: TracerPeu r w -> Map P.UnitId P.Unit -> P.UnitId -> Peu r [P.Unit]
bfs tracer units unit0 = fmap concat $ do
    uids <- either (throwM . PackageLoop) id $ TG.runG am $ \g -> do
        v <- maybe (throwM $ MissingUnit unit0) return $
            TG.gToVertex g unit0

        return $ map (TG.gFromVertex g) $
            -- nub and sort
            reverse $ S.toList $ S.fromList $ concat $ TG.dfs g v

    for uids $ \uid -> do
        unit <- lookupUnit units uid
        exes <- case M.toList (P.uComps unit) of
            [(_, compinfo)] ->
                collectExeDeps units (P.ciExeDeps compinfo)
            _ -> do
                putDebug tracer $ "Unit with multiple components " ++ show uid
                pure []
        pure $ [unit] <> exes

  where
    am :: M.Map P.UnitId (S.Set P.UnitId)
    am = fmap (foldMap P.ciLibDeps . P.uComps) units

data PlanConstructionException
    = PackageLoop [P.UnitId]
    | MissingUnit P.UnitId
  deriving Show

instance Exception PlanConstructionException

collectExeDeps :: M.Map P.UnitId P.Unit -> S.Set P.UnitId -> Peu r [P.Unit]
collectExeDeps units = traverse check . S.toList where
    check uid = lookupUnit units uid

lookupUnit :: M.Map P.UnitId P.Unit -> P.UnitId -> Peu r P.Unit
lookupUnit units uid =
    maybe (throwM $ MissingUnit uid) return $ M.lookup uid units

makefileLines :: Dep -> [Dep] -> [String]
makefileLines mainPackage deps =
    let cleanedDeps = ordNubOn depPackageName $ sortOn depPackageName deps
    in  [ "MODCABAL_STEM\t\t= " <> prettyShow (depPackageName mainPackage)
        , "MODCABAL_VERSION\t= " <>  prettyShow (depVersion mainPackage)
        ] <>
        [ "MODCABAL_REVISION\t= " <> show rev
        | let rev = depRevision mainPackage
        , rev > 0]
        <>
        [ "MODCABAL_MANIFEST\t= \\" | not $ null cleanedDeps ] <>
        map manifestLine cleanedDeps

manifestLine :: Dep -> String
manifestLine dep =
    let name = prettyShow $ depPackageName dep
        ver = prettyShow $ depVersion dep
        rev = show $ depRevision dep
     in intercalate "\t" ["", name, ver, rev, "\\"]

findExe :: PackageName ->  String -> Map P.UnitId P.Unit -> [(P.UnitId, P.PkgId)]
findExe pn exeName units =
    [ (uid, P.uPId unit)
    | (uid, unit) <- M.toList units
    , pkgName (toCabal (P.uPId unit)) == pn
    , (P.CompNameExe e, _) <- M.toList (P.uComps unit)
    , e == T.pack exeName
    ]

data Dep = Dep
    { depPackageName :: PackageName
    , depVersion     :: Version
    , depRevision    :: Word
    , depPkgId       :: P.PkgId
    }
  deriving (Show)
