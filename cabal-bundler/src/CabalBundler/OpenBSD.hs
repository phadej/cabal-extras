module CabalBundler.OpenBSD (
    generateOpenBSD,
)  where

import Peura

import Data.List (intercalate)

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
        [uid0] -> do
            usedUnits <- bfs tracer units uid0
            deps <- unitsToDeps meta usedUnits
            let cleanedDeps = ordNubOn depPackageName $ sortOn depPackageName deps
            return $ unlines $ map manifestLine cleanedDeps
        uids ->
              throwM $ UnknownExecutable exeName uids

unitsToDeps :: Map PackageName I.PackageInfo -> [P.Unit] -> Peu r [Dep]
unitsToDeps meta units = fmap concat $ for units $ \unit -> do
    let P.PkgId (P.PkgName tpkgname) (P.Ver verdigits) = P.uPId unit

    let cpkgname :: C.PackageName
        cpkgname = C.mkPackageName (T.unpack tpkgname)

    let cversion :: C.Version
        cversion = C.mkVersion verdigits

    case P.uType unit of
        P.UnitTypeBuiltin -> return []
        P.UnitTypeLocal -> return []
        _ -> do
            rev <- case P.uSha256 unit of
                Just _  -> do
                    pkgInfo <- maybe (throwM $ UnknownPackageName cpkgname) return $
                        M.lookup cpkgname meta
                    relInfo <- maybe (throwM $ UnknownPackageVersion cpkgname cversion) return $
                        M.lookup cversion $ I.piVersions pkgInfo

                    return $ fromIntegral (I.riRevision relInfo)

                Nothing -> case P.uType unit of
                    P.UnitTypeLocal   -> return 0
                    t                 -> throwM $ UnknownUnitType cpkgname t

            return $ pure $ Dep
                { depPackageName = cpkgname
                , depVersion     = cversion
                , depRevision    = rev
                }

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

manifestLine :: Dep -> String
manifestLine dep =
    let name = prettyShow $ depPackageName dep
        ver = prettyShow $ depVersion dep
        rev = show $ depRevision dep
     in intercalate "\t" ["", name, ver, rev, "\\"]

findExe :: PackageName ->  String -> Map P.UnitId P.Unit -> [P.UnitId]
findExe pn exeName units =
    [ uid
    | (uid, unit) <- M.toList units
    , pkgName (toCabal (P.uPId unit)) == pn
    , (P.CompNameExe e, _) <- M.toList (P.uComps unit)
    , e == T.pack exeName
    ]

data Dep = Dep
    { depPackageName :: PackageName
    , depVersion     :: Version
    , depRevision    :: Word
    }
  deriving (Show)
