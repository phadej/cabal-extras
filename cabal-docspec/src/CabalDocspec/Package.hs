module CabalDocspec.Package (
    Package (..),
    readLocalCabalFiles,
    readDirectCabalFiles,
) where

import Peura

import qualified Cabal.Plan                                   as Plan
import qualified Data.Map.Strict                              as Map
import qualified Distribution.PackageDescription.Parsec       as C
import qualified Distribution.Types.GenericPackageDescription as C

data Package = Package
    { pkgGpd   :: C.GenericPackageDescription
    , pkgDir   :: Path Absolute
    , pkgUnits :: [Plan.Unit]
    }
  deriving Show

readLocalCabalFiles
    :: TracerPeu r w
    -> Plan.PlanJson
    -> Peu r [Package]
readLocalCabalFiles tracer plan =
    for (itoList units0) $ \(path, units) -> do
        path' <- makeAbsoluteFilePath path
        cabalPath <- globDir1First "*.cabal" path'
        cabalBS <- readByteString cabalPath
        gpd <- maybe (die tracer $ "cannot parse " ++ toFilePath cabalPath) return
            $ C.parseGenericPackageDescriptionMaybe cabalBS

        return Package
            { pkgGpd   = gpd
            , pkgDir   = path'
            , pkgUnits = toList units
            }
  where
    units0 :: Map FilePath (NonEmpty Plan.Unit)
    units0 = group
        [ (path, unit)
        | unit <- toList (Plan.pjUnits plan)
        , Plan.uType unit == Plan.UnitTypeLocal
        , Just (Plan.LocalUnpackedPackage path) <- return (Plan.uPkgSrc unit)
        ]

readDirectCabalFiles
    :: TracerPeu r w
    -> [FilePath]
    -> Peu r [Package]
readDirectCabalFiles tracer paths = for paths $ \path -> do
    cabalPath <- makeAbsoluteFilePath path
    cabalBS <- readByteString cabalPath
    gpd <- maybe (die tracer $ "cannot parse " ++ toFilePath cabalPath) return
        $ C.parseGenericPackageDescriptionMaybe cabalBS
    
    return Package
        { pkgGpd   = gpd
        , pkgDir  = takeDirectory cabalPath
        , pkgUnits = []
        }

group :: (Ord a) => [(a,b)] -> Map a (NonEmpty b)
group = Map.fromListWith (<>) . map (fmap pure)
