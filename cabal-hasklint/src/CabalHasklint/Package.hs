module CabalHasklint.Package (
    Package (..),
    readLocalCabalFiles,
    readDirectCabalFiles,
) where

import Peura

import qualified Distribution.PackageDescription.Parsec as C

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
