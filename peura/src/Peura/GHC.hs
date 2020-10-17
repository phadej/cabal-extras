module Peura.GHC (
    -- * GHC info
    GhcInfo (..),
    getGhcInfo,
    -- * ghc-pkg
    findGhcPkg,
    -- * Package databases
    PackageDb,
    readPackageDb,
    ) where

import Data.Char           (isSpace)
import Data.List           (isSuffixOf, lookup)
import Distribution.Parsec (eitherParsec)
import Text.Read           (readMaybe)

import qualified Cabal.Parse                                          as Cbl
import qualified Data.ByteString.Lazy                                 as LBS
import qualified Data.Map.Strict                                      as Map
import qualified Distribution.CabalSpecVersion                        as C
import qualified Distribution.FieldGrammar                            as C
import qualified Distribution.Types.InstalledPackageInfo              as C
import qualified Distribution.Types.InstalledPackageInfo.FieldGrammar as C

import Peura.ByteString
import Peura.Exports
import Peura.Monad
import Peura.Paths
import Peura.Process
import Peura.Tracer
import Peura.Trace

data GhcInfo = GhcInfo
    { ghcPath     :: FilePath
    , ghcVersion  :: Version
    , ghcEnvDir   :: Path Absolute
    , ghcGlobalDb :: Path Absolute
    , ghcLibDir   :: Path Absolute
    }
  deriving Show

getGhcInfo :: Tracer (Peu r) (Trace w) -> FilePath -> Peu r GhcInfo
getGhcInfo tracer ghc = do
    ghcDir   <- getAppUserDataDirectory "ghc"

    infoBS <- LBS.toStrict <$> runProcessCheck tracer ghcDir ghc ["--info"]
    info <- maybe (die tracer "Cannot parse compilers --info output") return $
        readMaybe (fromUTF8BS infoBS)

    case lookup ("Project name" :: String) info of
        Just "The Glorious Glasgow Haskell Compilation System" -> do
            versionStr <- maybe (die tracer "cannot find 'Project version' in ghc --info") return $
                lookup "Project version" info
            ver <- case eitherParsec versionStr of
                Right ver -> return (ver :: Version)
                Left err  -> die tracer $ "Project version cannot be parsed\n" ++ err

            targetStr <- maybe (die tracer "cannot find 'Target platform' in ghc --info") return $
                lookup "Target platform" info
            (x,y) <- case splitOn '-' targetStr of
                x :| [_, y] -> return (x, y)
                _           -> die tracer "Target platform is not a triple"

            globalDbStr <- maybe (die tracer "Cannot find 'Global Package DB' in ghc --info") return $
                lookup "Global Package DB" info
            globalDb <- makeAbsoluteFilePath globalDbStr

            libDirStr <- maybe (die tracer "Cannot find 'LibDir' in ghc --info") return $
                lookup "LibDir" info
            libDir <- makeAbsoluteFilePath libDirStr

            return GhcInfo
                { ghcPath     = ghc
                , ghcVersion  = ver
                , ghcEnvDir   = ghcDir </> fromUnrootedFilePath (x ++ "-" ++ y ++ "-" ++ prettyShow ver) </> fromUnrootedFilePath "environments"
                , ghcGlobalDb = globalDb
                , ghcLibDir   = libDir
                }

        _ -> die tracer "Your compiler is not GHC"

findGhcPkg :: Tracer (Peu r) (Trace w) -> GhcInfo -> Peu r FilePath
findGhcPkg tracer ghcInfo = do
    let guess = toFilePath $ ghcLibDir ghcInfo </> fromUnrootedFilePath "bin/ghc-pkg"

    ghcDir   <- getAppUserDataDirectory "ghc"
    verBS <- LBS.toStrict <$> runProcessCheck tracer ghcDir guess ["--version"]

    let expected = "GHC package manager version " ++ prettyShow (ghcVersion ghcInfo)
        actual   = trim $ fromUTF8BS verBS
    if actual == expected
    then return guess
    else do
        putError tracer $ guess ++ " --version returned " ++ actual ++ "; expecting " ++ expected
        exitFailure

{-

-- This is old variant, which uses heuristics
findGhcPkg :: GhcInfo -> Peu r FilePath
findGhcPkg ghcInfo = do
    let ghc = ghcPath ghcInfo
        dir = FP.takeDirectory ghc
        file = FP.takeFileName ghc

    guess' <- case () of
        _ | file == "ghc"                       -> return "ghc-pkg"
          | Just sfx <- stripPrefix "ghc-" file -> return $ "ghc-pkg-" ++ sfx
          | otherwise                           -> do
              putError $ "Cannot guess ghc-pkg location from " ++ file
              exitFailure

    let guess | dir == "." = guess'
              | otherwise  = dir FP.</> guess'

    ghcDir   <- getAppUserDataDirectory "ghc"
    verBS <- LBS.toStrict <$> runProcessCheck ghcDir guess ["--version"]

    let expected = "GHC package manager version " ++ prettyShow (ghcVersion ghcInfo)
        actual   = trim $ fromUTF8BS verBS
    if actual == expected
    then return guess
    else do
        putError $ guess ++ " --version returned " ++ actual ++ "; expecting " ++ expected
        exitFailure
-}

-- | One of missing functions for lists in Prelude.
--
-- >>> splitOn '-' "x86_64-unknown-linux"
-- "x86_64" :| ["unknown","linux"]
--
-- >>> splitOn 'x' "x86_64-unknown-linux"
-- "" :| ["86_64-unknown-linu",""]
--
splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep = go where
    go [] = [] :| []
    go (x:xs)
        | x == sep  = [] :| ys : yss
        | otherwise = (x : ys) :| yss
      where
        (ys :| yss) = go xs

trim :: String -> String
trim = let tr = dropWhile isSpace . reverse in tr . tr

-------------------------------------------------------------------------------
-- PackageDb
-------------------------------------------------------------------------------

type PackageDb = Map UnitId C.InstalledPackageInfo

readPackageDb :: Path Absolute -> Peu r PackageDb
readPackageDb db = do
    files <- listDirectory db
    fmap (Map.fromList . concat) $ for files $ \p' -> do
        let p = db </> p'
        if ".conf" `isSuffixOf` toFilePath p
        then do
            contents <- readByteString p
            ipi <- either throwM return $ Cbl.parseWith parseIpi (toFilePath p) contents
            return [(C.installedUnitId ipi, ipi)]
        else return []
  where
    parseIpi fields = case C.partitionFields fields of
        (fields', _) -> C.parseFieldGrammar C.cabalSpecLatest fields' C.ipiFieldGrammar
