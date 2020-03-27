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
import Data.List           (isSuffixOf, lookup, stripPrefix)
import Distribution.Parsec (eitherParsec)
import Text.Read           (readMaybe)

import qualified Cabal.Parse                                          as Cbl
import qualified Data.ByteString.Lazy                                 as LBS
import qualified Data.Map.Strict                                      as Map
import qualified Distribution.CabalSpecVersion                        as C
import qualified Distribution.FieldGrammar                            as C
import qualified Distribution.Types.InstalledPackageInfo              as C
import qualified Distribution.Types.InstalledPackageInfo.FieldGrammar as C
import qualified System.FilePath                                      as FP

import Peura.ByteString
import Peura.Exports
import Peura.Monad
import Peura.Paths
import Peura.Process

data GhcInfo = GhcInfo
    { ghcPath     :: FilePath
    , ghcVersion  :: Version
    , ghcEnvDir   :: Path Absolute
    , ghcGlobalDb :: Path Absolute
    }
  deriving Show

getGhcInfo :: FilePath -> Peu r GhcInfo
getGhcInfo ghc = do
    ghcDir   <- getAppUserDataDirectory "ghc"

    infoBS <- LBS.toStrict <$> runProcessCheck ghcDir ghc ["--info"]
    info <- maybe (die "Cannot parse compilers --info output") return $
        readMaybe (fromUTF8BS infoBS)

    case lookup ("Project name" :: String) info of
        Just "The Glorious Glasgow Haskell Compilation System" -> do
            versionStr <- maybe (die "cannot find Project version in ghc --info") return $
                lookup "Project version" info
            ver <- case eitherParsec versionStr of
                Right ver -> return (ver :: Version)
                Left err  -> die $ "Project version cannot be parsed\n" ++ err

            targetStr <- maybe (die "cannot find Target platform in ghc --info") return $
                lookup "Target platform" info
            (x,y) <- case splitOn '-' targetStr of
                x :| [_, y] -> return (x, y)
                _           -> die "Target platform is not a triple"

            globalDbStr <- maybe (die "Cannot find Global Package DB in ghc --info") return $
                lookup "Global Package DB" info
            globalDb <- makeAbsoluteFilePath globalDbStr

            return GhcInfo
                { ghcPath     = ghc
                , ghcVersion  = ver
                , ghcEnvDir   = ghcDir </> fromUnrootedFilePath (x ++ "-" ++ y ++ "-" ++ prettyShow ver) </> fromUnrootedFilePath "environments"
                , ghcGlobalDb = globalDb
                }

        _ -> die "Your compiler is not GHC"

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

die :: String -> Peu r a
die msg = putError msg *> exitFailure

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
