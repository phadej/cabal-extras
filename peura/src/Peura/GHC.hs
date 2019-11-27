module Peura.GHC (
    GhcInfo (..),
    getGhcInfo,
    ) where

import Data.List           (lookup)
import Distribution.Parsec (eitherParsec)
import Text.Read           (readMaybe)

import qualified Data.ByteString.Lazy as LBS

import Peura.Exports
import Peura.Monad
import Peura.Paths
import Peura.Process

data GhcInfo = GhcInfo
    { ghcVersion :: Version
    , ghcEnvDir  :: Path Absolute
    }

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

            return GhcInfo
                { ghcVersion = ver
                , ghcEnvDir  = ghcDir </> fromUnrootedFilePath (x ++ "-" ++ y ++ "-" ++ prettyShow ver) </> fromUnrootedFilePath "environments"
                }

        _ -> die "Your compiler is not GHC"

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

die :: String -> Peu r a
die msg = putError msg *> exitFailure
