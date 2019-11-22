module CabalEnv.GHC where

import Peura

import Data.List           (lookup)
import Data.List.Split     (splitOn)
import Distribution.Parsec (eitherParsec)
import Text.Read           (readMaybe)

import qualified Data.ByteString.Lazy as LBS

import CabalEnv.Warning

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
            versionStr <- maybe (die "cannot find Project version in info") return $
                lookup "Project version" info
            ver <- case eitherParsec versionStr of
                Right ver -> return (ver :: Version)
                Left err  -> die $ "Project version cannot be parsed\n" ++ err

            targetStr <- maybe (die "cannot find Target platform in info") return $
                lookup "Target platform" info
            (x,y) <- case splitOn "-" targetStr of
                [x, _, y] -> return (x, y)
                _         -> die "Target platform is not triple"

            return GhcInfo
                { ghcVersion = ver
                , ghcEnvDir  = ghcDir </> fromUnrootedFilePath (x ++ "-" ++ y ++ "-" ++ prettyShow ver) </> fromUnrootedFilePath "environments"
                }

        _ -> die "Your compiler is not GHC"
