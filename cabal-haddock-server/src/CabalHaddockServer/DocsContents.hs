module CabalHaddockServer.DocsContents (
    DocsContents (..),
    readDocsContents,
) where

import Peura

import qualified Data.Set as Set

import CabalHaddockServer.Hoogle
import CabalHaddockServer.Warning

data DocsContents = DocsContents
    { docsContentsApi   :: API
    , docsContentsRoot  :: Path Absolute
    , docsContentsFiles :: Set (Path Unrooted)
    }

readDocsContents :: TracerPeu r (V1 W) -> Path Absolute -> Peu r (Maybe DocsContents)
readDocsContents tracer unpacked = do
    unpackedFiles <- recursiveListDirectoryFiles unpacked
    mhoogleFile   <- findHoogleFile tracer unpacked unpackedFiles

    case mhoogleFile of
        Nothing -> return Nothing
        Just hoogleFile -> do
            putDebug tracer $ "Found hoogle file " ++ toFilePath hoogleFile
            bs <- readByteString hoogleFile
            case parseHoogleFile bs of
                Left err -> do
                    putWarning tracer WHoogle err
                    return Nothing

                Right api -> do
                    putInfo tracer $
                        toFilePath unpacked ++ " contains " ++ prettyShow (apiPackageId api)

                    return $ Just DocsContents
                        { docsContentsApi   = api
                        , docsContentsRoot  = unpacked
                        , docsContentsFiles = Set.fromList unpackedFiles
                        }

findHoogleFile :: TracerPeu r (V1 W) -> Path Absolute -> [Path Unrooted] -> Peu r (Maybe (Path Absolute))
findHoogleFile tracer r fs = case mapMaybe check fs of
    [f] -> Just <$> canonicalizePath (r </> f)

    []  -> do
        putWarning tracer WMissingHoogleFile $
            "In " ++ toFilePath r
        return Nothing

    fs' -> do
        putWarning tracer WMultipleHoogleFiles $
            "In " ++ toFilePath r ++ " : " ++ unwords (map toUnrootedFilePath fs')
        return Nothing
  where
    check f | takeExtension f == Just txt = Just f
            | otherwise                   = Nothing
    txt = FileExt "txt"
