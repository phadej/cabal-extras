{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CabalHaddockServer.DocsContents (
    DocsFile (..),
    DocsContents (..),
    readDocsContentsDirectory,
    readDocsContentsTarball,
) where

import Peura

import qualified Codec.Archive.Tar      as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.Map.Strict        as Map
import qualified Data.Set               as Set
import qualified Hooglite

import CabalHaddockServer.Warning

-------------------------------------------------------------------------------
-- Output
-------------------------------------------------------------------------------

data DocsContents = DocsContents
    { docsContentsApi   :: Hooglite.API
    , docsContentsServe :: Path Unrooted -> DocsFile
    , docsContentsFiles :: Set (Path Unrooted)
    }

data DocsFile
    = DocsFileOnDisk (Path Absolute)
    | DocsFileInMemory LazyByteString

-------------------------------------------------------------------------------
-- Directory (unpacked)
-------------------------------------------------------------------------------

readDocsContentsDirectory :: TracerPeu r (V1 W) -> Path Absolute -> Peu r (Maybe DocsContents)
readDocsContentsDirectory tracer unpacked = do
    unpackedFiles <- recursiveListDirectoryFiles unpacked
    mhoogleFile   <- findHoogleFile tracer unpacked unpackedFiles

    case mhoogleFile of
        Nothing -> return Nothing
        Just hoogleFile' -> do
            hoogleFile <- canonicalizePath (unpacked </> hoogleFile')
            putDebug tracer $ "Found hoogle file " ++ toFilePath hoogleFile
            bs <- readByteString hoogleFile
            case Hooglite.parseHoogleFile (fromUTF8BS bs) of
                Left err -> do
                    putWarning tracer WHoogle err
                    return Nothing

                Right api -> do
                    putInfo tracer $
                        toFilePath unpacked ++ " contains " ++ prettyShow (Hooglite.apiPackage api)

                    return $ Just DocsContents
                        { docsContentsApi   = api
                        , docsContentsServe = \p -> DocsFileOnDisk (unpacked </> p)
                        , docsContentsFiles = Set.fromList unpackedFiles
                        }

-------------------------------------------------------------------------------
-- Tarball
-------------------------------------------------------------------------------

readDocsContentsTarball :: forall r. TracerPeu r (V1 W) -> Path Absolute -> Peu r (Maybe DocsContents)
readDocsContentsTarball tracer tarball = do
    lbs <- readLazyByteString tarball
    -- TODO: we should force entries1, catch exceptions and report them.
    let lbs' = GZip.decompress lbs
    let entries0 = Tar.read lbs'
    entries1 <- foldl'Entries (\acc e -> acc >>= onEntry e) (pure Map.empty) onError entries0
    entries <- processEntries entries1
    let files = Map.keysSet entries

    mhoogleFile   <- findHoogleFile tracer tarball (Set.toList files)
    case mhoogleFile of
        Nothing -> return Nothing
        Just hoogleFile -> do
            putDebug tracer $ "Found hoogle file " ++ toUnrootedFilePath hoogleFile
            bs <- maybe (die tracer "PANIC!") (return . fromUTF8LBS) $ Map.lookup hoogleFile entries
            case Hooglite.parseHoogleFile bs of
                Left err -> do
                    putWarning tracer WHoogle err
                    return Nothing

                Right api -> do
                    return $ Just DocsContents
                        { docsContentsApi   = api
                        , docsContentsServe = \p -> DocsFileInMemory (Map.findWithDefault LBS.empty p entries)
                        , docsContentsFiles = files
                        }
  where
    onError :: Tar.FormatError -> Peu r a
    onError err = die tracer (displayException err)

    foldl'Entries :: (a -> Tar.Entry -> a) -> a -> (e -> a) -> Tar.Entries e -> a
    foldl'Entries f z onErr = go z where
        go !acc (Tar.Next e es) = go (f acc e ) es
        go !_   (Tar.Fail err)  = onErr err
        go !acc Tar.Done        = acc

    onEntry :: Tar.Entry -> Map (Path Unrooted) LBS.ByteString -> Peu r (Map (Path Unrooted) LBS.ByteString)
    onEntry e !acc = case Tar.entryContent e of
        Tar.NormalFile lbs _ -> return (Map.insert (fromUnrootedFilePath (Tar.entryPath e)) lbs acc)
        _                    -> return acc

    -- TODO: we shouldn't die here, but warn and skip the tarball
    processEntries :: Map (Path Unrooted) LBS.ByteString -> Peu r (Map (Path Unrooted) LBS.ByteString)
    processEntries entries = case Map.lookupMin entries of
        Nothing     -> return entries
        Just (p, _) -> case splitFragments p of
            []      -> return entries
            [_]     -> die tracer "File in the root of tarball"
            pfx : _ -> Map.fromList <$> traverse (go pfx) (Map.toList entries)
      where
        go pfx (p, lbs) = case splitFragments p of
            [] -> pure (p, lbs)
            pfx' : rest
                | pfx == pfx' -> return (joinFragments rest, lbs)
                | otherwise   -> die tracer "File in different directory"

-------------------------------------------------------------------------------
-- Hoogle file
-------------------------------------------------------------------------------

findHoogleFile :: TracerPeu r (V1 W) -> Path Absolute -> [Path Unrooted] -> Peu r (Maybe (Path Unrooted))
findHoogleFile tracer r fs = case mapMaybe check fs of
    [f] -> return (Just f)

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
