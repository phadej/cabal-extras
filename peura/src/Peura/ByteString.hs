module Peura.ByteString (
    readByteString,
    writeByteString,
    readLazyByteString,
    fromUTF8BS,
    toUTF8BS,
    ) where

import Distribution.Simple.Utils (fromUTF8BS, toUTF8BS)

import Peura.Exports
import Peura.Monad
import Peura.Paths

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS

readByteString :: Path Absolute -> Peu r ByteString
readByteString = liftIO . BS.readFile . toFilePath

writeByteString :: Path Absolute -> ByteString -> Peu r ()
writeByteString p bs = liftIO (BS.writeFile (toFilePath p) bs)

readLazyByteString :: Path Absolute -> Peu r LazyByteString
readLazyByteString = liftIO . LBS.readFile . toFilePath
