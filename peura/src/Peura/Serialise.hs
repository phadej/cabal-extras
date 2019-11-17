module Peura.Serialise (
    readFileDeserialise,
    writeFileSerialise,
    ) where

import Peura.Exports
import Peura.Monad
import Peura.Paths

import qualified Codec.Serialise as S

readFileDeserialise :: Serialise a => Path Absolute -> Peu r a
readFileDeserialise p = liftIO $ S.readFileDeserialise (toFilePath p)

writeFileSerialise :: Serialise a => Path Absolute -> a -> Peu r ()
writeFileSerialise p = liftIO . S.writeFileSerialise (toFilePath p)
