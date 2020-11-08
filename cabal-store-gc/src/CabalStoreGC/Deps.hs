module CabalStoreGC.Deps (extractDeps) where

import Peura

import qualified Data.ByteString.Char8        as BS8
import qualified Distribution.Compat.Newtype  as C
import qualified Distribution.Parsec          as C
import qualified Distribution.Parsec.Newtypes as C
import qualified Distribution.Types.UnitId    as C

extractDeps :: ByteString -> [C.UnitId]
extractDeps contents =
    [ unitId
    | l <- BS8.lines contents
    , Just sfx' <- return (BS8.stripPrefix "deps:" l)
    , let sfx = BS8.dropWhile isSpace sfx'
    , unitId <- either fail (C.unpack' (C.alaList C.CommaFSep))
        $ C.eitherParsec $ fromUTF8BS sfx
    ]
