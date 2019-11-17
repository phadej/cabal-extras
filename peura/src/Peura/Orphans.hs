{-# OPTIONS_GHC -Wno-orphans #-}
module Peura.Orphans () where

{-
import System.Path (Path, Unrooted, fromUnrootedFilePath)
import Distribution.Parsec (eitherParsec)

import qualified Codec.Serialise as S

import Peura.Exports

instance r ~ Unrooted => IsString (Path r) where
    fromString = fromUnrootedFilePath

instance S.Serialise PackageName where
    encode = S.encode . prettyShow
    decode = S.decode >>= either fail return . eitherParsec

instance S.Serialise Version where
    encode = S.encode . prettyShow
    decode = S.decode >>= either fail return . eitherParsec

instance S.Serialise VersionRange where
    encode = S.encode . prettyShow
    decode = S.decode >>= either fail return . eitherParsec
-}
