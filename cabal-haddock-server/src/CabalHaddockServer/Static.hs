{-# LANGUAGE TemplateHaskell #-}
module CabalHaddockServer.Static (
    staticFiles,
) where

import Peura

import qualified Data.Map.Strict as Map
import qualified FileEmbedLzma

staticFiles :: Map FilePath ByteString
staticFiles = Map.fromList $$(FileEmbedLzma.embedDir "static")
