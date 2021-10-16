{-# LANGUAGE CPP #-}
module CabalHaddockServer.Options where

import Peura

import Control.Applicative ((<**>))

import qualified Options.Applicative as O

import CabalHaddockServer.Warning

parseOptions :: IO Opts
parseOptions = O.execParser $
    O.info (optsP <**> O.helper <**> versionP) $ mconcat
    [ O.fullDesc
    , O.progDesc "Browse local multi-package haddocks"
    , O.header "cabal-haddock-server - serves the docs tarball contents"
    ]
  where
    versionP = O.infoOption CURRENT_PACKAGE_VERSION
        $ O.long "version" <> O.help "Show version"

-------------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------------

data Opts = Opts
    { optTarballs :: [FsPath]
    , optTracer   :: TracerOptions W -> TracerOptions W
    }

optsP :: O.Parser Opts
optsP = pure Opts
    <*> many (O.argument fspathP $ mconcat [O.metavar "TARBALL", O.help "docs tarball"])
    <*> tracerOptionsParser

fspathP :: O.ReadM FsPath
fspathP = O.eitherReader $ return . fromFilePath
