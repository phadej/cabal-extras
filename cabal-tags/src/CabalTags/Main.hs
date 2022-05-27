{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module CabalTags.Main (main) where

import Peura
import Prelude ()

import Control.Applicative ((<**>))

import qualified Cabal.Config            as Cbl
import qualified Cabal.Plan              as P
import qualified Data.Map.Strict         as M
import qualified Data.Set                as S
import qualified Data.Text               as T
import qualified Distribution.ModuleName as MN
import qualified Distribution.Package    as C
import qualified Distribution.Parsec     as C
import qualified Options.Applicative     as O

import qualified CabalTags.GHC.All as GHC

import CabalTags.Tag

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    -- options
    opts <- O.execParser optsP'
    tracer <- makeTracerPeu @(V1 W) (optTracer opts defaultTracerOptions)

    runPeu tracer () $ do
        putInfo tracer $ "This cabal-tags is compiled with GHC version " ++ VERSION_ghc

        -- ghc info
        ghcInfo <- getGhcInfo tracer $ optCompiler opts
        
        when (prettyShow (ghcVersion ghcInfo) /= VERSION_ghc) $
            die tracer $ prettyShow (ghcVersion ghcInfo) ++ " doesn't match"

        putInfo tracer $ show ghcInfo
        
        -- build directory
        cwd <- getCurrentDirectory
        let buildDir' = 
                fromUnrootedFilePath "dist-newstyle" </>
                fromUnrootedFilePath "build"  </>
                fromUnrootedFilePath (ghcPlatform ghcInfo) </>
                fromUnrootedFilePath ("ghc-" ++ VERSION_ghc)

        let buildDir = cwd </> buildDir'

        -- list .hie files
        files <- filter (\f -> takeExtension f == Just (FileExt "hie")) <$> recursiveListDirectoryFiles buildDir

        -- make nameCache
        nameCache <- liftIO createNameCache

        -- for each file
        for_ (take 10 files) $ \file -> do
            let abspath = toFilePath (buildDir </> file)

            putInfo tracer abspath

            (hieFileResult, _newCache) <- liftIO $ GHC.readHieFile nameCache abspath
            let hieFile = GHC.hie_file_result hieFileResult

            let tags = tagsFromHieFile hieFile

            for_ tags $ putInfo tracer . show

  where
    optsP' = O.info (optsP <**> O.helper <**> versionP) $ mconcat
        [ O.fullDesc
        , O.progDesc "Tags generator"
        , O.header "cabal-tags - all tags belong to us"
        ]

    versionP = O.infoOption VERSION_cabal_tags
        $ O.long "version" <> O.help "Show version"

-------------------------------------------------------------------------------
-- GHC extras
-------------------------------------------------------------------------------

createNameCache :: IO GHC.NameCache
createNameCache = do
    uniqSupply <- GHC.mkSplitUniqSupply 'z'
    pure $ GHC.initNameCache uniqSupply []

-------------------------------------------------------------------------------
-- Opts
-------------------------------------------------------------------------------

data Opts = Opts
    { optCompiler     :: FilePath
    , optTracer       :: TracerOptions W -> TracerOptions W
    }

optsP :: O.Parser Opts
optsP = pure Opts
    <*  O.flag' () (O.short 'c' <> O.long "ctags" <> O.help "Generate ctags")
    <*> O.strOption (O.short 'w' <> O.long "with-compiler" <> O.value "ghc" <> O.showDefault <> O.help "Specify compiler to use")
    <*> tracerOptionsParser

-------------------------------------------------------------------------------
-- Warnings
-------------------------------------------------------------------------------

data W
    = WOrphans
    | WModuleWithOrphans
    | WUnknownUnit
    | WMissingIfaceFile
  deriving (Eq, Ord, Enum, Bounded)

instance Universe W where universe = [minBound .. maxBound]
instance Finite W

instance Warning W where
    warningToFlag WOrphans             = "orphans"
    warningToFlag WModuleWithOrphans   = "module-with-orphans"
    warningToFlag WUnknownUnit         = "unknown-unit"
    warningToFlag WMissingIfaceFile    = "missing-iface-file"
