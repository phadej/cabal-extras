module CabalBundler.Main (main) where

import Peura
import Prelude ()

import Control.Applicative ((<**>))
import Data.Version        (showVersion)
import Distribution.Parsec (eitherParsec)

import qualified Cabal.Index          as I
import qualified Data.Map.Strict      as M
import qualified Data.Set             as S
import qualified Distribution.Package as C
import qualified Distribution.Version as C
import qualified Options.Applicative  as O

import Paths_cabal_bundler (version)

import CabalBundler.NixSingle

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = runPeu () $ do
    opts <- liftIO $ O.execParser optsP'

    meta <- liftIO I.cachedHackageMetadata

    -- TODO: check that package is in metadata

    -- Solve

    let pid@(PackageIdentifier pn ver) = optPackageId opts
    let exeName = C.unPackageName pn

    mplan <- ephemeralPlanJson $ emptyPlanInput
        { piExecutables = M.singleton pn (C.thisVersion ver, S.singleton exeName)
        , piCompiler = Just (optCompiler opts)
        }

    plan <- case mplan of
        Nothing -> do
            putError $ "Cannot find an install plan for " ++ prettyShow pid
            exitFailure
        Just plan -> return plan

    -- Generate derivation

    rendered <- generateDerivationNix pn exeName plan meta

    case optOutput opts of
        Nothing -> output rendered
        Just fp -> do
            fp' <- makeAbsolute fp
            writeByteString fp' (toUTF8BS rendered)

  where
    optsP' = O.info (optsP <**> O.helper <**> versionP) $ mconcat
        [ O.fullDesc
        , O.progDesc "Create bundles using cabal-install solver"
        , O.header "cabal-bundler - for offline development"
        ]

    versionP = O.infoOption (showVersion version)
        $ O.long "version" <> O.help "Show version"

-------------------------------------------------------------------------------
-- Opts
-------------------------------------------------------------------------------

data Opts = Opts
    { optCompiler  :: FilePath
    , optOutput    :: Maybe FsPath
    , optPackageId :: PackageIdentifier
    }

optsP :: O.Parser Opts
optsP = Opts
    <$> O.strOption (O.short 'w' <> O.long "with-compiler" <> O.value "ghc" <> O.showDefault <> O.help "Specify compiler to use")
    <*> optional (O.option (O.eitherReader $ return . fromFilePath) (O.short 'o' <> O.long "output" <> O.metavar "PATH" <> O.help "Output location"))
    <*> O.argument (O.eitherReader eitherParsec) (O.metavar "PKG" <> O.help "package to install")

{-

-------------------------------------------------------------------------------
-- Rest
-------------------------------------------------------------------------------

curlScript :: [DownloadFile] -> String
curlScript files = unlines
    [ "curl --output " ++ fn ++ " '" ++ url ++ "'"
    | DownloadFile fn url _ <- sortOn dwFileName files
    ]

sha256sumsFile :: [DownloadFile] -> String
sha256sumsFile files = unlines
    [ C.prettyShow shasum ++ "  " ++ fn
    | DownloadFile fn _ shasum <- sortOn dwFileName files
    ]

hackagePackageBaseUrl :: String
hackagePackageBaseUrl = "http://hackage.haskell.org/package"

hackagePackageUrl :: C.PackageIdentifier -> String
hackagePackageUrl pid = hackagePackageBaseUrl
    FP.</> C.prettyShow pid
    FP.</> C.prettyShow pid ++ ".tar.gz"

hackageRevisionUrl :: C.PackageIdentifier -> Word -> String
hackageRevisionUrl pid rev = hackagePackageBaseUrl
    FP.</> C.prettyShow pid
    FP.</> "revision"
    FP.</> show rev ++ ".cabal"

data DownloadFile = DownloadFile
    { dwFileName :: FilePath
    , dwUrl      :: String
    , dwSha256   :: I.SHA256
    }
  deriving Show

-}
