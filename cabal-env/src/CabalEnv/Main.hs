{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Copyright: Oleg Grenrus
-- License: GPL-3.0-or-later
module CabalEnv.Main (main) where

import Peura

import Cabal.Plan
       (SearchPlanJson (InBuildDir), findAndDecodePlanJson)
import Control.Applicative           ((<**>))
import Data.Char                     (isAlphaNum)
import Data.List
       (intercalate, isPrefixOf, lines, lookup, sort)
import Data.List.Split               (splitOn)
import Data.Semigroup                (Semigroup (..))
import Data.Version                  (showVersion)
import Distribution.Parsec           (eitherParsec, explicitEitherParsec)
import Distribution.Types.Dependency (Dependency (..))
import Distribution.Version
       (anyVersion, intersectVersionRanges, nullVersion, thisVersion)
import System.FilePath.Glob          (glob)
import Text.Read                     (readMaybe)

import qualified Data.ByteString.Lazy            as LBS
import qualified Data.List.NonEmpty              as NE
import qualified Data.Map.Strict                 as Map
import qualified Distribution.Compat.CharParsing as P
import qualified Options.Applicative             as O

import CabalEnv.FakePackage

import Paths_cabal_env (version)

main :: IO ()
main = runPeu () $ do
    opts <- liftIO $ O.execParser optsP'
    ghcEnvDir <- getGhcEnvDir (optCompiler opts)

    case optAction opts of
        Nothing         -> installAction opts ghcEnvDir
        Just ActionShow -> showAction opts ghcEnvDir
        Just ActionList -> listAction opts ghcEnvDir
  where
    optsP' = O.info (optsP <**> O.helper <**> versionP) $ mconcat
        [ O.fullDesc
        , O.progDesc "Manage package-enviroments"
        , O.header "cabal-env - a better cabal-install install --lib"
        ]

    versionP = O.infoOption (showVersion version)
        $ O.long "version" <> O.help "Show version"

-------------------------------------------------------------------------------
-- List
-------------------------------------------------------------------------------

listAction :: Opts -> Path Absolute -> Peu r ()
listAction Opts {..} ghcEnvDir = do
    when optVerbose $ putInfo $ "Environments available in " ++ toFilePath ghcEnvDir
    fs <- listDirectory ghcEnvDir
    for_ (sort fs) $ \f ->
        putInfo $ toUnrootedFilePath f ++ if optVerbose then " " ++ toFilePath (ghcEnvDir </> f) else ""

-------------------------------------------------------------------------------
-- Show
-------------------------------------------------------------------------------

showAction :: Opts -> Path Absolute -> Peu r ()
showAction Opts {..} ghcEnvDir = do
    (_cmds, pkgIds) <- getEnvironmentContents $ ghcEnvDir </> fromUnrootedFilePath optEnvName
    when optVerbose $ putInfo $ "Packages in " ++ optEnvName ++ " environment"
    for_ (sort pkgIds) $ \pkgId ->
        output $ prettyShow pkgId

-------------------------------------------------------------------------------
-- Install
-------------------------------------------------------------------------------

installAction :: Opts -> Path Absolute -> Peu r ()
installAction Opts {..} ghcEnvDir = do
    unless (null optDeps) $ do
        when optVerbose $ putDebug $ "GHC environment directory: " ++ toFilePath ghcEnvDir

        createDirectoryIfMissing True ghcEnvDir
        (_cmds, pkgIds) <- getEnvironmentContents $ ghcEnvDir </> fromUnrootedFilePath optEnvName

        when optVerbose $ do
            putDebug "packages in environment"
            traverse_ (outputErr . prettyShow) pkgIds

        let cabalFile = fakePackage $ Map.fromListWith intersectVersionRanges $
                [ (pn, if ver == nullVersion || optAnyVer then anyVersion else thisVersion ver)
                | PackageIdentifier pn ver <- pkgIds
                , pn `notElem` [ mkPackageName "rts" ]
                ] ++
                [ (pn, vr)
                | Dependency pn vr _ <- optDeps
                ]

        when optVerbose $ do
            putDebug "Generated fake-package.cabal"
            outputErr cabalFile

        withSystemTempDirectory "cabal-env-fake-package-XXXX" $ \tmpDir -> do
            writeByteString (tmpDir </> fromUnrootedFilePath "fake-package.cabal") $ toUTF8BS cabalFile
            writeByteString (tmpDir </> fromUnrootedFilePath "cabal.project") $ toUTF8BS $ unlines
                [ "packages: ."
                , "with-compiler: " ++ optCompiler
                , "documentation: False"
                , "write-ghc-environment-files: always"
                , "package *"
                , "  documentation: False"
                ]

            ec <- runProcessOutput tmpDir "cabal" $
                ["v2-build", "all", "--builddir=dist-newstyle"] ++ ["--dry-run" | optDryRun ]

            case ec of
                ExitFailure _ -> do
                    putError "ERROR: cabal v2-build failed"
                    putError "This is might be due inconsistent dependencies (delete package env file, or try -a) or something else"
                    exitFailure

                ExitSuccess | optDryRun -> return ()
                ExitSuccess -> do
                    -- TODO: use cabal-plan to read stuff, better than relying on ghc.environment files, maybe?
                    -- TODO: remove Glob dependency
                    _plan <- liftIO $ findAndDecodePlanJson (InBuildDir $ toFilePath $ tmpDir </> fromUnrootedFilePath "dist-newstyle")
                    -- print plan

                    matches <- liftIO $ glob $ toFilePath $ tmpDir </> fromUnrootedFilePath ".ghc.environment.*-*-*"
                    case matches of
                        [envFile'] -> do
                            envFile <- makeAbsoluteFilePath envFile'
                            envFileContents <- readByteString envFile
                            when optVerbose $ do
                                putDebug "local .ghc.environment file"
                                outputErr $ fromUTF8BS envFileContents

                            -- strip local stuff
                            let ls :: [String]
                                ls = filter (\l -> not $ any (`isPrefixOf` l) ["package-db dist-newstyle", "package-id fake-package-0-inplace"])
                                   $ lines
                                   $ fromUTF8BS envFileContents

                            -- (over)write ghc environment file
                            let newEnvFileContents  :: String
                                newEnvFileContents = unlines ls
                            when optVerbose $ do
                                putDebug "writing environment file"
                                outputErr newEnvFileContents

                            writeByteString (ghcEnvDir </> fromUnrootedFilePath optEnvName) (toUTF8BS newEnvFileContents)

                        _ -> die "Cannot find .ghc.environment file"

-------------------------------------------------------------------------------
-- die
-------------------------------------------------------------------------------

die :: String -> Peu r a
die str = putError str *> exitFailure

-------------------------------------------------------------------------------
-- GHC environment directory
-------------------------------------------------------------------------------

getGhcEnvDir :: FilePath -> Peu r (Path Absolute)
getGhcEnvDir ghc = do
    ghcDir   <- getAppUserDataDirectory "ghc"

    infoBS <- LBS.toStrict <$> runProcessCheck ghcDir ghc ["--info"]
    info <- maybe (die "Cannot parse compilers --info output") return $
        readMaybe (fromUTF8BS infoBS)

    case lookup ("Project name" :: String) info of
        Just "The Glorious Glasgow Haskell Compilation System" -> do
            versionStr <- maybe (die "cannot find Project version in info") return $
                lookup "Project version" info
            ver <- case eitherParsec versionStr of
                Right ver -> return (ver :: Version)
                Left err  -> die $ "Project version cannot be parsed\n" ++ err

            targetStr <- maybe (die "cannot find Target platform in info") return $
                lookup "Target platform" info
            (x,y) <- case splitOn "-" targetStr of
                [x, _, y] -> return (x, y)
                _         -> die "Target platform is not triple"

            return $ ghcDir </> fromUnrootedFilePath (x ++ "-" ++ y ++ "-" ++ prettyShow ver) </> fromUnrootedFilePath "environments"

        _ -> die "Your compiler is not GHC"

-------------------------------------------------------------------------------
-- Some types
-------------------------------------------------------------------------------

data Command
    = CmdInstall Dependency  -- ^ install package
  deriving (Show)

data Environment = Env
    { envCommands :: [Command]
    , envPackages :: [PackageIdentifier]
    }
  deriving (Show)

-------------------------------------------------------------------------------
-- GHC environment file
-------------------------------------------------------------------------------

withEnvironmentFile :: forall r a. a -> ([String] -> a) -> Path Absolute -> Peu r a
withEnvironmentFile def k fp = handle onIOError $ do
    contents <- readByteString fp
    return (k (lines (fromUTF8BS contents)))
  where
    onIOError :: IOException -> Peu r a
    onIOError _ = return def

-- TODO: use Environment
parseEnvironmentContents :: [String] -> ([Command], [PackageIdentifier])
parseEnvironmentContents ls =
    ( []
    , mapMaybe (either (const Nothing) Just . parse) ls
    )
  where
    parse :: String -> Either String PackageIdentifier
    parse = explicitEitherParsec $ do
        _ <- P.string "package-id"
        P.spaces
        parts <- P.sepByNonEmpty (P.munch1 $ \c -> c == '.' || isAlphaNum c) (P.char '-')
        either fail return $
            if isHash (NE.last parts)
            then parse' (NE.init parts)
            else parse' (NE.toList parts)

    isHash :: String -> Bool
    isHash s = length s == 64

    parse' :: [String] -> Either String PackageIdentifier
    parse' = eitherParsec . intercalate "-"

getEnvironmentContents :: Path Absolute -> Peu r ([Command], [PackageIdentifier])
getEnvironmentContents = withEnvironmentFile mempty parseEnvironmentContents

-------------------------------------------------------------------------------
-- Options parser
-------------------------------------------------------------------------------

-- TODO: commands to list package environments, their contents, delete, copy.
-- TODO: special . name for "package environment in this directory"
data Opts = Opts
    { optCompiler :: FilePath
    , optEnvName  :: String
    , optAnyVer   :: Bool
    , optVerbose  :: Bool
    , optDryRun   :: Bool
    , optDeps     :: [Dependency]
    , optAction   :: Maybe Action
    }

data Action
    = ActionShow  -- ^ show package environment contents
    | ActionList  -- ^ list package environments

optsP :: O.Parser Opts
optsP = Opts
    <$> O.strOption (O.short 'w' <> O.long "with-compiler" <> O.value "ghc" <> O.showDefault <> O.help "Specify compiler to use")
    <*> O.strOption (O.short 'n' <> O.long "name" <> O.value "default" <> O.showDefault <> O.help "Environment name")
    <*> O.switch (O.short 'a' <> O.long "any" <> O.help "Allow any version of existing packages")
    <*> O.switch (O.short 'v' <> O.long "verbose" <> O.help "Print stuff...")
    <*> O.switch (O.short 'd' <> O.long "dry-run" <> O.help "Dry run, don't install anything")
    <*> many (O.argument (O.eitherReader eitherParsec) (O.metavar "PKG..." <> O.help "packages (with possible bounds)"))
    -- behaviour flags
    <*> optional actionP

actionP :: O.Parser Action
actionP = showP <|> listP where
    showP = O.flag' ActionShow (O.short 's' <> O.long "show" <> O.help "Shows the contents of the environment")
    listP = O.flag' ActionList (O.short 'l' <> O.long "list" <> O.help "List package environments")

-------------------------------------------------------------------------------
-- Fake project
-------------------------------------------------------------------------------

-- TODO: fakeProject :: ...

-------------------------------------------------------------------------------
-- cabal.config
-------------------------------------------------------------------------------

newtype CabalConfig = CabalConfig
    { ccStoreDir :: FilePath
    }

getCabalConfig :: IO CabalConfig
getCabalConfig = fail "not implemented"
