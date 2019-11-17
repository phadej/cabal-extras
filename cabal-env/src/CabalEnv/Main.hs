{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Copyright: Oleg Grenrus
-- License: GPL-3.0-or-later
module CabalEnv.Main (main) where

-- TODO use Peura
import CabalEnv.Prelude

import Control.Exception    (handle)
import Data.Semigroup       (Semigroup (..))
import Data.Version         (showVersion)
import System.Directory
       (createDirectoryIfMissing, getAppUserDataDirectory, listDirectory)
import System.FilePath.Glob (glob)
import System.IO.Temp       (withSystemTempDirectory)
import System.Process
       (StdStream (NoStream), createProcess, cwd, proc, std_in, waitForProcess)

import Cabal.Plan (SearchPlanJson (InBuildDir), findAndDecodePlanJson)

import Distribution.Version
       (anyVersion, intersectVersionRanges, nullVersion, thisVersion)

import qualified Data.ByteString                 as BS
import qualified Data.List.NonEmpty              as NE
import qualified Data.Map.Strict                 as Map
import qualified Distribution.Compat.CharParsing as P
import qualified Options.Applicative             as O

import CabalEnv.FakePackage
import CabalEnv.Utils

import Paths_cabal_env (version)

main :: IO ()
main = do
    opts <- O.execParser optsP'
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

listAction :: Opts -> FilePath -> IO ()
listAction Opts {..} ghcEnvDir = do
    when optVerbose $ putStrLn $ "Environments available in " ++ ghcEnvDir
    fs <- listDirectory ghcEnvDir
    for_ (sort fs) $ \f ->
        putStrLn $ f ++ if optVerbose then " " ++ ghcEnvDir </> f else ""

-------------------------------------------------------------------------------
-- Show
-------------------------------------------------------------------------------

showAction :: Opts -> FilePath -> IO ()
showAction Opts {..} ghcEnvDir = do
    (_cmds, pkgIds) <- getEnvironmentContents $ ghcEnvDir </> optEnvName
    when optVerbose $ putStrLn $ "Packages in " ++ optEnvName ++ " environment"
    for_ (sort pkgIds) $ \pkgId ->
        -- TODO: when verbose mark which are installed
        putStrLn $ prettyShow pkgId

-------------------------------------------------------------------------------
-- Install
-------------------------------------------------------------------------------

installAction :: Opts -> FilePath -> IO ()
installAction Opts {..} ghcEnvDir = do
    unless (null optDeps) $ do
        when optVerbose $ do
            putStrLn "=== ghc environment directory"
            putStrLn ghcEnvDir

        createDirectoryIfMissing True ghcEnvDir
        (cmds, pkgIds) <- getEnvironmentContents $ ghcEnvDir </> optEnvName

        when optVerbose $ do
            putStrLn "=== packages in environment"
            traverse_ (putStrLn . prettyShow) pkgIds

        when (null cmds) $ do
            putStrLn "No cabal-fmt commands found in the environment"
            putStrLn "Using all packages in the environment as input"

        let cabalFile = fakePackage $ Map.fromListWith intersectVersionRanges $
                [ (pn, if ver == nullVersion || optAnyVer then anyVersion else thisVersion ver)
                | PackageIdentifier pn ver <- pkgIds
                , pn `notElem` [ mkPackageName "rts" ]
                ] ++
                [ (pn, vr)
                | Dependency pn vr _ <- optDeps
                ]

        when optVerbose $ do
            putStrLn "=== Generated fake-package.cabal"
            putStrLn cabalFile

        withSystemTempDirectory "cabal-env-fake-package-XXXX" $ \tmpDir -> do
            writeFile (tmpDir </> "fake-package.cabal") cabalFile
            writeFile (tmpDir </> "cabal.project") $ unlines
                [ "packages: ."
                , "with-compiler: " ++ optCompiler
                , "documentation: False"
                , "write-ghc-environment-files: always"
                , "package *"
                , "  documentation: False"
                ]

            (_, _, _, hdl) <- createProcess $
                    let p0 = proc "cabal" $ ["v2-build", "all", "--builddir=dist-newstyle"] ++ if optDryRun then ["--dry-run"] else []
                        p  = p0
                            { cwd    = Just tmpDir
                            , std_in = NoStream
                            }
                        in p
            ec <- waitForProcess hdl

            case ec of
                ExitFailure _ -> do
                    hPutStrLn stderr "ERROR: cabal v2-build failed"
                    hPutStrLn stderr "This is might be due inconsistent dependencies (delete package env file, or try -a) or something else"
                    exitFailure

                ExitSuccess | optDryRun -> return ()
                ExitSuccess -> do
                    -- TODO: use cabal-plan to read stuff, better than relying on ghc.environment files, maybe?
                    -- TODO: remove Glob dependency
                    _plan <- findAndDecodePlanJson (InBuildDir $ tmpDir </> "dist-newstyle")
                    -- print plan

                    matches <- glob $ tmpDir </> ".ghc.environment.*-*-*"
                    case matches of
                        [envFile] -> do
                            envFileContents <- BS.readFile envFile
                            when optVerbose $ do
                                putStrLn "=== local .ghc.environment file"
                                BS.putStr envFileContents

                            -- strip local stuff
                            let ls :: [String]
                                ls = filter (\l -> not $ any (`isPrefixOf` l) ["package-db dist-newstyle", "package-id fake-package-0-inplace"])
                                   $ lines
                                   $ fromUTF8BS envFileContents

                            -- (over)write ghc environment file
                            when optVerbose $ do
                                putStrLn "=== writing environment file"
                                putStr $ unlines ls

                            writeFile (ghcEnvDir </> optEnvName) (unlines ls)

                        _ -> die "Cannot find .ghc.environment file"


-------------------------------------------------------------------------------
-- GHC environment directory
-------------------------------------------------------------------------------

getGhcEnvDir :: FilePath -> IO FilePath
getGhcEnvDir ghc = do
    ghcDir   <- getAppUserDataDirectory "ghc"

    infoBS <- readProcessWithExitCode' ghc ["--info"]
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

            return $ ghcDir </> (x ++ "-" ++ y ++ "-" ++ prettyShow ver) </> "environments"

        _ -> die "Your compiler is not GHC"

-------------------------------------------------------------------------------
-- Some types
-------------------------------------------------------------------------------

data Command
    = CmdInstall Dependency  -- ^ install package
  deriving (Show)

data Environment = Env
    { envCommands :: [Command]
    , envPackages :: [PackageId]
    }
  deriving (Show)

-------------------------------------------------------------------------------
-- GHC environment file
-------------------------------------------------------------------------------

withEnvironmentFile :: forall r. r -> ([String] -> r) -> FilePath -> IO r
withEnvironmentFile def k fp = handle onIOError $ do
    contents <- BS.readFile fp
    return (k (lines (fromUTF8BS contents)))
  where
    onIOError :: IOError -> IO r
    onIOError _ = return def

-- TODO: use Environment
parseEnvironmentContents :: [String] -> ([Command], [PackageId])
parseEnvironmentContents ls =
    ( []
    , mapMaybe (either (const Nothing) Just . parse) ls
    )
  where
    parse :: String -> Either String PackageId
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

    parse' :: [String] -> Either String PackageId
    parse' = eitherParsec . intercalate "-"

getEnvironmentContents :: FilePath -> IO ([Command], [PackageId])
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
