{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CabalDocspec.Phase2 (
    phase2,
) where

import Peura

import Control.Monad      (foldM)
import Data.List          (isInfixOf)
import System.Environment (getEnvironment)

import qualified Cabal.Config           as Cabal
import qualified Data.Map               as Map
import qualified Data.Set               as Set
import qualified Language.Haskell.Lexer as L

import CabalDocspec.Doctest.Example
import CabalDocspec.Doctest.Extract
import CabalDocspec.Doctest.Parse
import CabalDocspec.ExprVars
import CabalDocspec.GHCi
import CabalDocspec.Located
import CabalDocspec.Opts
import CabalDocspec.Summary
import CabalDocspec.Trace
import CabalDocspec.Warning

phase2
    :: forall r. TracerPeu r Tr
    -> DynOpts
    -> [UnitId]
    -> GhcInfo
    -> Maybe (Path Absolute) -- ^ Build directory, @builddir@
    -> Cabal.Config Identity
    -> Path Absolute
    -> [(String,String)]
    -> [Module [Located DocTest]]
    -> Peu r Summary
phase2 tracer dynOpts unitIds ghcInfo mbuildDir cabalCfg cwd extraEnv parsed = do
    let preserveIt = case optPreserveIt dynOpts of
            PreserveIt     -> True
            DontPreserveIt -> False

    let timeout :: Int
        timeout = max fastTimeout $ truncate $ optTimeout dynOpts * 1e6

    let timeoutMsg :: String
        timeoutMsg = optTimeoutMsg dynOpts ++ "\n"

    -- second phase: fire up the ghci, and execute stuff
    storeDir <- makeAbsoluteFilePath $ runIdentity $ Cabal.cfgStoreDir cabalCfg
    let storeDir' = storeDir </> fromUnrootedFilePath ("ghc-" ++ prettyShow (ghcVersion ghcInfo))
    let storeDb = storeDir' </> fromUnrootedFilePath "package.db"
    storeExists <- doesDirectoryExist storeDb

    let GhcFlags {..} = getGhcFlags ghcInfo

    let rtsArgs :: [String]
        rtsArgs
            | null (optGhciRtsopts dynOpts) = []
            | otherwise = ["+RTS"] ++ optGhciRtsopts dynOpts ++ ["-RTS"]

    let ghciArgs :: [String]
        ghciArgs =
            [ "-i" -- so we don't explode on hs-source-dirs: . packages
            ] ++
            [ "-X" ++ ext
            | ext <- optExts dynOpts
            ] ++
            [ "-hide-all-packages"
            , ghcFlagNoUserPackageDb
            -- , "-package-db=" ++ toFilePath (ghcGlobalDb ghcInfo)
            ] ++
            [ ghcFlagPackageDb ++ "=" ++ toFilePath storeDb
            | storeExists
            ] ++
            [ ghcFlagPackageDb ++ "=" ++ toFilePath localDb
            | buildDir <- toList mbuildDir
            , let localDb = buildDir </> fromUnrootedFilePath "packagedb" </> fromUnrootedFilePath ("ghc-" ++ prettyShow (ghcVersion ghcInfo))
            ] ++
            [ ghcFlagPackageId ++ "=" ++ u
            | u <- map prettyShow unitIds
            ] ++ rtsArgs

    currEnv <- liftIO getEnvironment
    let env = Map.toList $ Map.fromList $ extraEnv ++ currEnv

    withInteractiveGhc tracer ghcInfo cwd env ghciArgs $ \ghci -> do
        fmap mconcat $ for parsed $ \m -> do
            traceApp tracer $ TracePhase2 (moduleName m)

            let reset :: Peu r ()
                reset = do
                    -- load module in question, also resets imports
                    _ <- eval tracer ghci False fastTimeout timeoutMsg $ ":m " ++ prettyShow (moduleName m)

                    -- reload, resets local bindings
                    void $ eval tracer ghci False fastTimeout timeoutMsg ":r"

                    -- seed it varible.
                    when preserveIt $ void $ eval tracer ghci False fastTimeout timeoutMsg "()"

            let skipSetup :: SubSummary -> Located DocTest -> Peu r (Either SubSummary SubSummary)
                skipSetup acc ld = return (Right (acc <> foldMap skipSetupDocTest ld))

            let skipCliSetup :: SubSummary -> String -> Peu r (Either SubSummary SubSummary)
                skipCliSetup acc _ = return (Right (acc <> ssSkip))

            let runCliSetup :: SubSummary -> String -> Peu r (Either SubSummary SubSummary)
                runCliSetup acc expr = do
                    result <- eval tracer ghci preserveIt timeout timeoutMsg expr
                    case mkResult [] (lines result) of
                            Equal -> return (Left (acc <> ssSuccess))
                            NotEqual diff -> do
                                putError tracer expr
                                putError tracer $ unlines ("" : diff)
                                return (Right (acc <> ssError))

            let runSetup :: SubSummary -> Located DocTest -> Peu r (Either SubSummary SubSummary)
                runSetup acc (L pos doctest) = case doctest of
                    Property expr -> do
                        putError tracer $ "properties are not supported in setup, skipping: " ++ expr
                        return (Right (acc <> ssFailure))

                    Example expr expected -> do
                        result <- eval tracer ghci preserveIt timeout timeoutMsg expr
                        case mkResult expected (lines result) of
                            Equal -> return (Left (acc <> ssSuccess))
                            NotEqual diff -> do
                                putError tracer expr
                                putError tracer $ prettyPos pos ++ unlines ("" : diff)
                                return (Right (acc <> ssError))

            let runSetupGroup :: Peu r SubSummary
                runSetupGroup = do
                    reset

                    -- command line --setup
                    intermediate <- foldl2 runCliSetup skipCliSetup (Left mempty) (optSetup dynOpts)

                    -- in file $setup
                    either id id <$> case moduleSetup m of
                        Nothing     -> return intermediate
                        Just setups -> foldl2 runSetup skipSetup intermediate setups

            let skipping :: Summary -> Located DocTest -> Peu r (Either Summary Summary)
                skipping acc ld = return (Right (acc <> foldMap skipDocTest ld))

            let runExample :: Summary -> Located DocTest -> Peu r (Either Summary Summary)
                runExample acc (L pos doctest) = case doctest of
                    Property expr -> case optProperties dynOpts of
                        SkipProperties -> do
                            putWarning tracer WSkippedProperty $ prettyPos pos ++ " skipping " ++ expr
                            return (Left (acc <> mempty { sProperties = ssSkip }))

                        SimpleProperties -> do
                            let vars = exprVars expr `Set.intersection` optPropVariables dynOpts

                            let lambdaPrefix :: String
                                lambdaPrefix
                                    | null vars = ""
                                    | otherwise = unwords $ "\\" : Set.toList vars ++ ["-> "]

                            -- imports
                            _ <- eval tracer ghci False fastTimeout timeoutMsg
                                "import Test.QuickCheck (quickCheck)"

                            -- evaluate property
                            result <- eval tracer ghci False timeout timeoutMsg $
                                "quickCheck (" ++ lambdaPrefix ++ expr ++ ")"

                            if "OK, passed" `isInfixOf` result
                            then return (Left (acc <> mempty { sProperties = ssSuccess }))
                            else do
                                putError tracer expr
                                putError tracer $ prettyPos pos ++ "\n" ++ result
                                return (Right (acc <> mempty { sProperties = ssError }))

                    Example expr expected -> do
                        result <- eval tracer ghci preserveIt timeout timeoutMsg expr
                        case mkResult expected (lines result) of
                            Equal -> do
                                return (Left (acc <> mempty { sExamples = ssSuccess }))
                            NotEqual diff -> do
                                putError tracer expr
                                putError tracer $ prettyPos pos ++ unlines ("" : diff)
                                return (Right (acc <> mempty { sExamples = ssError }))

            let runExampleGroup :: Summary -> [Located DocTest] -> Peu r Summary
                runExampleGroup acc
                    = fmap (either id id)
                    . foldl2 runExample skipping (Left acc)

            -- run setup
            setupRes <- runSetupGroup

            -- if there are no issues in setup
            if isOk setupRes
            then do
                -- ... run content
                let combine xs = mconcat (mempty { sSetup = setupRes } : xs)
                fmap combine $ for (moduleContent m) $ \contents -> do
                    -- we don't recount setups, even they are rerun
                    _ <- runSetupGroup
                    runExampleGroup mempty contents

            else do
                -- ... skip run
                putWarning tracer WErrorInSetup $
                    "Issue in $setup, skipping " ++ prettyShow (moduleName m) ++ " module"
                let res = foldMap (foldMap (foldMap skipDocTest)) (moduleContent m)
                return $ mempty { sSetup = setupRes } <> res

foldl2
    :: (Monad m, Foldable f)
    => (b -> a -> m (Either b c))
    -> (c -> a -> m (Either b c))
    -> Either b c
    -> f a
    -> m (Either b c)
foldl2 f g = foldM go where
    go (Left b)  a = f b a
    go (Right c) a = g c a

fastTimeout :: Int
fastTimeout = 1000000

eval :: TracerPeu r Tr -> GHCi -> Bool -> Int -> String -> String -> Peu r String
eval tracer ghci preserveIt timeout timeoutMsg expr = do
    traceApp tracer $ TraceGHCiInput expr
    res <- sendExpressions tracer ghci preserveIt timeout [expr]
    case res of
        Timeout -> return timeoutMsg
        Exited ec -> die tracer $ "ghci exited " ++ show ec
        Result out err -> do
            -- putDebug tracer (fromUTF8BS out)
            -- putDebug tracer (fromUTF8BS err)

            -- first the "importand" error output, then standard output
            return (fromUTF8BS err ++ fromUTF8BS out)

prettyPos :: L.Pos -> String
prettyPos pos = "in comment at " ++ show (L.line pos) ++ ":" ++ show (L.column pos)
