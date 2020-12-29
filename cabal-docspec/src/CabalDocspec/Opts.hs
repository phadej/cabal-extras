module CabalDocspec.Opts where

import Peura

import qualified Distribution.Parsec          as C
import qualified Distribution.Types.BuildInfo as C
import qualified Distribution.Compat.CharParsing as P
import qualified Options.Applicative          as O

import CabalDocspec.Trace
import CabalDocspec.Warning

data Opts = Opts
    { optCabalPlan  :: CabalPlan
    , optGhci       :: DynOpts -> DynOpts
    , optCompiler   :: FilePath
    , optBuilddir   :: FsPath
    , optTracer     :: TracerOptions W -> TracerOptions W
    , optTargets    :: [String]
    }


-- | Options which can change per component.
data DynOpts = DynOpts
    { optPhase      :: Phase
    , optPreserveIt :: PreserveIt
    , optStripComs  :: StripComments
    , optExts       :: [String]
    , optTimeout    :: Double
    , optSetup      :: [String]
    , optExtraPkgs  :: [PackageName]
    , optVerbosity  :: Verbosity
    }
  deriving Show

defaultDynOpts :: DynOpts
defaultDynOpts = DynOpts
    { optPhase      = Phase2
    , optPreserveIt = DontPreserveIt
    , optStripComs  = DontStripComments
    , optExts       = []
    , optTimeout    = 3
    , optSetup      = []
    , optExtraPkgs  = []
    , optVerbosity  = Verbosity 0
    }

newtype Verbosity  = Verbosity Int deriving (Eq, Ord, Show)
data PreserveIt    = PreserveIt | DontPreserveIt deriving (Eq, Show)
data CabalPlan     = CabalPlan | NoCabalPlan deriving (Eq, Show)
data StripComments = StripComments | DontStripComments deriving (Eq, Show)

data Phase
    = Manual
    | Phase1
    | Phase2
  deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- Parse from BuildInfo
-------------------------------------------------------------------------------

-- TODO: take tracer
dynOptsFromBuildInfo :: TracerPeu r Tr -> C.BuildInfo -> Peu r (DynOpts -> DynOpts)
dynOptsFromBuildInfo tracer bi = do
    endos <- for customFields (uncurry parse)
    return $ \x -> foldl' (&) x endos
  where
    customFields = C.customFieldsBI bi

    parse name@"x-docspec-options" contents =
        case C.explicitEitherParsec (many (C.parsecToken <* P.spaces)) contents of
            Left err -> do
                putWarning tracer WInvalidField $ name ++ ": " ++ err
                return id
            Right args -> case O.execParserPure (O.prefs mempty) (O.info dynOptsP mempty) args of
                O.Success x -> return x
                O.Failure f -> do
                    putWarning tracer WInvalidField $ name ++ ": " ++ fst (O.renderFailure f name)
                    return id
                O.CompletionInvoked _ -> do
                    putWarning tracer WInvalidField $ name ++ ": optparse-applicatice completion invoked"
                    return id

    parse name@"x-docspec-extra-packages" contents =
        case C.explicitEitherParsec (many (C.parsec <* P.spaces)) contents of
            Left err -> do
                putWarning tracer WInvalidField $ name ++ ": " ++ err
                return id

            Right pkgs -> return $ \dynOpts -> dynOpts
                { optExtraPkgs = optExtraPkgs dynOpts ++ pkgs
                }


    parse _ _ = return id

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

optsP :: O.Parser Opts
optsP = pure Opts
    <*> cabalPlanP
    <*> dynOptsP
    <*> O.strOption (O.short 'w' <> O.long "with-compiler" <> O.metavar "PATH" <> O.value "ghc" <> O.showDefault <> O.help "Specify compiler to use")
    <*  optional (O.flag' () (O.long "ghc" <> O.help "Compiler is GHC (always on)"))
    <*> O.option fspath (O.long "builddir" <> O.value (fromFilePath "dist-newstyle") <> O.metavar "BUILDDIR")
    <*> tracerOptionsParser
    <*> many (O.strArgument $ O.metavar "TARGET")
  where
    fspath = O.eitherReader $ return . fromFilePath

dynOptsP :: O.Parser (DynOpts -> DynOpts)
dynOptsP = pure combine
    <*> phaseP
    <*> preserveItP
    <*> stripComsP
    <*> listP extP
    <*> timeoutP
    <*> listP (O.strOption (O.long "setup" <> O.metavar "EXPR" <> O.help "A setup expression"))
    <*> listP extraPkgP
    <*> verbosityP
  where
    listP p = flip (++) <$> many p
    combine f1 f2 f3 f4 f5 f6 f7 f8 (DynOpts x1 x2 x3 x4 x5 x6 x7 x8) =
        DynOpts (f1 x1) (f2 x2) (f3 x3) (f4 x4) (f5 x5) (f6 x6) (f7 x7) (f8 x8)

lastOpt :: [a] -> a -> a
lastOpt xs initial = foldl' (\_ x -> x) initial xs

preserveItP :: O.Parser (PreserveIt -> PreserveIt)
preserveItP = lastOpt <$> many (on <|> off) where
    on  = O.flag'     PreserveIt $ O.long    "preserve-it" <> O.help "Preserve it variable"
    off = O.flag' DontPreserveIt $ O.long "no-preserve-it" <> O.help "Don't preserve it variable. (default)"

timeoutP :: O.Parser (Double -> Double)
timeoutP = lastOpt <$> many p where
    p = O.option O.auto (O.long "timeout" <> O.metavar "SECS" <> O.help "Evaluation timeout in seconds")

cabalPlanP :: O.Parser CabalPlan
cabalPlanP = foldl' (\_ x -> x) CabalPlan <$> many (on <|> off) where
    on  = O.flag'   CabalPlan $ O.long    "cabal-plan" <> O.help "Look for cabal plan. (default)"
    off = O.flag' NoCabalPlan $ O.long "no-cabal-plan" <> O.help "Don't look for cabal plan. Interpret targets as paths to .cabal files"

stripComsP :: O.Parser (StripComments -> StripComments)
stripComsP = lastOpt <$> many (on <|> off) where
    on  = O.flag'     StripComments $ O.long    "strip-comments" <> O.help "Strip comments in examples"
    off = O.flag' DontStripComments $ O.long "no-strip-comments" <> O.help "Don't strip comments in examples"

extP :: O.Parser String
extP = O.strOption (O.short 'X' <> O.metavar "EXT" <> O.help "Extensions")

extraPkgP :: O.Parser PackageName
extraPkgP = O.option (O.eitherReader C.eitherParsec) $
    O.long "extra-package" <> O.metavar "PKG" <> O.help "Extra packages to require (should exist in a plan)"

phaseP :: O.Parser (Phase -> Phase)
phaseP = lastOpt <$> many (phase1P <|> phase2P <|> manualP) where
    phase1P = O.flag' Phase1 (O.long "phase1" <> O.help "Perform only phase 1 (extraction of examples)")
    phase2P = O.flag' Phase2 (O.long "phase2" <> O.help "Perform phase2 (execution of examples in GHCi)")
    manualP = O.flag' Manual (O.long "man" <> O.help "Show manual")

verbosityP :: O.Parser (Verbosity -> Verbosity)
verbosityP = accum <$> many (plusP <|> minusP)
  where
    accum xs (Verbosity v) = Verbosity (foldl' (+) v xs)

    plusP :: O.Parser Int
    plusP =  O.flag' 1 (O.short 'v' <> O.long "verbose" <> O.help "Increase verbosity level")

    minusP :: O.Parser Int
    minusP = O.flag' (negate 1) (O.short 'q' <> O.long "quiet" <> O.help "Decrease verbosity level")
