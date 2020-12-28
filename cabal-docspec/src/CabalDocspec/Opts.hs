module CabalDocspec.Opts where

import Peura

import qualified Distribution.Parsec as C
import qualified Options.Applicative as O

import CabalDocspec.Warning

data Opts = Opts
    { optPhase      :: Phase
    , optCabalPlan  :: CabalPlan
    , optStripComs  :: StripComments
    , optGhci       :: GhciOpts
    , optExtraPkgs  :: [PackageName]
    , optCompiler   :: FilePath
    , optBuilddir   :: FsPath
    , optVerbosity  :: Verbosity
    , optTracer     :: TracerOptions W -> TracerOptions W
    , optTargets    :: [String]
    }

data Verbosity = Quiet | Normal | Verbose deriving (Eq, Ord)

data GhciOpts = GhciOpts
    { optPreserveIt :: PreserveIt
    , optExts       :: [String]
    , optTimeout    :: Double
    , optSetup      :: [String]
    }
  deriving Show

data PreserveIt    = PreserveIt | DontPreserveIt deriving (Eq, Show)
data CabalPlan     = CabalPlan | NoCabalPlan deriving (Eq, Show)
data StripComments = StripComments | DontStripComments deriving (Eq, Show)

data Phase
    = Manual
    | Phase1
    | Phase2
  deriving (Eq, Ord, Show)

optsP :: O.Parser Opts
optsP = pure Opts
    <*> phaseP
    <*> cabalPlanP
    <*> stripComsP
    <*> ghciOptsP
    <*> many extraPkgP
    <*> O.strOption (O.short 'w' <> O.long "with-compiler" <> O.metavar "PATH" <> O.value "ghc" <> O.showDefault <> O.help "Specify compiler to use")
    <*  optional (O.flag' () (O.long "ghc" <> O.help "Compiler is GHC (always on)"))
    <*> O.option fspath (O.long "builddir" <> O.value (fromFilePath "dist-newstyle") <> O.metavar "BUILDDIR")
    <*> verbosityP
    <*> tracerOptionsParser
    <*> many (O.strArgument $ O.metavar "TARGET")
  where
    fspath = O.eitherReader $ return . fromFilePath

ghciOptsP :: O.Parser GhciOpts
ghciOptsP = pure GhciOpts
    <*> preserveItP
    <*> many extP
    <*> O.option O.auto (O.long "timeout" <> O.metavar "SECS" <> O.value 3 <> O.help "Evaluation timeout in seconds")
    <*> many (O.strOption (O.long "setup" <> O.metavar "EXPR" <> O.help "A setup expression"))

preserveItP :: O.Parser PreserveIt
preserveItP = foldl' (\_ x -> x) DontPreserveIt <$> many (on <|> off) where
    on  = O.flag'     PreserveIt $ O.long    "preserve-it" <> O.help "Preserve it variable"
    off = O.flag' DontPreserveIt $ O.long "no-preserve-it" <> O.help "Don't preserve it variable. (default)"

cabalPlanP :: O.Parser CabalPlan
cabalPlanP = foldl' (\_ x -> x) CabalPlan <$> many (on <|> off) where
    on  = O.flag'   CabalPlan $ O.long    "cabal-plan" <> O.help "Look for cabal plan. (default)"
    off = O.flag' NoCabalPlan $ O.long "no-cabal-plan" <> O.help "Don't look for cabal plan. Interpret targets as paths to .cabal files"

stripComsP :: O.Parser StripComments
stripComsP = foldl' (\_ x ->  x) DontStripComments <$> many (on <|> off) where
    on  = O.flag'     StripComments $ O.long    "strip-comments" <> O.help "Strip comments in examples"
    off = O.flag' DontStripComments $ O.long "no-strip-comments" <> O.help "Don't strip comments in examples"

extP :: O.Parser String
extP = O.strOption (O.short 'X' <> O.metavar "EXT" <> O.help "Extensions")

extraPkgP :: O.Parser PackageName
extraPkgP = O.option (O.eitherReader C.eitherParsec) $
    O.long "extra-package" <> O.metavar "PKG" <> O.help "Extra packages to require (should exist in a plan)"

phaseP :: O.Parser Phase
phaseP = phase1P <|> phase2P <|> manualP <|> pure Phase2 where
    phase1P = O.flag' Phase1 (O.long "phase1" <> O.help "Perform only phase 1 (extraction of examples)")
    phase2P = O.flag' Phase2 (O.long "phase2" <> O.help "Perform phase2 (execution of examples in GHCi)")
    manualP = O.flag' Manual (O.long "man" <> O.help "Show manual")

verbosityP :: O.Parser Verbosity
verbosityP = mk . foldl' (+) 0 <$> many (plusP <|> minusP)
  where
    mk :: Integer -> Verbosity
    mk n = case compare n 0 of
        LT -> Quiet
        EQ -> Normal
        GT -> Verbose

    plusP :: O.Parser Integer
    plusP =  O.flag' 1 (O.short 'v' <> O.long "verbose" <> O.help "Increase verbosity level")

    minusP :: O.Parser Integer
    minusP = O.flag' (negate 1) (O.short 'q' <> O.long "quiet" <> O.help "Decrease verbosity level")
