module CabalHasklint.Opts where

import Peura

import qualified Data.Set                        as Set
import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.ModuleName         as C
import qualified Distribution.Parsec             as C
import qualified Distribution.Types.BuildInfo    as C
import qualified Options.Applicative             as O

import CabalHasklint.Trace
import CabalHasklint.Warning

-------------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------------

data Opts = Opts
    { optCabalPlan :: CabalPlan
    , optDynOpts   :: DynOpts -> DynOpts
    , optCompiler  :: FilePath
    , optBuildDir  :: FsPath
    , optTracer    :: TracerOptions W -> TracerOptions W
    , optTargets   :: [String]
    }


-- | Options which can change per component.
data DynOpts = DynOpts
    { optModules        :: Set C.ModuleName
    , optCppIncludeDirs :: [FsPath]
    , optVerbosity      :: Verbosity
    }

defaultDynOpts :: DynOpts
defaultDynOpts = DynOpts
    { optModules        = mempty
    , optCppIncludeDirs = []
    , optVerbosity      = Verbosity 0
    }

newtype Verbosity  = Verbosity Int deriving (Eq, Ord, Show)
data CabalPlan     = CabalPlan | NoCabalPlan deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Parse from BuildInfo
-------------------------------------------------------------------------------

dynOptsFromBuildInfo :: TracerPeu r Tr -> C.BuildInfo -> Peu r (DynOpts -> DynOpts)
dynOptsFromBuildInfo tracer bi = do
    endos <- for customFields (uncurry parse)
    return $ \x -> foldl' (&) x endos
  where
    customFields = C.customFieldsBI bi

    parse name@"x-hasklint-options" contents =
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

    parse _ _ = return id

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

optsP :: O.Parser Opts
optsP = pure Opts
    <*> cabalPlanP
    <*> dynOptsP
    <*> O.strOption (O.short 'w' <> O.long "with-compiler" <> O.metavar "PATH" <> O.value "ghc" <> O.showDefault <> O.help "Specify compiler to use, should be GHC-9.2")
    <*  optional (O.flag' () (O.long "ghc" <> O.help "Compiler is GHC (always on)"))
    <*> O.option fspath (O.long "builddir" <> O.value (fromFilePath "dist-newstyle") <> O.metavar "BUILDDIR")
    <*> tracerOptionsParser
    <*> many (O.strArgument $ O.metavar "TARGET")

fspath :: O.ReadM FsPath
fspath = O.eitherReader $ return . fromFilePath

dynOptsP :: O.Parser (DynOpts -> DynOpts)
dynOptsP = pure combine
    <*> setP moduleNameP
    <*> listP cppDirP
    <*> verbosityP
  where
    setP :: Ord a => O.Parser a -> O.Parser (Set a -> Set a)
    setP p = (\xs ys -> Set.fromList xs <> ys) <$> many p

    listP :: O.Parser a -> O.Parser ([a] -> [a])
    listP p = flip (++) <$> many p

    combine f1 f2 f3  (DynOpts x1 x2 x3) =
        DynOpts (f1 x1) (f2 x2) (f3 x3)

lastOpt :: [a] -> a -> a
lastOpt xs initial = foldl' (\_ x -> x) initial xs

cabalPlanP :: O.Parser CabalPlan
cabalPlanP = foldl' (\_ x -> x) CabalPlan <$> many (on <|> off) where
    on  = O.flag'   CabalPlan $ O.long    "cabal-plan" <> O.help "Look for cabal plan. (default)"
    off = O.flag' NoCabalPlan $ O.long "no-cabal-plan" <> O.help "Don't look for cabal plan. Interpret targets as paths to .cabal files"

extP :: O.Parser String
extP = O.strOption (O.short 'X' <> O.metavar "EXT" <> O.help "Extensions")

extraPkgP :: O.Parser PackageName
extraPkgP = O.option (O.eitherReader C.eitherParsec) $
    O.long "extra-package" <> O.metavar "PKG" <> O.help "Extra packages to require (should exist in a plan)"

moduleNameP :: O.Parser C.ModuleName
moduleNameP = O.option (O.eitherReader C.eitherParsec) $
    O.short 'm' <> O.long "module" <> O.metavar "MODULE" <> O.help "Which modules to check (all if empty)"

rtsOptsP :: O.Parser [String]
rtsOptsP = O.option (fmap words O.str) $
    O.long "ghci-rtsopts" <> O.metavar "OPTS" <> O.help "RTS options for GHCi process"

verbosityP :: O.Parser (Verbosity -> Verbosity)
verbosityP = accum <$> many (plusP <|> minusP)
  where
    accum xs (Verbosity v) = Verbosity (foldl' (+) v xs)

    plusP :: O.Parser Int
    plusP =  O.flag' 1 (O.short 'v' <> O.long "verbose" <> O.help "Increase verbosity level")

    minusP :: O.Parser Int
    minusP = O.flag' (negate 1) (O.short 'q' <> O.long "quiet" <> O.help "Decrease verbosity level")

cppDirP :: O.Parser FsPath
cppDirP = O.option fspath (O.short 'I' <> O.help "Add ⟨dir⟩ to the directory search list for #include files")
