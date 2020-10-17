{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications   #-}
module CabalEnv.Environment (
    Environment (..),
    withEnvironment,
    withEnvironmentMaybe,
    encodeEnvironment,
  ) where

import Peura

import Data.Generics.Lens.Lite       (field)
import Distribution.Types.Dependency (Dependency (..))

import qualified Cabal.Parse                     as Parse
import qualified Cabal.Plan                      as Plan
import qualified Codec.Compression.Lzma          as LZMA
import qualified Data.Aeson                      as A
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Base64          as Base64
import qualified Data.ByteString.Base64.Lazy     as Base64.Lazy
import qualified Data.ByteString.Char8           as BS8
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.ByteString.Lazy.Char8      as LBS8
import qualified Data.List                       as L
import qualified Data.Map.Strict                 as Map
import qualified Distribution.CabalSpecVersion   as C
import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.Compat.Newtype     as C
import qualified Distribution.FieldGrammar       as C
import qualified Distribution.Fields             as C
import qualified Distribution.Parsec             as C
import qualified Distribution.Parsec.Newtypes    as C
import qualified Distribution.Pretty             as C
import qualified Text.PrettyPrint                as PP

import CabalEnv.Warning

data Environment plan = Environment
    { envPackages   :: [Dependency]
    , envHidden     :: [PackageName]
    , envTransitive :: Bool
    , envLocalPkgs  :: Map PackageName (Path Absolute)
    , envPlan       :: plan
    }
  deriving (Show, Generic)

-------------------------------------------------------------------------------
-- Newtypes
-------------------------------------------------------------------------------

newtype BS64 = BS64 BS.ByteString
  deriving anyclass (C.Newtype BS.ByteString)

instance C.Parsec BS64 where
    parsec = do
        str <- many P.anyChar
        return (BS64 $ LBS.toStrict $ LZMA.decompress $ LBS.fromStrict $ Base64.decodeLenient $ toUTF8BS str)

instance C.Pretty BS64 where
    pretty (BS64 bs) = PP.vcat
        [ PP.text x
        | x <- chunks 80 str
        ]
      where
        str = LBS8.unpack $ Base64.Lazy.encode $ LZMA.compress $ LBS.fromStrict bs

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = case L.splitAt n xs of
    (ys, zs) -> ys : chunks n zs

newtype LocalPkgs = LocalPkgs (Map PackageName (Path Absolute))
  deriving anyclass (C.Newtype (Map PackageName (Path Absolute)))

instance C.Parsec LocalPkgs where
    parsec = fmap (LocalPkgs . Map.fromList) $ C.parsecLeadingCommaList $ do
        pn <- C.parsec
        P.spaces
        C.FilePathNT path <- C.parsec
        case fromAbsoluteFilePathMaybe path of
            Just path' -> return (pn, path')
            Nothing    -> P.unexpected $ "Not absolute path " ++ path

instance C.Pretty LocalPkgs where
    pretty (LocalPkgs m) = PP.vcat $ PP.punctuate PP.comma
        [ C.pretty pn PP.<+> C.pretty (C.FilePathNT (toFilePath path))
        | (pn, path) <- Map.toList m
        ]
-------------------------------------------------------------------------------
-- Grammars
-------------------------------------------------------------------------------

environmentGrammar
    :: (C.FieldGrammar g, Applicative (g (Environment BS.ByteString)))
    => g (Environment BS.ByteString) (Environment BS.ByteString)
environmentGrammar = Environment
    <$> C.uniqueFieldAla       "packages"   (C.alaList C.CommaVCat)  (field @"envPackages")
    <*> C.optionalFieldDefAla  "hidden"     (C.alaList C.CommaVCat)  (field @"envHidden")     []
    <*> C.booleanFieldDef      "transitive"                          (field @"envTransitive") False
    <*> C.monoidalFieldAla     "local-pkgs" LocalPkgs                (field @"envLocalPkgs")
    <*> C.uniqueFieldAla       "plan"       BS64                     (field @"envPlan")

parseEnvironment
    :: [C.Field C.Position]
    -> C.ParseResult (Environment BS.ByteString)
parseEnvironment fields0 = do
    let (fields1, _sections) = C.partitionFields fields0
    C.parseFieldGrammar C.cabalSpecLatest fields1 environmentGrammar

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

withEnvironment
    :: TracerPeu r W
    -> Path Absolute
    -> (Environment Plan.PlanJson -> Peu r ())
    -> Peu r ()
withEnvironment tracer fp k = withEnvironmentMaybe tracer fp $ \menv ->
    case menv of
        Nothing  -> putWarning tracer WMissingCabalEnvData $ "No cabal-env data found in " ++ toFilePath fp
        Just env -> k $ env { envPlan = snd (envPlan env) }

withEnvironmentMaybe
    :: TracerPeu r W
    -> Path Absolute
    -> (Maybe (Environment (BS.ByteString, Plan.PlanJson)) -> Peu r a)
    -> Peu r a
withEnvironmentMaybe tracer fp k = do
    exists <- doesFileExist fp
    if exists
    then do
        contents <- readByteString fp

        -- lines starting with @-- cabal-env @
        let ls :: [ByteString]
            ls = filter (not . BS.null)
               $ mapMaybe (BS.stripPrefix "-- cabal-env ")
               $ BS8.lines contents

        if null ls
        then k Nothing
        else case Parse.parseWith parseEnvironment (toFilePath fp) (BS8.unlines ls) of
            Left err  -> die tracer (displayException err)
            Right env -> case A.eitherDecodeStrict' (envPlan env) of
                Left err   -> die tracer err
                Right plan -> k (Just (env { envPlan = (envPlan env, plan) }))
    else k Nothing

-------------------------------------------------------------------------------
-- Encoding
-------------------------------------------------------------------------------

encodeEnvironment :: Environment BS.ByteString -> String
encodeEnvironment env =
    let fields = C.prettyFieldGrammar C.cabalSpecLatest environmentGrammar env
    in C.showFields (const []) fields
