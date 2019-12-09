{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
module CabalEnv.Environment (
    Environment (..),
    withEnvironment,
    withEnvironmentMaybe,
    encodeEnvironment,
  ) where

import Peura

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
import qualified Distribution.CabalSpecVersion   as C
import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.Compat.Lens        as C
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
    , envPlan       :: plan
    }
  deriving (Show)

envPackagesL :: C.Lens' (Environment plan) [Dependency]
envPackagesL f s = (\x -> s { envPackages = x }) <$> f (envPackages s)

envHiddenL :: C.Lens' (Environment plan) [PackageName]
envHiddenL f s = (\x -> s { envHidden = x }) <$> f (envHidden s)

envTransitiveL :: C.Lens' (Environment plan) Bool
envTransitiveL f s = (\x -> s { envTransitive = x }) <$> f (envTransitive s)

envPlanL :: C.Lens' (Environment plan) plan
envPlanL f s = (\x -> s { envPlan = x }) <$> f (envPlan s)

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

-------------------------------------------------------------------------------
-- Grammars
-------------------------------------------------------------------------------

environmentGrammar
    :: (C.FieldGrammar g, Applicative (g (Environment BS.ByteString)))
    => g (Environment BS.ByteString) (Environment BS.ByteString)
environmentGrammar = Environment
    <$> C.uniqueFieldAla       "packages"   (C.alaList C.CommaVCat)  envPackagesL
    <*> C.optionalFieldDefAla  "hidden"     (C.alaList C.CommaVCat)  envHiddenL      []
    <*> C.booleanFieldDef      "transitive"                          envTransitiveL  False
    <*> C.uniqueFieldAla       "plan"       BS64                     envPlanL

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
    :: Path Absolute
    -> (Environment Plan.PlanJson -> Peu r ())
    -> Peu r ()
withEnvironment fp k = withEnvironmentMaybe fp $ \menv ->
    case menv of
        Nothing  -> putWarning WMissingCabalEnvData $ "No cabal-env data found in " ++ toFilePath fp
        Just env -> k $ env { envPlan = snd (envPlan env) }

withEnvironmentMaybe
    :: Path Absolute
    -> (Maybe (Environment (BS.ByteString, Plan.PlanJson)) -> Peu r a)
    -> Peu r a
withEnvironmentMaybe fp k = do
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
            Right env -> case A.eitherDecodeStrict' (envPlan env) of
                Right plan -> k (Just (env { envPlan = (envPlan env, plan) }))
                Left err   -> do
                    putError err
                    die ""
            Left err  -> do
                putError $ displayException err
                die ""
    else k Nothing

-------------------------------------------------------------------------------
-- Encoding
-------------------------------------------------------------------------------

encodeEnvironment :: Environment BS.ByteString -> String
encodeEnvironment env =
    let fields = C.prettyFieldGrammar C.cabalSpecLatest environmentGrammar env
    in C.showFields (const []) fields
