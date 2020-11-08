{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | A quick'n'dirty to parse hoogle.txt files.
module CabalDiff.Hoogle (
    API,
    parseFile,
    Key,
    renderKey,
    ) where

import Peura
import Prelude ()

import Data.Char                           (isPrint)
import Distribution.Compat.CharParsing
       (char, eof, manyTill, satisfy, skipMany, spaces, string, try,
       unexpected)
import Distribution.ModuleName             (ModuleName)
import Distribution.Parsec
       (ParsecParser, parsec, runParsecParser)
import Distribution.Parsec.FieldLineStream (fieldLineStreamFromBS)

import qualified Data.Map.Strict as Map

-- | API of a package.
type API = Map ModuleName (Map Key String)

-- | Parse hoogle file, to get an (approximation of) API
parseFile :: ByteString -> Either String API
parseFile = explicitEitherParsecBS $ do
    whitespace
    packageP *> whitespace
    versionP *> whitespace

    entries <- concatMap (either (singleton . Left) (map Right)) <$> many entryP

    eof

    Map.fromList <$> postprocess entries
  where
    packageP = string "@package" *> untilEOL
    versionP = string "@version" *> untilEOL

    entryP = do
        word <- wordP
        spaces
        x <- case word of
            "module"   -> Left <$> moduleP
            "class"    -> Right . singleton <$> classP
            "instance" -> Right . singleton <$> instanceP
            "data"     -> Right . singleton <$> dataP
            "type"     -> Right . singleton <$> typeP
            "newtype"  -> Right . singleton <$> newtypeP
            "infixl"   -> Right . singleton <$> infixP KeyInL
            "infixr"   -> Right . singleton <$> infixP KeyInR
            "infix"    -> Right . singleton <$> infixP KeyInf
            "pattern"  -> Right . singleton <$> patternP
            _          -> Right <$> functionP word
        whitespace
        return x

    moduleP = parsec

    classP = do
        contents <- many (satisfy (\c -> c /= '\n' && c /= '{'))
        more <- optional $ do
            _ <- char '{'
            classContentsP `manyTill` char '}'

        return (KeyCls contents, maybe "" concat more)

    classContentsP = do
        whitespace
        (' ' :) <$> many (satisfy $ \c -> not (isSpace c || c == '}'))

    instanceP = do
        contents <- many (satisfy (\c -> c /= '\n'))
        return (KeyIns contents, "")

    patternP = do
        contents <- many (satisfy (\c -> c /= '\n'))
        return (KeyPat contents, "")

    dataP = do
        t <- optional (string "family")
        spaces
        name <- wordP
        spaces
        contents <- many (satisfy (\c -> c /= '\n'))

        case t of
            Nothing -> return (KeyDat name, contents)
            Just _  -> return (KeyDaF name, contents)

    newtypeP = do
        name <- wordP
        spaces
        spaces
        contents <- many (satisfy (\c -> c /= '\n'))
        return (KeyNew name, contents)

    typeP = do
        t <- optional (string "family")
        spaces
        name <- wordP
        spaces
        contents <- many (satisfy (\c -> c /= '\n'))

        case t of
            Nothing -> return (KeyTyp name, contents)
            Just _  -> return (KeyTyF name, contents)

    infixP con = do
        contents <- many (satisfy (\c -> c /= '\n'))
        return (con contents, "")

    functionP name = do
        names' <- many (char ',' *> spaces *> wordP <* spaces)
        let names = name :| names'

        _ <- string "::"
        spaces
        contents <- many (satisfy (\c -> c /= '\n'))
        return $ if null names' then [(KeyFun name, contents)] else
            case head names of
                '[' : _ -> [ (KeyFun $ "[" ++ filter notBracket n ++ "]", contents) | n <- toList names ]
                _       -> [ (KeyFun n, contents) | n <- toList names ]

    notBracket '[' = False
    notBracket ']' = False
    notBracket _   = True

    wordP = some (satisfy $ \c -> isPrint c && not (isSpace c) && c /= ',')

    -- spaces and comments
    whitespace = do
        spaces
        isComm <- optional (try (string "--"))
        case isComm of
            Nothing -> return ()
            Just _  -> untilEOL *> whitespace

    untilEOL = skipMany (satisfy (/= '\n')) *> void (char '\n')

    postprocess []                     = return []
    postprocess (Right (key, _) : _)   = unexpected $ "Database starts with " ++ show key
    postprocess (Left moduleName : xs) = case takeWhileMaybe fromRight xs of
        (ys, zs) -> do
            rest <- postprocess zs
            return ((moduleName, Map.fromList ys) : rest)

    fromRight = either (const Nothing) Just

takeWhileMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
takeWhileMaybe f = go where
    go xs@[]      = ([], xs)
    go xs@(x:xs') = case f x of
        Nothing -> ([],xs)
        Just y  -> let (ys,zs) = go xs' in (y:ys,zs)

singleton :: a -> [a]
singleton x = [x]

-- | Various entries in the API.
data Key
    = KeyCls String
    | KeyDaF String
    | KeyDat String
    | KeyInf String
    | KeyInL String
    | KeyInR String
    | KeyIns String
    | KeyNew String
    | KeyPat String
    | KeyTyF String
    | KeyTyp String
    | KeyFun String
  deriving (Eq, Ord, Show, Generic)

instance Binary Key

renderKey :: Key -> String -> String
renderKey (KeyCls k) rest = unwords ["class", k, rest]
renderKey (KeyDat k) rest = unwords ["data", k, rest]
renderKey (KeyDaF k) rest = unwords ["data", "family", k, rest]
renderKey (KeyInf k) rest = unwords ["infix", k, rest]
renderKey (KeyInL k) rest = unwords ["infixl", k, rest]
renderKey (KeyInR k) rest = unwords ["infixr", k, rest]
renderKey (KeyIns k) rest = unwords ["instance", k, rest]
renderKey (KeyNew k) rest = unwords ["newtype", k, rest]
renderKey (KeyPat k) rest = unwords ["pattern", k, rest]
renderKey (KeyTyF k) rest = unwords ["type", "family", k, rest]
renderKey (KeyTyp k) rest = unwords ["type", k, rest]
renderKey (KeyFun k) rest = unwords [k, "::", rest]

-- | Move to peura
explicitEitherParsecBS :: ParsecParser a -> ByteString -> Either String a
explicitEitherParsecBS parser
    = either (Left . show) Right
    . runParsecParser (parser <* spaces) "<eitherParsec>"
    . fieldLineStreamFromBS
