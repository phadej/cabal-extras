{-# LANGUAGE BangPatterns #-}
module CabalDocspec.Lexer (
    -- * Passes
    stubbornPass0,
    needsCppPass,
    Pos,
    -- * Comments
    Comment (..),
    extractComments,
    extractDocstrings,
    -- ** Stripping comments
    dropComments,
) where

import Peura

import Data.Char              (toLower)
import Data.List              (init, break, stripPrefix)
import Language.Haskell.Lexer (Pos, PosToken)
import Text.Read              (read)

import qualified Distribution.ModuleName as C
import qualified Language.Haskell.Lexer  as L
import qualified Text.Parsec             as P

import CabalDocspec.Doctest.Extract
import CabalDocspec.Located

-------------------------------------------------------------------------------
-- Needs CPP pass
-------------------------------------------------------------------------------

-- | Whether input needs a CPP pass.
--
-- Looks for leading LANGUAGE pragmas for occurence of CPP.
--
needsCppPass :: Bool -> String -> Maybe [PosToken]
needsCppPass True  _     = Nothing
needsCppPass False input = go tokens
  where
    tokens = stubbornPass0 input

    go :: [PosToken] -> Maybe [PosToken]
    go []                             = Just tokens
    go ((L.Commentstart,  _):xs)      = go xs
    go ((L.Comment,  _):xs)           = go xs
    go ((L.Whitespace, _):xs)         = go xs
    go ((L.NestedComment, (_, s)):xs)
        | "CPP" `elem` parseLanguagePragma s
        = Nothing

        | otherwise
        = go xs
    go _                              = Just tokens

-------------------------------------------------------------------------------
-- Pass
-------------------------------------------------------------------------------

-- Stubborn pass filters out error tokens (which start with
-- and continues with the rest
--
-- This works around the following @haskell-lexer@ deficiencies:
--  - doesn't recognise prefix quotes
--  - errors on many unicode characters in comments
--
stubbornPass0 :: String -> [PosToken]
stubbornPass0 = stubborn . L.lexerPass0 where
    stubborn :: [PosToken] -> [PosToken]
    stubborn [] = []
    stubborn [(L.ErrorToken, (pos, '\'' : s)), (L.TheRest, (_, s'))] =
        map (second (first (addPos pos))) $ stubbornPass0 (s ++ s')

    stubborn [(L.Commentstart, (pos, cs)), (L.ErrorToken, (_, s)), (L.TheRest, (_, c : t))] =
        adjust $ map (second (first (addPos pos))) $ stubbornPass0 (cs ++ s ++ " " ++ t)
      where
        adjust (tcs@(L.Commentstart, _) : (L.Comment, (cpos, s')) : ts)
          | Just (' ' : t') <- stripPrefix s s'
          = tcs : (L.Comment, (cpos, s <> [c] <> t')) : ts
        adjust ts = ts

    stubborn (t : ts) = t : stubborn ts

    -- only line...
    addPos pos1 pos2 = pos2
        { L.line   = L.line pos1 + L.line pos2 - 1
        }

-------------------------------------------------------------------------------
-- Comment
-------------------------------------------------------------------------------

data Comment
    = LineCommentBlock Pos String
    | NestedComment Pos String
  deriving Show

instance NFData Comment where
    rnf (LineCommentBlock pos s) = rnfPos pos `seq` rnf s
    rnf (NestedComment pos s)    = rnfPos pos `seq` rnf s

rnfPos :: Pos -> ()
rnfPos (L.Pos x y z) = rnf x `seq` rnf y `seq` rnf z

extractComments :: [PosToken] -> [Comment]
extractComments = go 0 where
    go :: Int -> [PosToken] -> [Comment]
    go _diff [] = []
    -- normal comments, -- foo
    go diff ((L.Commentstart, (pos, s)) : (L.Comment, (_pos, str)) : next) =
        goLines diff (adjustPos diff pos) (\n -> s ++ str ++ n) next
    -- nested comments, {- foo -}
    go diff ((L.NestedComment, (pos, s)) : next) = case parseLinePragma s of
        Just l  -> go (l - L.line pos - 1) next
        Nothing -> NestedComment (adjustPos diff pos) s : go diff next
    -- other things are skipped
    go diff (_ : next) = go diff next

    goLines :: Int -> Pos -> (String -> String) -> [PosToken] -> [Comment]
    goLines diff !pos !acc ((L.Commentstart, (_, s)) : (L.Comment, (_, str)) : next) =
        goLines diff pos (acc . (s ++) . (str ++)) next
    -- there could be whitespace between line comments, but not empty lines.
    goLines diff !pos !acc ((L.Whitespace, (_,s)) : next)
        | all (/= '\n') s
        = goLines diff pos acc next
    goLines diff !pos !acc next = LineCommentBlock pos (acc "") : go diff next

    -- adjust position based on {-# LINE #-} pragmas
    adjustPos diff pos = pos { L.line = L.line pos + diff }

-------------------------------------------------------------------------------
-- Process expressions
-------------------------------------------------------------------------------

dropComments :: String -> String
dropComments
    = init
    . concat
    . map wsComment
    . L.lexerPass0
    . (++ "\n") -- work around https://github.com/yav/haskell-lexer/issues/9
  where
    wsComment :: PosToken -> String
    wsComment (L.Commentstart,  (_, s)) = map ws s
    wsComment (L.NestedComment, (_, s)) = map ws s
    wsComment (L.Comment,       (_, s)) = map ws s
    wsComment (_,               (_, s)) =        s

    ws c = if isSpace c then c else ' '

-------------------------------------------------------------------------------
-- Extract docstrings
-------------------------------------------------------------------------------

extractDocstrings :: C.ModuleName -> [Comment] -> Module (Located String)
extractDocstrings modname comments = Module
    { moduleName    = modname
    , moduleSetup   = listToMaybe
        [ c
        | NamedComment "setup" c <- comments'
        ]
    , moduleContent = mapMaybe nonSetup comments'
    }
  where
    nonSetup (HdkComment c) = Just c
    nonSetup (NamedComment "setup" _) = Nothing
    nonSetup (NamedComment _ c) = Just c

    comments' = mapMaybe classifyComment comments

data HdkComment
    = NamedComment String (Located String)
    | HdkComment (Located String)
  deriving (Show)

classifyComment :: Comment -> Maybe HdkComment
classifyComment c = case s of
    '|' : _ -> Just (HdkComment (L pos s))
    '^' : _ -> Just (HdkComment (L pos s))
    '*' : _ -> Just (HdkComment (L pos s)) -- this is an over approximation, for exports
    -- https://gitlab.haskell.org/ghc/ghc/-/blob/bd877edd9499a351db947cd51ed583872b2facdf/compiler/GHC/Parser/Lexer.x#L1404-1407
    '$' : z -> let (name, rest) = break isSpace z in Just (NamedComment name (L pos rest))
    _       -> Nothing
  where
    s = commentString c
    pos = commentPos c

commentString :: Comment -> String
commentString (NestedComment _ ('{' : '-' : rest)) = dropWhile isSpace $ dropTrailingClose rest
commentString (NestedComment _ s)                  = dropWhile isSpace s
commentString (LineCommentBlock _ s)               = dropWhile isSpace $ unlines $ map (dropWhile (== '-')) $ lines s

commentPos :: Comment -> Pos
commentPos (NestedComment p _)    = p
commentPos (LineCommentBlock p _) = p

dropTrailingClose :: String -> String
dropTrailingClose "-}"   = ""
dropTrailingClose (c:cs) = c : dropTrailingClose cs
dropTrailingClose []     = []


-------------------------------------------------------------------------------
-- Pragma parsers
-------------------------------------------------------------------------------

parseLinePragma :: String -> Maybe Int
parseLinePragma input =
    case P.parse linePragmaP "<input>" input of
        Right res -> Just res
        Left _    -> Nothing
  where
    linePragmaP :: P.Parsec String () Int
    linePragmaP = do
        _ <- P.string "{-#"
        skipSpacesP
        token <- tokenP
        unless (map toLower token == "line") $ fail $ "unexpected " ++ token
        skipSpacesP
        read <$> some (P.satisfy isDigit)

parseLanguagePragma :: String -> [String]
parseLanguagePragma input =
    case P.parse languagePragmaP "<input>" input of
        Right res -> res
        Left _    -> []
  where
    languagePragmaP :: P.Parsec String () [String]
    languagePragmaP = do
        _ <- P.string "{-#"
        skipSpacesP
        token <- tokenP
        unless (map toLower token == "language") $ fail $ "unexpected " ++ token
        skipSpacesP

        P.sepBy1 (tokenP <* skipSpacesP) (P.char ',' *> skipSpacesP)

skipSpacesP :: P.Parsec String () ()
skipSpacesP = P.skipMany P.space

tokenP :: P.Parsec String () String
tokenP = some $ P.satisfy isAlphaNum
