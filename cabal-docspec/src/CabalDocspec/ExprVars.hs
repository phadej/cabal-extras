{-# LANGUAGE DeriveFunctor #-}
module CabalDocspec.ExprVars (
    exprVars,
) where

import Peura

import qualified Language.Haskell.Lexer  as L
import qualified Data.Set as Set

exprVars :: String -> Set String
exprVars input =
    expr Set.empty Set.empty $ filter notWhitespace $ L.lexerPass0 input
  where
    expr :: Set String -> Set String -> [L.PosToken] -> Set String
    expr  acc  bound []                                 = Set.difference acc bound
    
    -- collect ids
    expr  acc  bound ((L.Varid, (_, v))         : rest) = expr (Set.insert v acc) bound rest
    -- we skip Varsyms. Don't use them in properties :)

    expr  acc  bound ((L.Reservedop, (_, "\\")) : rest) = 
        let (vars, rest') = span notArrow rest
        in expr acc (bound <> Set.fromList (mapMaybe isVarid vars)) rest'

    -- debug
    -- expr  acc  bound rest = error (show (take 5 rest))

    -- for other tokens, we simply continue.
    expr  acc  bound (_                         : rest) = expr acc bound rest

notWhitespace :: L.PosToken -> Bool
notWhitespace (L.Whitespace, _) = False
notWhitespace _                 = True

isVarid :: L.PosToken -> Maybe String
isVarid (L.Varid, (_, v)) = Just v
isVarid _                 = Nothing

notArrow :: L.PosToken -> Bool
notArrow (L.Reservedop, (_, "->")) = False
notArrow _                         = True
