{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
module Hiero where

import Prelude

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Text.PrettyPrint as PP

import qualified FastString as GHC
import qualified HieTypes as GHC

import qualified CabalTags.GHC.All as GHC

data AST loc a = AST loc String String [String] [AST loc a]
  deriving Show

fromHieAST :: GHC.HieAST a -> AST GHC.Span a
fromHieAST ast = case Set.toList (GHC.nodeAnnotations info) of
    []              -> AST loc ""                 ""                  ids children
    (con, name) : _ -> AST loc (GHC.unpackFS con) (GHC.unpackFS name) ids children
  where
    loc  = GHC.nodeSpan ast
    info = GHC.nodeInfo ast

    children = map fromHieAST (GHC.nodeChildren ast)
    ids      = map (either GHC.moduleNameString (GHC.occNameString . GHC.occName)) $ Map.keys (GHC.nodeIdentifiers info)

-------------------------------------------------------------------------------
-- Patterns
-------------------------------------------------------------------------------

pattern ASTModule :: loc -> [String] -> [AST loc a] -> AST loc a
pattern ASTModule loc ids xs = AST loc "Module" "Module" ids xs

pattern ASTNames :: loc -> [String] -> [AST loc a] -> AST loc a
pattern ASTNames loc ids xs = AST loc "" "" ids xs

-------------------------------------------------------------------------------
-- Pretty-printing
-------------------------------------------------------------------------------

ppAST :: AST loc a -> PP.Doc
ppAST (AST _loc x y zs ws) = PP.parens $ PP.hang xy' 2 $ PP.sep (zs' ++ ws')
  where
    xy' | null x, null y = "#"
        | otherwise      = PP.text $ x ++ "." ++ y
    zs'                  = map PP.text zs
    ws'                  = map ppAST ws
