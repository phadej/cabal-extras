module CabalTags.Tag where

import Peura
import Prelude ()

import qualified CabalTags.GHC.All as GHC

import Hiero

{-

CabalTags.GHC.All	cabal-tags/src/CabalTags/GHC/All.hs	1
CabalTags.Main	cabal-tags/src/CabalTags/Main.hs	4
CabalTags.Tag	cabal-tags/src/CabalTags/Tag.hs	1
Finite W	cabal-tags/src/CabalTags/Main.hs	117
Main	cabal-tags/cli/Main.hs	1
Opts	cabal-tags/src/CabalTags/Main.hs	94
Opts	cabal-tags/src/CabalTags/Main.hs	94
Universe W	cabal-tags/src/CabalTags/Main.hs	116
W	cabal-tags/src/CabalTags/Main.hs	109
WMissingIfaceFile	cabal-tags/src/CabalTags/Main.hs	113
WModuleWithOrphans	cabal-tags/src/CabalTags/Main.hs	111
WOrphans	cabal-tags/src/CabalTags/Main.hs	110
WUnknownUnit	cabal-tags/src/CabalTags/Main.hs	112
Warning W	cabal-tags/src/CabalTags/Main.hs	119
createNameCache	cabal-tags/src/CabalTags/Main.hs	85
main	cabal-tags/src/CabalTags/Main.hs	27
optCompiler	cabal-tags/src/CabalTags/Main.hs	95
optTracer	cabal-tags/src/CabalTags/Main.hs	96
optsP	cabal-tags/src/CabalTags/Main.hs	99
universe	cabal-tags/src/CabalTags/Main.hs	116
warningToFlag	cabal-tags/src/CabalTags/Main.hs	120


-}

-------------------------------------------------------------------------------
-- Tag
-------------------------------------------------------------------------------

data Tag = Tag
    { tagId   :: String
    , tagFile :: FilePath
    , tagLine :: Int
    }
  deriving Show

-------------------------------------------------------------------------------
-- HIE AST traversals
-------------------------------------------------------------------------------

tagsFromHieFile :: GHC.HieFile -> [Tag]
tagsFromHieFile hieFile = ifoldr tagsFromHieAST [] (GHC.getAsts (GHC.hie_asts hieFile))

tagsFromHieAST :: GHC.FastString -> GHC.HieAST GHC.TypeIndex -> [Tag] -> [Tag]
tagsFromHieAST name' ghcast acc0 = case ast of
    ASTModule _loc _name children -> foldr go acc0 children 
    _                             -> acc0
  where
    ast = fromHieAST ghcast
    file = GHC.unpackFS name' -- this is wrong

    go :: AST GHC.Span GHC.TypeIndex -> [Tag] -> [Tag]
    go (AST sp "AbsBinds" _ _ (ASTNames _ [name] _ : _)) acc = Tag name file line : acc
      where
        line = GHC.srcLocLine (GHC.realSrcSpanStart sp)

    go _ acc = acc



