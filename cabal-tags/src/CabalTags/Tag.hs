{-# LANGUAGE StandaloneDeriving #-}
module CabalTags.Tag where

import Peura
import Prelude ()

import qualified CabalTags.GHC.All as GHC

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
tagsFromHieAST name' = go
  where
    file = GHC.unpackFS name' -- this is wrong

    go :: GHC.HieAST GHC.TypeIndex -> [Tag] -> [Tag]
    go ast acc = Tag (show (GHC.nodeAnnotations info, GHC.nodeIdentifiers info)) file line : foldr go acc (GHC.nodeChildren ast)
      where
        info = GHC.nodeInfo ast
        line = GHC.srcLocLine (GHC.realSrcSpanStart (GHC.nodeSpan ast))

{-
    go1 :: Int -> GHC.NodeIdentifiers GHC.TypeIndex -> [Tag] -> [Tag]
    go1 line idents acc = ifoldr (go2 line) acc idents

    go2 :: Int -> Either GHC.ModuleName GHC.Name -> GHC.IdentifierDetails GHC.TypeIndex -> [Tag] -> [Tag]
    go2 _line (Left _)  _details acc = acc
    go2  line (Right n) _details acc = Tag (show n) file line : acc
-}


deriving instance Show a => Show (GHC.IdentifierDetails a)

instance Show GHC.ModuleName where
    show _ = "MN"

instance Show GHC.Name where
    show = GHC.occNameString . GHC.occName
