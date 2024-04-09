module CabalHasklint.Lint where

import Data.Monoid (All (..))
import Peura

import qualified Data.Set                     as Set
import qualified Distribution.ModuleName      as C
import qualified Distribution.Types.BuildInfo as C

import GHC.Hs           (GhcPs, HsModule, hsmodImports, hsmodName)
import GHC.Hs.ImpExp    (ImportDecl (..), ImportDeclQualifiedStyle (..))
import GHC.Types.SrcLoc (GenLocated (..), Located, unLoc)

import Language.Haskell.Syntax.ImpExp      (ImportListInterpretation (Exactly))
import Language.Haskell.Syntax.Module.Name (moduleNameString)

import CabalHasklint.GHC.Utils
import CabalHasklint.Trace
import CabalHasklint.Warning

lint
    :: TracerPeu r Tr
    -> [C.ModuleName] -- TODO: maybe take configured PD?
    -> C.BuildInfo
    -> Located (HsModule GhcPs)
    -> Peu r All
lint tracer compModules _bi (L _ module_) = do
    traceApp tracer $ TraceLint (fromString thisModuleName)

    -- Check that all imports are either qualified or has import list,
    -- except for local ones.
    --
    -- TODO: add configuration.
    -- Which modules are OK to import unqualified, e.g. Prelude.Compat, Peura... GHC.Generics?
    --
    let compModules' = Set.fromList compModules
    for_ (hsmodImports module_) $ \(L loc importDecl) -> do
        let moduleName :: C.ModuleName
            moduleName = C.fromString (moduleNameString (unLoc (ideclName importDecl)))

        unless (Set.member moduleName compModules') $ case ideclQualified importDecl of
            NotQualified -> case ideclImportList importDecl of
                Just (Exactly, _) -> return ()
                _                 -> putWarning tracer WUnqualImport $ "Wild import of " ++ prettyShow moduleName ++ " in " ++ fakeShowPpr loc

            _ -> return ()

    return (All True)
  where
    thisModuleName = maybe "<unknown>" fakeShowPpr (hsmodName module_)
