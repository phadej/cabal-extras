{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
-- |
-- Copyright: Oleg Grenrus
-- License: GPL-2.0-or-later
module CabalDeps.Main (main) where

import Peura
import Prelude ()

import Control.Applicative ((<**>))
import Data.Version        (showVersion)

import qualified Cabal.Index          as I
import qualified Cabal.Plan           as P
import qualified Data.Map.Strict      as Map
import qualified Data.Set             as Set
import qualified Distribution.Package as C
import qualified Distribution.Pretty  as C
import qualified Options.Applicative  as O

import Paths_cabal_deps (version)

main :: IO ()
main = do
    opts <- O.execParser optsP'
    runPeu () $ doDeps opts
  where
    optsP' = O.info (optsP <**> O.helper <**> versionP) $ mconcat
        [ O.fullDesc
        , O.progDesc "Check project or package deps"
        , O.header "cabal-diff"
        ]

    versionP = O.infoOption (showVersion version)
        $ O.long "version" <> O.help "Show version"

-------------------------------------------------------------------------------
-- Options parser
-------------------------------------------------------------------------------

data Opts = Opts

optsP :: O.Parser Opts
optsP = pure Opts

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

doDeps :: Opts -> Peu r ()
doDeps _ = do
    putInfo "Reading Hackage metadata"
    meta <- liftIO I.cachedHackageMetadata

    putInfo "Reading plan.json for current project"
    plan <- liftIO $ P.findAndDecodePlanJson (P.ProjectRelativeToDir ".")

    let pkgIds :: Set C.PackageIdentifier
        pkgIds = Set.fromList
            [ toCabal (P.uPId unit)
            | unit <- Map.elems (P.pjUnits plan)
            -- filter global-db packages
            , P.uType unit /= P.UnitTypeBuiltin
            ]

    for_ pkgIds $ \pkgId ->
        check meta pkgId

    -- TODO: use exitCode to indicate

-- TODO: return True or False if fine or not.
check
    :: Map PackageName I.PackageInfo
    -> C.PackageIdentifier
    -> Peu r ()
check meta pid@(C.PackageIdentifier pn ver) =
    case Map.lookup pn meta of
        Nothing -> putWarning WNotOnHackage $ C.prettyShow pn ++ " is not on Hackage"
        Just pi -> case Map.lookupMax (I.piPreferredVersions pi) of
            Nothing        ->
                putWarning WNoPreferredVersions $ C.prettyShow pn ++ " doesn't have preferred versions"
            Just (ver', _)
                | ver == ver' -> return ()
                      -- putDebug $ C.prettyShow pid ++ " is latest version"
                | ver <  ver' -> putWarning WNotLatest $
                    C.prettyShow pn ++ " doesn't use latest version"
                    ++ "; latest " ++ C.prettyShow ver'
                    ++ "; used " ++ C.prettyShow ver
                | otherwise   -> putWarning WNotOnHackage $
                    C.prettyShow pid ++ " is not on Hackage"


-------------------------------------------------------------------------------
-- Warnings
-------------------------------------------------------------------------------

data W
    = WNotOnHackage
    | WNoPreferredVersions
    | WNotLatest

instance Warning W where
    warningToFlag WNotOnHackage        = "not-on-hackage"
    warningToFlag WNoPreferredVersions = "no-preferred-versions" -- TODO: that's not good name
    warningToFlag WNotLatest           = "not-latest"
