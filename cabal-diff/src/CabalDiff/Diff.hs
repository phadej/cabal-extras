module CabalDiff.Diff (
    Diff (..),
    apiDiff,
    outputApiDiff,
    ) where

import Peura
import Prelude ()

import Data.Align              (alignWith)
import Data.These              (These (..))
import Distribution.ModuleName (ModuleName)
import System.Console.ANSI
       (Color (..), ConsoleLayer (Foreground), ColorIntensity(Vivid), SGR (..))

import qualified Data.ByteString as BS

import CabalDiff.Hoogle

data Diff
    = Added String
    | Removed String
    | Same String
    | Changed String String
  deriving (Show)

isSame :: Diff -> Bool
isSame (Same _) = True
isSame _        = False

apiDiff
    :: API -> API
    -> Map ModuleName (Map Key Diff)
apiDiff = alignWith $ \m' -> case m' of
    This m -> fmap Removed m
    That m -> fmap Added m
    These x y -> alignWith f x y
  where
    f (This a) = Removed a
    f (That a) = Added a
    f (These a b)
        | a == b    = Same a
        | otherwise = Changed a b

outputApiDiff :: Map ModuleName (Map Key Diff) -> Peu r ()
outputApiDiff ad = withSetSgrCode $ \setSgrCode -> do
    let colored c s = setSgrCode [SetColor Foreground Vivid c] ++ s ++ setSgrCode []
    ifor_ ad $ \moduleName moduleDiff ->
        if all isSame moduleDiff
        then output $ "    " ++ prettyShow moduleName
        else do
            output $ colored Cyan "@@@" ++ " " ++ prettyShow moduleName
            ifor_ moduleDiff $ \key change -> case change of
                Same _rest   -> return ()
                Added rest   -> output $ colored Green $ "++ " ++ renderKey key rest
                Removed rest -> output $ colored Red   $ "-- " ++ renderKey key rest
                Changed a b  -> do
                                output $ colored Blue    $ " - " ++ renderKey key a
                                output $ colored Yellow  $ " + " ++ renderKey key b

-------------------------------------------------------------------------------
-- Test examples
-------------------------------------------------------------------------------

_test1 :: IO ()
_test1 = runPeu () $ do
    a <- liftIO $ BS.readFile "fixtures/colour-2.3.4.txt" >>= either fail return . parseFile
    b <- liftIO $ BS.readFile "fixtures/colour-2.3.5.txt" >>= either fail return . parseFile
    outputApiDiff (apiDiff a b)

_test2 :: IO ()
_test2 = runPeu () $ do
    a <- liftIO $ BS.readFile "fixtures/optics-core.txt" >>= either fail return . parseFile
    b <- liftIO $ BS.readFile "fixtures/servant.txt" >>= either fail return . parseFile
    outputApiDiff (apiDiff a b)
