module CabalDiff.Diff (
    Diff (..),
    apiDiff,
    outputApiDiff,
    ) where

import Peura
import Prelude ()

import Data.Align              (alignWith)
import Distribution.ModuleName (ModuleName)
import System.Console.ANSI
       (Color (..), ColorIntensity (Vivid), ConsoleLayer (Foreground), SGR (..))

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

outputApiDiff :: TracerPeu r w -> Map ModuleName (Map Key Diff) -> Peu r ()
outputApiDiff tracer ad = do
    let colored c = [SetColor Foreground Vivid c]
    ifor_ ad $ \moduleName moduleDiff ->
        if all isSame moduleDiff
        then output tracer $ "    " ++ prettyShow moduleName
        else do
            outputSgr tracer (colored Cyan) $ "@@@" ++ " " ++ prettyShow moduleName
            ifor_ moduleDiff $ \key change -> case change of
                Same _rest   -> return ()
                Added rest   -> outputSgr tracer (colored Green) $ "++ " ++ renderKey key rest
                Removed rest -> outputSgr tracer (colored Red)   $ "-- " ++ renderKey key rest
                Changed a b  -> do
                                outputSgr tracer (colored Blue)    $ " - " ++ renderKey key a
                                outputSgr tracer (colored Yellow)  $ " + " ++ renderKey key b

-------------------------------------------------------------------------------
-- Test examples
-------------------------------------------------------------------------------

-- _test1 :: IO ()
-- _test1 = runPeu () $ do
--     a <- liftIO $ BS.readFile "fixtures/colour-2.3.4.txt" >>= either fail return . parseFile
--     b <- liftIO $ BS.readFile "fixtures/colour-2.3.5.txt" >>= either fail return . parseFile
--     outputApiDiff (let x = x in x) (apiDiff a b)
--
-- _test2 :: IO ()
-- _test2 = runPeu () $ do
--     a <- liftIO $ BS.readFile "fixtures/optics-core.txt" >>= either fail return . parseFile
--     b <- liftIO $ BS.readFile "fixtures/servant.txt" >>= either fail return . parseFile
--     outputApiDiff (let x = x in x) (apiDiff a b)
