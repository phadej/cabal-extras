{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Data.TreeDiff
import Data.TreeDiff.Golden       (ediffGolden)
import Distribution.ModuleName    (ModuleName (..))
import Distribution.Pretty        (prettyShow)
import System.FilePath            ((</>))
import Test.Tasty                 (defaultMain, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)

import qualified Data.ByteString as BS

import CabalDiff.Hoogle

main :: IO ()
main = defaultMain $ testGroup "golden"
    [ testGroup "parser"
        [ golden "foldable1"
        , golden "optics-core"
        , golden "servant"
        , golden "singletons"
        , golden "vec"
        , golden "colour-2.3.4"
        , golden "colour-2.3.5"
        ]
    ]
  where
    golden name = ediffGolden goldenTest name goldenPath $ do
        contents <- BS.readFile hooglePath
        either  fail return $ parseFile contents
      where
        goldenPath = "fixtures" </> (name ++ ".golden")
        hooglePath = "fixtures" </> (name ++ ".txt")

-------------------------------------------------------------------------------
-- orphans
-------------------------------------------------------------------------------

instance ToExpr ModuleName where
    toExpr mn = App "ModuleName" [toExpr (prettyShow mn)]

instance ToExpr Key
