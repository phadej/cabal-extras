module Main (main) where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@=?))

import qualified Data.Set as Set

import CabalDocspec.ExprVars

main :: IO ()
main = defaultMain $ testGroup "cabal-docspec"
    [ exprVarsTests
    ]

exprVarsTests :: TestTree
exprVarsTests = testGroup "ExprVars"
    [ ex "x + y"                                 ["x","y"]
    , ex "forAll xs $ \\x -> x == x"             ["forAll", "xs"]
    , ex "\\(xs :: [Int]) -> reverse xs === xs"  ["reverse"]
    ]
  where
    ex expr vars = testCase expr $ Set.fromList vars @=? exprVars expr
