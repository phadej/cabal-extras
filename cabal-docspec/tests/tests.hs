module Main (main) where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@=?), (@?=))

import qualified Data.Set as Set

import CabalDocspec.Diff
import CabalDocspec.ExprVars


main :: IO ()
main = defaultMain $ testGroup "cabal-docspec"
    [ exprVarsTests
    , diffTests
    ]

exprVarsTests :: TestTree
exprVarsTests = testGroup "ExprVars"
    [ ex "x + y"                                 ["x","y"]
    , ex "forAll xs $ \\x -> x == x"             ["forAll", "xs"]
    , ex "\\(xs :: [Int]) -> reverse xs === xs"  ["reverse"]
    ]
  where
    ex expr vars = testCase expr $ Set.fromList vars @=? exprVars expr

diffTests :: TestTree
diffTests = testGroup "Diff"
    [ testCase "diffString1" $ diffString (map Exact "foo")           "boo"  @?= (1, 3, DS "f" "b" (SS "oo" ES))
    , testCase "diffString2" $ diffString (Wildcard : map Exact "oo") "bboo" @?= (0, 4, SS "bboo" ES)

    , testCase "diffLines1"  $ diffLines [Wildcard] ["aaa","bbb"]                          @?= (0, DiffSame ["aaa","bbb"] DiffEmpty)
    , testCase "diffLines2"  $ diffLines [Wildcard, Exact $ map Exact "bbb"] ["aaa","bbb"] @?= (0, DiffSame ["aaa","bbb"] DiffEmpty)
    , testCase "diffLines3"  $ diffLines [Wildcard, Exact $ map Exact "bbc"] ["aaa","bbb"] @?=
         (1 / 3,DiffSame ["aaa"] (DiffChunk [Norm "bb" (Emph "c" Empty)] [Norm "bb" (Emph "b" Empty)] DiffEmpty))
    ]