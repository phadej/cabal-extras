--
-- This module is derivative of
--
-- Copyright (c) 2009-2018 Simon Hengel <sol@typeful.net>
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.
--
module CabalDocspec.Doctest.Example (
    Result (..),
    mkResult,
) where

import Prelude

import Peura (fromString)

import qualified System.Console.ANSI        as ANSI
import qualified Text.PrettyPrint.Annotated as PP

import CabalDocspec.Diff
import CabalDocspec.Doctest.Parse

data Result = Equal | NotEqual (PP.Doc [ANSI.SGR])
  deriving (Eq, Show)

-- TODO: escape
mkResult :: ExpectedResult -> [String] -> Result
mkResult expected actual =
      case diffLines expected' actual of
        (0, _) -> Equal
        (_, d) -> NotEqual (ppDiff d)
  where
    expected' :: [Wild [Wild Char]]
    expected' =
        [ case l of
            WildCardLine -> Wildcard
            ExpectedLine l' -> Exact $ concat
                [ case c of
                    WildCardChunk -> [Wildcard]
                    LineChunk c'  -> map Exact c'
                | c <- l'
                ]

        | l <- expected
        ]

ppDiff :: Diff -> PP.Doc [ANSI.SGR]
ppDiff = PP.vcat . go where
    go :: Diff -> [PP.Doc [ANSI.SGR]]
    go DiffEmpty       = []
    go (DiffSame ls d) =
        [ fromString $ ' ' : l
        | l <- ls
        ] ++ go d

    go (DiffChunk xs ys d) =
        [ PP.annotate [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red] $ "-" <> go' ANSI.Red x
        | x <- xs
        ] ++
        [ PP.annotate [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green] $ "+" <> go' ANSI.Green y
        | y <- ys
        ] ++ go d

    go' :: ANSI.Color -> EmphString -> PP.Doc [ANSI.SGR]
    go' _ Empty       = ""
    go' c (Norm s ss) = fromString s <> go' c ss
    go' c (Emph s ss) = bold c (fromString s) <> go' c ss

bold :: ANSI.Color -> PP.Doc [ANSI.SGR] -> PP.Doc [ANSI.SGR]
bold c = PP.annotate  [ANSI.SetColor ANSI.Background ANSI.Dull c, ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White]
