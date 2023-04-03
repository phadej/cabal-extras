module CabalDocspec.Diff (
    Wild (..),
    Diff (..),
    EmphString (..),
    diffLines,
    diffString,
    DiffStr (..),
) where

import Peura
import Prelude (minimum)

import qualified Data.Ratio
import qualified Data.Vector as V

-- | Expected results can contain wildcards.
data Wild a = Wildcard | Exact a

-------------------------------------------------------------------------------
-- String
-------------------------------------------------------------------------------

data DiffStr
    = ES                       -- ^ end
    | SS String        DiffStr -- ^ same string
    | DS String String DiffStr -- ^ swapped string
  deriving (Eq, Show)

consSame :: Char -> DiffStr -> DiffStr
consSame c (SS s d) = SS (c : s) d
consSame c d        = SS [c] d

consLeft :: Char -> DiffStr -> DiffStr
consLeft c (DS l r d) = DS (c : l) r d
consLeft c d          = DS [c] [] d

consRight :: Char -> DiffStr -> DiffStr
consRight c (DS l r d) = DS l (c : r) d
consRight c d          = DS [] [c] d

consDiff :: Char -> Char -> DiffStr -> DiffStr
consDiff c c' (DS l r d) = DS (c : l) (c' : r) d
consDiff c c' d          = DS [c] [c'] d

-- | String result
data SR = SR !Int DiffStr
  deriving Show

instance Eq SR where
    SR n _ == SR m _ = n == m

instance Ord SR where
    compare (SR n _) (SR m _) = compare n m

-- | Diff two strings returning the similarity metric.
--
diffString :: [Wild Char] -> String -> (Int, Int, DiffStr)
diffString xs ys = case cached 0 0 of
    SR d ds -> (d, max il jl, ds) -- TODO: don't count wilds.
  where
    is = V.fromList xs
    js = V.fromList ys

    il = V.length is
    jl = V.length js

    cached :: Int -> Int -> SR
    cached i j = cachedV V.! (j + (jl + 1) * i)

    cachedV :: V.Vector SR
    cachedV = V.generate ((il + 1) * (jl + 1)) $ \ij -> case ij `divMod` (jl + 1) of
        (i, j)
            | i == il
            , j == jl
            -> SR 0 ES

            | i == il
            , let SR d ds = cached i (j + 1)
            , let jc = js V.! j
            -> SR (d + 1) (consRight jc ds)

            | j == jl
            , let SR d ds = cached (i + 1) j
            -> case is V.! i of
              Wildcard -> SR d ds
              Exact ic -> SR (d + 1) (consLeft ic ds)

            | otherwise
            , let jc = js V.! j
            -> case is V.! i of
              Wildcard -> minimum
                  [ let SR d ds = cached i (j + 1) in SR d (consSame jc ds)
                  , cached (i + 1) j
                  ]
              Exact ic ->
                  if ic == jc
                  then (let SR d ds = cached (i + 1) (j + 1) in SR d (consSame ic ds))
                  else minimum
                      [ let SR d ds = cached i       (j + 1) in SR (d + 1) (consRight jc ds)
                      , let SR d ds = cached (i + 1) j       in SR (d + 1) (consLeft ic ds)
                      , let SR d ds = cached (i + 1) (j + 1) in SR (d + 1) (consDiff ic jc ds)
                      ]

-------------------------------------------------------------------------------
-- Lines
-------------------------------------------------------------------------------

data Diff
    = DiffEmpty
    | DiffSame [String] Diff
    | DiffChunk [EmphString] [EmphString] Diff
  deriving (Eq, Show)

-- | String with emphasis.
data EmphString
    = Emph String EmphString
    | Norm String EmphString
    | Empty
  deriving (Eq, Show)

consS :: String -> Diff -> Diff
consS c (DiffSame s d) = DiffSame (c : s) d
consS c d              = DiffSame [c] d

consL :: String -> Diff -> Diff
consL c (DiffChunk l r d) = DiffChunk (Norm c Empty : l) r d
consL c d         = DiffChunk [Norm c Empty] [] d

consR :: String -> Diff -> Diff
consR c (DiffChunk l r d) = DiffChunk l (Norm c Empty : r) d
consR c d         = DiffChunk [] [Norm c Empty] d

consD :: DiffStr -> Diff -> Diff
consD ds (DiffChunk l r d) = DiffChunk (x : l) (y : r) d where (x, y) = diffStr ds
consD ds d                 = DiffChunk [x]     [y]     d where (x, y) = diffStr ds

consD' :: DiffStr -> Diff -> Diff
consD' ds (DiffChunk l r d) = DiffChunk (x : l) (y : r) d where (x, y) = diffStr' ds
consD' ds d                 = DiffChunk [x]     [y]     d where (x, y) = diffStr' ds

-- | Render 'DiffStr' as two emphasised strings
diffStr :: DiffStr -> (EmphString, EmphString)
diffStr ES         = (Empty, Empty)
diffStr (SS s d)   = let (x, y) = diffStr d in (Norm s x, Norm s y)
diffStr (DS l r d) = let (x, y) = diffStr d in (Emph l x, Emph r y)

-- | Render 'DiffStr' as two emphasised strings, without emphasis
diffStr' :: DiffStr -> (EmphString, EmphString)
diffStr' ES         = (Empty, Empty)
diffStr' (SS s d)   = let (x, y) = diffStr' d in (Norm s x, Norm s y)
diffStr' (DS l r d) = let (x, y) = diffStr' d in (Norm l x, Norm r y)

-- | Line result
data LR = LR !Rational Diff
  deriving Show

instance Eq LR where
    LR n _ == LR m _ = n == m

instance Ord LR where
    compare (LR n _) (LR m _) = compare n m

-- |
--
-- >>> diffLines [Exact (map Exact "foo")] ["boo"]
--
-- >>> diffLines [Exact (map Exact "boo")] ["boo"]
--
diffLines :: [Wild [Wild Char]] -> [String] -> (Rational, Diff)
diffLines xs ys = case cached 0 0 of
    LR d ds -> (d, ds)
  where
    is = V.fromList xs
    js = V.fromList ys

    il = V.length is
    jl = V.length js

    cached :: Int -> Int -> LR
    cached i j = cachedV V.! (j + (jl + 1) * i)

    cachedV :: V.Vector LR
    cachedV = V.generate ((il + 1) * (jl + 1)) $ \ij -> case ij `divMod` (jl + 1) of
        (i, j)
            | i == il
            , j == jl
            -> LR 0 DiffEmpty

            | i == il
            , let LR d ds = cached i (j + 1)
            , let jc = js V.! j
            -> LR (d + 1) (consR jc ds)

            | j == jl
            , let LR d ds = cached (i + 1) j
            -> case is V.! i of
              Wildcard -> LR d ds
              Exact ic -> LR (d + 1) (consL (flatten ic) ds)

            | otherwise
            , let jc = js V.! j
            -> case is V.! i of
              Wildcard -> minimum
                  [ let LR d ds = cached i (j + 1) in LR d (consS jc ds)
                  , cached (i + 1) j
                  ]
              Exact ic -> case diffString ic jc of
                  (num, den, sr)
                      | num == 0  -> let LR d ds = cached (i + 1) (j + 1) in LR d (consS jc ds)
                      | otherwise -> minimum
                      [ let LR d ds = cached i       (j + 1) in LR (d + 1) (consR jc ds)
                      , let LR d ds = cached (i + 1) j       in LR (d + 1) (consL (flatten ic) ds)
                      , let LR d ds = cached (i + 1) (j + 1) in LR (d + r) $ if r < 0.5 then consD sr ds else consD' sr ds
                      ]
                    where
                      r = toInteger num Data.Ratio.% toInteger den

flatten :: [Wild Char] -> String
flatten []             = []
flatten (Wildcard : w) = flatten w
flatten (Exact c : w)  = c : flatten w
