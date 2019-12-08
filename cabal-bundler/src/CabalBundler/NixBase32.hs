module CabalBundler.NixBase32 (
    encodeBase32,
    ) where

import Peura

import Prelude (splitAt)

import Data.Word (Word8)
import Data.List (foldl')
import Data.Bits

import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString as BS

-- | Print 'BS.ByteString' in Nix favoured base32.
--
-- This implementation is not efficient, but it's obvious what happens.
-- Plenty of 'reverse' is due "Big-endianess" of nix variant.
encodeBase32 :: BS.ByteString -> String
encodeBase32
    = reverse
    . map (toChar . bitsToBits . reverse)
    . chunks 5
    . reverse
    . concatMap w8ToBits
    . reverse
    . BS.unpack
  where
    toChar :: Int -> Char
    toChar i = charsBase32 V.! i

charsBase32 :: V.Vector Char
charsBase32 = V.fromList "0123456789abcdfghijklmnpqrsvwxyz"

-------------------------------------------------------------------------------
-- C "reference" implementation
-------------------------------------------------------------------------------

-- 
-- 
-- static string printHash32(const Hash & hash)
-- {
--     assert(hash.hashSize);
--     size_t len = hash.base32Len();
--     assert(len);
-- 
--     string s;
--     s.reserve(len);
-- 
--     for (int n = (int) len - 1; n >= 0; n--) {
--         unsigned int b = n * 5;
--         unsigned int i = b / 8;
--         unsigned int j = b % 8;
--         unsigned char c =
--             (hash.hash[i] >> j)
--             | (i >= hash.hashSize - 1 ? 0 : hash.hash[i + 1] << (8 - j));
--         s.push_back(base32Chars[c & 0x1f]);
--     }
-- 
--     return s;
-- }

_encodeBase32 :: BS.ByteString -> String
_encodeBase32 bs
    | BS.length bs == 32 =
        [ digits32 V.! (idx .&. 0x1f)
        | n <- reverse [ 0 .. len - 1]
        , let b = n * 5
        , let (i, j) = b `divMod` 8
        , let idx = fromIntegral (BS.index bs i) `shiftR` j .|. if i >= 32 - 1 then 0 else fromIntegral (BS.index bs (succ i)) `shiftL` (8 - j)
        ]
    | otherwise = ""
  where
    len = 52 -- length of hash

    digits32 :: V.Vector Char
    digits32 = V.fromList "0123456789abcdfghijklmnpqrsvwxyz"


-------------------------------------------------------------------------------
-- Bit streams
-------------------------------------------------------------------------------

data Bit = B0 | B1
  deriving (Show)

w8ToBits :: Word8 -> [Bit]
w8ToBits w = map toBit
    [ w .&. 0x80
    , w .&. 0x40
    , w .&. 0x20
    , w .&. 0x10
    , w .&. 0x08
    , w .&. 0x04
    , w .&. 0x02
    , w .&. 0x01
    ]
  where
    toBit 0 = B0
    toBit _ = B1

bitsToBits :: Bits a => [Bit] -> a
bitsToBits = foldl' f zeroBits where
    f x B0 = x `shiftL` 1
    f x B1 = (x `shiftL` 1) `setBit` 0

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = ys : chunks n zs
  where
    (ys, zs) = splitAt n xs
