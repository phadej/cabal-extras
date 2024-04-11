{-# OPTIONS_GHC -ddump-simpl -dsuppress-all #-}
module Example where

import Data.ByteString (ByteString)
import GHC.Generics    (Generic)

import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE

import GenericEq

-------------------------------------------------------------------------------
-- text
-------------------------------------------------------------------------------

countChars :: ByteString -> Int
countChars = T.length . T.toUpper . TE.decodeUtf8
{-# INLINE countChars #-}


-------------------------------------------------------------------------------
-- Generic equality
-------------------------------------------------------------------------------

data T where
    T1 :: Int -> Char -> T
    T2 :: Bool -> Double -> T
    T3 :: ByteString -> T.Text -> T
  deriving Generic

{-
data T where
    T1 :: Int -> T
    T2 :: Bool -> T
    T3 :: Char -> T
    T4 :: Double -> T
  deriving Generic
-}

instance Eq T where
    (==) = genericEq
