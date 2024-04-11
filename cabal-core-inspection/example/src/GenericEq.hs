module GenericEq (genericEq) where

import GHC.Generics

genericEq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
genericEq = \ x y -> geq (from x) (from y)
{-# INLINE genericEq #-}

class GEq f where
    geq :: f a -> f a -> Bool

instance (GEqSum f, i ~ D) => GEq (M1 i c f) where
    geq (M1 x) (M1 y) = geqSum x y
    {-# INLINE geq #-}

class GEqSum f where
    geqSum :: f a -> f a -> Bool

instance (GEqSum f, GEqSum g) => GEqSum (f :+: g) where
    geqSum (L1 x) (L1 y) = geqSum x y
    geqSum (R1 x) (R1 y) = geqSum x y
    geqSum _      _      = False
    {-# INLINE geqSum #-}

instance (GEqProduct f, i ~ C) => GEqSum (M1 i c f) where
    geqSum (M1 x) (M1 y) = geqProduct x y
    {-# INLINE geqSum #-}

class GEqProduct f where
    geqProduct :: f a -> f a -> Bool

instance (GEqProduct f, GEqProduct g) => GEqProduct (f :*: g) where
    geqProduct (x1 :*: y1) (x2 :*: y2) =
        geqProduct x1 x2 && geqProduct y1 y2
    {-# INLINE geqProduct #-}

instance (GEqField f, i ~ S) => GEqProduct (M1 i c f) where
    geqProduct (M1 x) (M1 y) = geqField x y
    {-# INLINE geqProduct #-}

class GEqField f where
    geqField :: f a -> f a -> Bool

instance (Eq a, i ~ R) => GEqField (K1 i a) where
    geqField (K1 x) (K1 y) = x == y
    {-# INLINE geqField #-}
