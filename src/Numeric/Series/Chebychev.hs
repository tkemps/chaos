{-# LANGUAGE UnicodeSyntax, NoMonomorphismRestriction, TypeFamilies, FlexibleContexts, ScopedTypeVariables, RankNTypes, TypeInType, TupleSections, MagicHash, UnboxedTuples, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Numeric.Series.Chebychev where

import Data.Vector.Generic ((!))
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import qualified Data.Array.Repa as R
import Data.Array.Repa (Array(..), U, Z(..), (:.)(..), DIM1,
                        computeUnboxedP, computeUnboxedS, fromFunction, sumAllS)

-- | This function computes the n coefficients of the approximating Chebyshev
--  polynomial given a function func to be approximated, lower and upper limits
--  of the interval [a, b] over which to approximate func and the maximum degree 
--  n of the Chebyshev polynomial.  This routine was converted from chebft, 
--  p. 192, Numerical Recipes in C
chebyshevFitR :: (Monad m) ⇒ (Double -> Double) -> Double -> Double -> Int -> m (Array U DIM1 Double)
{-# INLINE chebyshevFitR #-}
chebyshevFitR f !a !b !n = do
  let comp = if (n>50) then computeUnboxedP else return . computeUnboxedS
  fs ← comp $ fromFunction (Z:.n) (
    \(Z :. k) → let !k' = fromIntegral k
                in f (cos(pin*(k'+0.5))*bma+bpa))
  fit ← comp $ fromFunction (Z:.n)
                (\(Z:.j) → let !j' = (fromIntegral j)
                               !pijn = pi*j'/n'
                           in z * sumAllS (fromFunction (Z :. n)
                                           (\(Z:.k) → let k' = fromIntegral k
                                                        in fs R.!(Z:.k) * cos(pijn*(k'+0.5)))))
  return fit

  where
    n' = (fromIntegral n)
    pin = pi/n'
    z = 2.0/n'
    bma = 0.5*(b-a)
    bpa = 0.5*(b+a)

chebyshevFit :: (VG.Vector v Double) =>
                 (Double -> Double) -> Double -> Double -> Int -> v Double
{-# INLINE chebyshevFit #-}
chebyshevFit f !a !b !n =
  VG.generate n (\j → let j' = (fromIntegral j)
                          pijn = pi*j'/n'
                      in z * sum [let k' = fromIntegral k
                                  in fs!k * cos(pijn*(k'+0.5))
                                 | k ← [0..(n-1)]])

  where
    n' = (fromIntegral n)
    pin = pi/n'
    z = 2.0/n'
    bma = 0.5*(b-a)
    bpa = 0.5*(b+a)
--    fs ∷ VU.Vector Double
    fs = VU.generate n (\k → let k' = fromIntegral k
                             in f (cos(pin*(k'+0.5))*bma+bpa))
