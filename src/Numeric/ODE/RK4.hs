{-# LANGUAGE UnicodeSyntax, NoMonomorphismRestriction, TypeFamilies, FlexibleContexts, ScopedTypeVariables, RankNTypes, TypeInType, TupleSections, MagicHash, UnboxedTuples, BangPatterns #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Numeric.ODE.RK4 where

import Control.Exception
--import qualified Data.Vector.Unboxed as VU
--import Data.Vector.Generic ((!))
import qualified Data.Vector.Generic as VG

rk4 ∷ (VG.Vector v Double,
       VG.Vector v (Double, Double),
       VG.Vector v (Double, Double, Double, Double)) ⇒
  v Double                           -- ^ starting point y ∈ R^n
  → v Double                         -- ^ derivative dy/dx = f(x,y) ∈ R^n at starting point (x, y)
  → Double                           -- ^ starting point x ∈ R
  → Double                           -- ^ stepsize h ∈ R
  → (Double → v Double → v Double)   -- ^ derivative f(x, y) = dy/dx ∈ R^n
  → v Double                         -- ^ advanced vector y1 ∈ R^n
{-# INLINE rk4 #-}
rk4 y y' x h f = {-# SCC "rk4" #-}
  let yt1 = VG.zipWith (\y dydx → y + 0.5*h*dydx) y y'
      dyt = f (x + 0.5*h) yt1
      yt2 = VG.zipWith (\y dydx → y + 0.5*h*dydx) y dyt
      dym1 = f (x + 0.5*h) yt2
      yt3 = VG.zipWith (\y dydx → y + h*dydx) y dym1
      dym2 = VG.zipWith (+) dym1 dyt
      dyt2 = f (x+h) yt3
  in VG.zipWith4 (\y y' dyt dym → y + h/6.0*(y' + dyt + 2*dym)) y y' dyt2 dym2

rk41 :: Double -> Double -> Double -> Double -> (Double -> Double -> Double) -> Double
{-# INLINE rk41 #-}
rk41 !y !y' !x !h f = {-# SCC "rk41" #-}
  let dyt = f (x + 0.5*h) (y + 0.5*h*y')
      dym1 = f (x + 0.5*h) (y + 0.5*h*dyt)
      dyt2 = f (x+h) (y + h*dym1)
  in y + h/6.0*(y' + dyt2 + 2*(dym1 + dyt))

data StepsizeTooSmall = StepsizeTooSmall deriving Show

instance Exception StepsizeTooSmall

rkDumb ∷ ∀ v . (VG.Vector v Double,
                VG.Vector v (Double, Double),
                VG.Vector v (Double, Double, Double, Double)) ⇒
  v Double
  → (Double, Double)
  → (Double → v Double → v Double)
  → Int
  → [(Double, v Double)]
rkDumb y0 (x0, x1) f nStep = rkIter nStep x0 y0
  where
    h = (x1-x0)/(fromIntegral nStep)
    
    rkIter ∷ Int → Double → v Double → [(Double, v Double)]
    rkIter k x v | k>0 = let (x', v') = rkStep x v
                         in (x, v) : (rkIter (k-1) x' v')
                 | otherwise = [(x, v)]

    rkStep ∷ Double → v Double → (Double, v Double)
    rkStep x v =
      let dv = f x v
          v' = rk4 v dv x h f
          x' = x + h
      in if x==x' then throw StepsizeTooSmall else (x', v')

rkDumb1 :: Double
        -> (Double, Double)
        -> (Double -> Double -> Double)
        -> Int
        -> [(Double, Double)]
rkDumb1 y0 (x0, x1) f nStep = rkIter nStep x0 y0
  where
    h = (x1-x0)/(fromIntegral nStep)
    
    rkIter ∷ Int → Double → Double → [(Double, Double)]
    rkIter k x v | k>0 = let (x', v') = rkStep x v
                         in (x, v) : (rkIter (k-1) x' v')
                 | otherwise = [(x, v)]

    rkStep ∷ Double → Double → (Double, Double)
    rkStep x v =
      let dv = f x v
          v' = rk41 v dv x h f
          x' = x + h
      in if x==x' then throw StepsizeTooSmall else (x', v')
