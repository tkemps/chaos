{-# LANGUAGE UnicodeSyntax, NoMonomorphismRestriction, TypeFamilies, FlexibleContexts, ScopedTypeVariables, RankNTypes, TypeInType, TupleSections, MagicHash, UnboxedTuples, BangPatterns, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Main where

import Control.Monad
import Criterion
import Criterion.Main
import Criterion.Main.Options
import Criterion.Types
import Data.Array.Repa (Array, D, Z(..), (!), DIM1, (:.)(..), sumAllS, computeUnboxedP)
import Numeric.ODE.CKRK
import Numeric.ODE.RK4
import System.IO.Unsafe (unsafePerformIO)

setupEnv :: Monad m => m (Int, Int)
setupEnv = do
  let small = 40
  let big = 1000
  return (small, big)

f ∷ Double → Double → Double
{-# INLINE f #-}
f !x !y = 7*y^(2∷Int)*x^(3∷Int)

main :: IO ()
main = defaultMainWith
  defaultConfig {
    resamples = 1000,
    reportFile = Just "bench-ODE.html",
    verbosity = Verbose}
  [
  env setupEnv $ \ ~(small,big) -> bgroup "ODE" [
      bgroup "small, y'=7*y^0.5*x^3, y(2)=3" [
          bench "RK4+rkDumb1" $ nf
            (\n → let sol = rkDumb1 3.0 (2.0, 5.0) f n
                  in  snd $ last sol) small
          , bench "CKRK+odeint" $ nf
            (\n → let x1 = 2.0
                      x2 = 5.0
                      sol = odeint (singleton 3.0)
                            (x1, x2)
                            (\x y → singleton (cos x))
--                            (\x y → let y0 = y!(Z:.0)
--                                    in singleton (7*y0**0.5*x^(3∷Int)))
                            ((x2-x1)/fromIntegral n)
                            0
                            (1e-8/fromIntegral n)
                      sumAbsErr = sum (map (\(x1, y1, _, _) → abs $ (sin x1) - (sumAllS y1)) sol)
                  in sumAbsErr) small
          , bench "CKRK+odeint1" $ nf
            (\n → let sol = odeint1 3.0
                            (2.0, 5.0)
                            (\x y → 7*y**0.5*x^(3∷Int))
                            (1.0/fromIntegral n)
                            0
                            (1e-10/fromIntegral n)
                      sumAbsErr = sum (map (\(x1, y1, _, _) → abs $ (sin x1) - y1) sol)
                  in sumAbsErr) small]
      ,  bgroup "big, y'=7*y^0.5*x^3, y(2)=3" [
          bench "RK4+rkDumb1" $ nf
            (\n → let f ∷ Double → Double → Double
                      f x y = 7*y**0.5*x^(3∷Int)
                      sol = rkDumb1 3.0 (2.0, 5.0) f n
                  in  snd $ last sol) big
          , bench "CKRK+odeint" $ nf
            (\n → let sol = odeint (singleton 3.0)
                            (2.0, 5.0)
                            (\x y → let y0 = y!(Z:.0)
                                    in singleton (7*y0**0.5*x^(3∷Int)))
                            (1.0/fromIntegral n)
                            0
                            (1e-10/fromIntegral n)
                      sumAbsErr = sum (map (\(x1, y1, _, _) → abs $ (sin x1) - (sumAllS y1)) sol)
                  in sumAbsErr) big
          , bench "CKRK+odeint1" $ nf
            (\n → let sol = odeint1 3.0
                            (2.0, 5.0)
                            (\x y → 7*y**0.5*x^(3∷Int))
                            (1.0/fromIntegral n)
                            0
                            (1e-10/fromIntegral n)
                      sumAbsErr = sum (map (\(x1, y1, _, _) → abs $ (sin x1) - y1) sol)
                  in sumAbsErr) big]
      ]
  ]
