{-# LANGUAGE UnicodeSyntax, NoMonomorphismRestriction, TypeFamilies, FlexibleContexts, ScopedTypeVariables, RankNTypes, TypeInType, TupleSections, MagicHash, UnboxedTuples, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Main where

import Control.Monad.ST
import Control.Monad.Trans.State.Lazy
import Data.List
import Data.STRef
import Diagrams.Prelude
import Graphics.Rendering.Chart.Backend.Diagrams (toFile, renderableToFile)
import Graphics.Rendering.Chart.Easy
import Numeric.ODE.RK4

main :: IO ()
main = do
  toFile def "sin.svg" plotSin
  _ ← renderableToFile def "fp.svg" plotFixPoints
  return ()

plotSin :: StateT (Layout Double Double) (State CState) ()
plotSin = do
  layout_title .= "ODE y' = cos x with x0=0, y0=0 giving y = sin x"
  setColors [opaque blue, opaque red]
  plot (line "am" [ps])
  where
    ps = rkDumb1 0.0 (0.0, pi) (const . cos) 100

plotFixPoints :: Graphics.Rendering.Chart.Easy.Renderable ()
plotFixPoints = toRenderable layout
  where
    layout = layout_title .~ "Fix points of the logistic equation"
           $ layout_plots .~ [toPlot bifurcations]
           $ def
    bifurcations = plot_points_style .~ filledCircles (0.5) (opaque red)
                   $ plot_points_values .~ ps
                   $ plot_points_title .~ "Parameter r of the logistic function"
                   $ def
    rs = [0,(0.01)..4]
    ps = concatMap fp rs
    fp ∷ Double -> [(Double, Double)]
    fp r = let xs = (take 1000) . (drop 1000) $ iter (logistic r) 0.5
               zs = fixPoints 1e-10 xs
           in fmap (r,) zs

iter :: (a -> a) -> a -> [a]
iter f a = let a' = f a
           in a:(iter f a')

limit ∷ Double → [Double] → Double
limit eps xs = let ds = zipWith (-) xs (tail xs)
                   zs = zip ds xs
               in snd . head $ dropWhile (\(d,_) → d>eps) zs

fixPoints ∷ Double → [Double] → [Double]
fixPoints eps xs = nubBy (\x1 x2 → abs (x1 - x2) < eps) xs

iterFixPoints :: Int -> Int -> Double -> Double -> (Double -> Double) -> [Double]
iterFixPoints n m x0 eps f = let xs = (take n) . (drop m) $ iter f x0
                             in fixPoints eps (sort xs)

logistic ∷ Double → Double → Double
logistic r x = r * x * (1 - x)

logisticFixPoints :: Double -> [Double]
logisticFixPoints r = iterFixPoints 100 1000 0.5 1e-10 (logistic r)

iβ ∷ Int
iβ = runST $ do
  let one ∷ Double
      one = 1.0
      --two = one + one
      zero = one - one
  a <- newSTRef one
  let loop1 = do
        modifySTRef a (\x → x+x)
        a' <- readSTRef a
        let temp = a' + one
            temp1 = temp - a'
        if (temp1 - one == zero)
          then loop1
          else (return a')
  a' ← loop1
  b ← newSTRef one
  let loop2 = do
        modifySTRef b (\x → x+x)
        b' <- readSTRef b
        let temp = a' + b'
            itemp = floor (temp - a')
        if (itemp == 0)
          then loop2
          else (return itemp)
  itemp ← loop2
  return itemp
