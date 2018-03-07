{-# LANGUAGE UnicodeSyntax, NoMonomorphismRestriction, TypeFamilies, FlexibleContexts, ScopedTypeVariables, RankNTypes, TypeInType, TupleSections, MagicHash, UnboxedTuples, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Main where

import Criterion
import Criterion.Main
import qualified Data.Vector as V
import Data.Array.Repa (Z(..), (:.)(..), extent, sumAllS)
import Numeric.Series.Chebychev

setupEnv :: Monad m => m (Int, Int)
setupEnv = do
  let small = 20
  let big = 500
  return (small, big)

main :: IO ()
main = defaultMain [
  env setupEnv $ \ ~(small,big) -> bgroup "Chebychev" [
      bgroup "small, exp()" [
          bench "length and sum of ChebychevR series" $ nfIO
            (do
                let n = small
                fit ← chebyshevFitR exp 0 1 n
                let (Z:.len) = extent fit
                return (len, sumAllS fit))
          , bench "length and sum of Chebychev series" $ nf
            (\n → let fit ∷ V.Vector Double
                      fit = chebyshevFit exp 0 1 n
                  in (length fit, sum fit)) small
          ]
      ,  bgroup "big, exp()" [
          bench "length and sum of ChebychevR series" $ nfIO
            (do
                let n = big
                fit ← chebyshevFitR exp 0 1 n
                let (Z:.len) = extent fit
                return (len, sumAllS fit))
          , bench "length and sum of Chebychev series" $ nf
            (\n → let fit ∷ V.Vector Double
                      fit = chebyshevFit exp 0 1 n
                  in (length fit, sum fit)) big
          ]
      ]
  ]
