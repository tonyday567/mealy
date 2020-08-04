{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Data.Mealy.Simulate
  ( rvs,
    rvsp,
    create,
  )
where

import Control.Monad.Primitive (PrimState)
import NumHask.Prelude hiding (fold)
import System.Random.MWC
import System.Random.MWC.Probability hiding (beta)

-- $setup
-- >>> :set -XFlexibleContexts
-- >>> import Data.Mealy
-- >>> gen <- create
-- >>> let n = 3
-- >>> let eq' a b = all nearZero $ zipWith (-) a b
-- >>> let eq'p a b = all (\x -> x) $ zipWith (\(x0,x1) (y0,y1) -> nearZero (x0-y0) && nearZero (x1-y1)) a b

-- | rvs creates a list of standard normal random variates.
-- >>> t <- rvs gen n
-- >>> t `eq'` [-0.8077385934202513,-1.3423948150518445,-0.4900206084002882]
-- True
--
-- >>> rs <- rvs gen 10000
-- >>> fold (ma 1) rs
-- -1.735737734197327e-3
--
-- >>> fold (std 1) rs
-- 0.9923615647768976
rvs :: Gen (PrimState IO) -> Int -> IO [Double]
rvs gen n = samples n standardNormal gen

-- | rvsPair generates a list of correlated random variate tuples
-- |
-- >>> t <- rvsp gen 3 0.8
-- >>> t `eq'p` [(-0.8077385934202513,-1.4591410449385904),(-1.3423948150518445,-0.6046212701237168),(-0.4900206084002882,0.923007518547542)]
-- True
--
-- >>> rsp <- rvsp gen 10000 0.8
-- >>> fold (corr (ma 1) (std 1)) rsp
-- 0.7933213647252008
rvsp :: Gen (PrimState IO) -> Int -> Double -> IO [(Double, Double)]
rvsp gen n c = do
  s0 <- rvs gen n
  s1 <- rvs gen n
  let s1' = zipWith (\x y -> c * x + sqrt (1 - c * c) * y) s0 s1
  pure $ zip s0 s1'
