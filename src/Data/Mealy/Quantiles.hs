{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE StrictData #-}

module Data.Mealy.Quantiles
  ( median,
    quantiles,
    digitize,
  )
where

import Data.Mealy
import Data.Ord
import Data.TDigest hiding (median)
import Data.TDigest.Internal
import Data.TDigest.Tree.Internal (TDigest (..), absMaxSize, emptyTDigest, insertCentroid, relMaxSize, size, toMVector)
import qualified Data.Vector.Algorithms.Heap as VHeap
import qualified Data.Vector.Unboxed as VU
import NumHask.Prelude hiding (fold)
import Control.Monad.ST

data OnlineTDigest = OnlineTDigest
  { td :: TDigest 25,
    tdN :: Int,
    tdRate :: Double
  }
  deriving (Show)

emptyOnlineTDigest :: Double -> OnlineTDigest
emptyOnlineTDigest = OnlineTDigest (emptyTDigest :: TDigest n) 0

-- | decaying quantiles based on the tdigest library
quantiles :: Double -> [Double] -> Mealy Double [Double]
quantiles r qs = M inject step extract
  where
    step x a = onlineInsert a x
    inject a = onlineInsert a (emptyOnlineTDigest r)
    extract x = fromMaybe (0 / 0) . (`quantile` t) <$> qs
      where
        (OnlineTDigest t _ _) = onlineForceCompress x

median :: Double -> Mealy Double Double
median r = M inject step extract
  where
    step x a = onlineInsert a x
    inject a = onlineInsert a (emptyOnlineTDigest r)
    extract x = fromMaybe (0 / 0) (quantile 0.5 t)
      where
        (OnlineTDigest t _ _) = onlineForceCompress x

onlineInsert' :: Double -> OnlineTDigest -> OnlineTDigest
onlineInsert' x (OnlineTDigest td' n r) =
  OnlineTDigest
    (insertCentroid (x, r ^^ (- (fromIntegral $ n + 1) :: Integer)) td')
    (n + 1)
    r

onlineInsert :: Double -> OnlineTDigest -> OnlineTDigest
onlineInsert x otd = onlineCompress (onlineInsert' x otd)

onlineCompress :: OnlineTDigest -> OnlineTDigest
onlineCompress otd@(OnlineTDigest Nil _ _) = otd
onlineCompress otd@(OnlineTDigest t _ _)
  | Data.TDigest.Tree.Internal.size t > relMaxSize * compression
      && Data.TDigest.Tree.Internal.size t > absMaxSize =
    onlineForceCompress otd
  | otherwise = otd
  where
    compression = 25

onlineForceCompress :: OnlineTDigest -> OnlineTDigest
onlineForceCompress otd@(OnlineTDigest Nil _ _) = otd
onlineForceCompress (OnlineTDigest t n r) = OnlineTDigest t' 0 r
  where
    t' =
      foldl' (flip insertCentroid) emptyTDigest $
        (\(m, w) -> (m, w * (r ^^ n))) . fst <$> VU.toList centroids
    -- Centroids are shuffled based on space
    centroids :: VU.Vector (Centroid, Double)
    centroids =
      runST $ do
        v <- toMVector t
        -- sort by cumulative weight
        VHeap.sortBy (comparing snd) v
        VU.unsafeFreeze v

digitize :: Double -> [Double] -> Mealy Double Int
digitize r qs = M inject step extract
  where
    step (x, _) a = (onlineInsert a x, a)
    inject a = (onlineInsert a (emptyOnlineTDigest r), a)
    extract (x, l) = bucket' qs' l
      where
        qs' = fromMaybe (0 / 0) . (`quantile` t) <$> qs
        (OnlineTDigest t _ _) = onlineForceCompress x
        bucket' xs l' =
          fold (M id (+) id) $
            ( \x' ->
                if x' > l'
                  then 0
                  else 1
            )
              <$> xs
