{-# LANGUAGE DataKinds #-}

module Data.Quantiles
  ( tDigest,
    tDigestQuantiles,
    tDigestHist,
    OnlineTDigest (..),
    onlineQuantiles,
    Data.Quantiles.median,
    onlineDigitize,
    onlineDigestHist,
  )
where

import qualified Control.Foldl as L
import Control.Monad.ST (runST)
import Data.Foldable
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import Data.Ord
import Data.TDigest
import Data.TDigest.Internal
import Data.TDigest.Postprocess (HistBin, histogram)
import Data.TDigest.Tree.Internal (TDigest (..), absMaxSize, emptyTDigest, insertCentroid, relMaxSize, size, toMVector)
import qualified Data.Vector.Algorithms.Heap as VHeap
import qualified Data.Vector.Unboxed as VU
import Prelude

-- | a raw non-online tdigest fold
tDigest :: L.Fold Double (TDigest 25)
tDigest = L.Fold step begin done
  where
    step x a = insert a x
    begin = tdigest ([] :: [Double]) :: TDigest 25
    done = id

-- | non-online version
tDigestQuantiles :: [Double] -> L.Fold Double [Double]
tDigestQuantiles qs = L.Fold step begin done
  where
    step x a = insert a x
    begin = tdigest ([] :: [Double]) :: TDigest 25
    done x = fromMaybe (0 / 0) . (`quantile` compress x) <$> qs

-- | non-online version
tDigestHist :: L.Fold Double (Maybe (NonEmpty HistBin))
tDigestHist = L.Fold step begin done
  where
    step x a = insert a x
    begin = tdigest ([] :: [Double]) :: TDigest 25
    done = histogram . compress

data OnlineTDigest
  = OnlineTDigest
      { td :: TDigest 25,
        tdN :: Int,
        tdRate :: Double
      }
  deriving (Show)

emptyOnlineTDigest :: Double -> OnlineTDigest
emptyOnlineTDigest = OnlineTDigest (emptyTDigest :: TDigest n) 0

-- | decaying quantiles based on the tdigest library
onlineQuantiles :: Double -> [Double] -> L.Fold Double [Double]
onlineQuantiles r qs = L.Fold step begin done
  where
    step x a = onlineInsert a x
    begin = emptyOnlineTDigest r
    done x = fromMaybe (0 / 0) . (`quantile` t) <$> qs
      where
        (OnlineTDigest t _ _) = onlineForceCompress x

median :: Double -> L.Fold Double Double
median r = L.Fold step begin done
  where
    step x a = onlineInsert a x
    begin = emptyOnlineTDigest r
    done x = fromMaybe (0 / 0) (quantile 0.5 t)
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

onlineDigitize :: Double -> [Double] -> L.Fold Double Int
onlineDigitize r qs = L.Fold step begin done
  where
    step (x, _) a = (onlineInsert a x, a)
    begin = (emptyOnlineTDigest r, 0 / 0)
    done (x, l) = bucket' qs' l
      where
        qs' = fromMaybe (0 / 0) . (`quantile` t) <$> qs
        (OnlineTDigest t _ _) = onlineForceCompress x
        bucket' xs l' =
          L.fold L.sum $
            ( \x' ->
                if x' > l'
                  then 0
                  else 1
            )
              <$> xs

-- | decaying histogram based on the tdigest library
onlineDigestHist :: Double -> L.Fold Double (Maybe (NonEmpty HistBin))
onlineDigestHist r = L.Fold step begin done
  where
    step x a = onlineInsert a x
    begin = emptyOnlineTDigest r
    done x = histogram . compress $ t
      where
        (OnlineTDigest t _ _) = onlineForceCompress x
