{-# LANGUAGE GADTs #-}

-- | Producer / Consumer patterns as Mealy machines.
--
-- A producer unfolds a list into a stream of outputs.
-- A consumer folds a stream of inputs into a summary.
--
-- Both are Mealy machines; the symmetry is in the state.
module ProducerConsumer where

import Data.Mealy

-- | Producer: unfold a list into a stream of values.
--
-- >>> fold (producer [1,2,3]) [(),(),()]
-- [1,2,3]
--
-- State: remaining list.  `inject` loads it; `step` pops the head;
-- `extract` reads the head.
producer :: [a] -> Mealy () a
producer as = Mealy inject step extract
  where
    inject () = as
    step (_ : xs) () = xs
    step [] () = []
    extract (x : _) = x
    extract [] = error "producer: exhausted"

-- | Consumer: fold a stream into a list.
--
-- >>> fold consumer [1,2,3]
-- [1,2,3]
--
-- State: accumulated list (reversed for O(1) prepend).
-- `inject` seeds with first element; `step` prepends;
-- `extract` reverses to restore order.
consumer :: Mealy a [a]
consumer = Mealy inject step extract
  where
    inject a = [a]
    step acc a = a : acc
    extract = reverse

-- | Running sum.
--
-- >>> fold summer [1,2,3,4]
-- 10
summer :: (Num a) => Mealy a a
summer = Mealy inject step extract
  where
    inject a = a
    step acc a = acc + a
    extract = id

-- | Last element.
--
-- >>> fold lastM [1,2,3,4]
-- 4
lastM :: Mealy a a
lastM = Mealy inject step extract
  where
    inject a = a
    step _ a = a
    extract = id

-- | Producer and consumer compose to the identity.
--
-- >>> fold (consumer . producer [1,2,3]) [(),(),()]
-- [1,2,3]
producerConsumerIdentity :: [Int] -> [Int]
producerConsumerIdentity xs = fold (consumer . producer xs) (replicate (length xs) ())

-- | Circuit encoding: a Mealy producer is a single Knot in
-- Circuit (->) Either [()], and a consumer is a Knot in
-- Circuit (->) Either [a].
--
-- See examples/mealy.md for the full isomorphism.
