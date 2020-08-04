mealy
===

[![Build Status](https://travis-ci.org/tonyday567/mealy.svg)](https://travis-ci.org/tonyday567/mealy) [![Hackage](https://img.shields.io/hackage/v/mealy.svg)](https://hackage.haskell.org/package/mealy) [![lts](https://www.stackage.org/package/mealy/badge/lts)](http://stackage.org/lts/package/mealy) [![nightly](https://www.stackage.org/package/mealy/badge/nightly)](http://stackage.org/nightly/package/mealy) 

``` haskell
{- | A 'Mealy' is a triple of functions

 * (a -> b) __inject__ Convert an input into the state type.
 * (b -> a -> b) __step__ Update state given prior state and (new) input.
 * (c -> b) __extract__ Convert state to the output type.

 By adopting this order, a Mealy sum looks like:

> M id (+) id

where the first id is the initial injection to a contravariant position, and the second id is the covriant extraction.

 __inject__ kicks off state on the initial element of the Foldable, but is otherwise be independent of __step__.

> scan (M e s i) (x : xs) = e <$> scanl' s (i x) xs

-}
newtype Mealy a b = Mealy {l1 :: L1 a b}
  deriving (Profunctor, Category) via L1
  deriving (Functor, Applicative) via L1 a

-- | Pattern for a 'Mealy'.
--
-- @M extract step inject@
pattern M :: (a -> c) -> (c -> a -> c) -> (c -> b) -> Mealy a b
pattern M i s e = Mealy (L1 e s i)

{-# COMPLETE M #-}

-- | Fold a list through a 'Mealy'.
--
-- > cosieve == fold
fold :: Mealy a b -> [a] -> b
fold _ [] = panic "on the streets of Birmingham."
fold (M i s e) (x : xs) = e $ foldl' s (i x) xs

-- | Run a list through a 'Mealy' and return a list of values for every step
--
-- > length (scan _ xs) == length xs
scan :: Mealy a b -> [a] -> [b]
scan _ [] = []
scan (M i s e) (x : xs) = fromList (e <$> scanl' s (i x) xs)

```
