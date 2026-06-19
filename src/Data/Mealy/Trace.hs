{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports -Wno-pattern-namespace-specifier #-}

-- | A cartesian 'Traced' instance for 'Mealy'.
--
-- This is the experiment from the discussion of Hasegawa-style shared
-- gradients: a 'Mealy' can be given a @Traced Mealy (,)@ instance by tying a
-- lazy knot at each step, analogous to the @MonadFix@ trace for @Kleisli m@.
--
-- The instance is lawful for morphisms that are productive / non-strict in the
-- feedback channel.  It passes the usual yanking check:
--
-- >>> scan (trace swapMealy) [1,2,3 :: Int]
-- [1,2,3]
--
-- However, it diverges for strict numeric accumulators such as the moving
-- average, because the per-step fixed point @a_t = r * a_t + x_t@ has no lazy
-- solution.  That is exactly why 'Data.Mealy.Diff.DiffMealy' uses a reverse
-- step instead.
module Data.Mealy.Trace
  ( -- * Traced instance
    swapMealy,
  )
where

import Circuit.Traced (Traced (..))
import Data.Bifunctor (second)
import Data.Mealy (Mealy (..), scan, pattern M)
import NumHask.Prelude hiding (id)
import Prelude (id)

-- $setup
-- >>> import Circuit.Traced (trace, untrace)
-- >>> import Data.Mealy (Mealy (..), scan)

-- | A stateless swap mealy, useful for checking the yanking law.
swapMealy :: Mealy (a, b) (b, a)
swapMealy = M swap (const swap) id
  where
    swap (x, y) = (y, x)

instance Traced Mealy (,) where
  trace (M inject step extract) =
    M
      (\b -> let s0 = inject (a0, b); a0 = fst (extract s0) in s0)
      ( \s b ->
          let (s', _a) = fix (\ ~(s'', a') -> (step s (a', b), fst (extract s'')))
           in s'
      )
      (snd . extract)

  untrace (M inject step extract) =
    M
      (second inject)
      (\(_a0, s) (a, b) -> (a, step s b))
      (second extract)
