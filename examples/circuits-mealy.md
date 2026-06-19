# circuits-mealy ⟜ Traced Mealy Either, encode, scan triangle

**ref** ⟜ ~/haskell/circuits/ — Trace, Hyper, Traced
**ref** ⟜ ~/mg/examples/mealy.md — the isomorphism card (context)
**note** ⟜ Mealy is standalone. It does not depend on circuits. This card shows the bridge — what you get when you optionally encode a Mealy into Trace or Hyper. Nothing in this card compiles as-is; it requires both `circuits` and `mealy` in scope. Use `cabal repl -b circuits` from the mealy project root.

## Traced Mealy Either

The Either trace is what gives Mealy its feedback semantics. Initialization (`Right a`) is unconditionally productive — it creates state immediately. Feedback (`Left x`) iterates through `step` until `extract` produces `Right b`. The `settle` helper drains feedback internally so that after `inject'` or `step'`, the state is guaranteed settled.

```haskell
instance Traced Mealy Either where
  trace (Mealy inject step extract) = Mealy inject' step' extract'
    where
      inject' a = settle (inject (Right a))
      step' s a = settle (step s (Right a))
      extract' s = case extract s of
        Right b -> b
        Left _  -> error "Traced Mealy Either: unsettled state"
      settle s = case extract s of
        Left x  -> settle (step s (Left x))
        Right _ -> s

  untrace (Mealy z h k) = Mealy inject' step' extract'
    where
      inject' (Left x)       = Left x
      inject' (Right b)      = Right (z b)
      step' (Left x) _       = Left x
      step' (Right _s) (Left x)  = Left x
      step' (Right s) (Right b)  = Right (h s b)
      extract' (Left x)      = Left x
      extract' (Right s)     = Right (k s)
```

`untrace` lifts a plain Mealy into the Either tensor: `Right` values thread through the Mealy triple, `Left` values pass through untouched. This is exactly what `Traced` demands — the feedback channel is preserved while the payload is transformed.

## encode → Trace (list-shot)

Encode a Mealy as a single `Trace` that processes the entire list in one `reify`. State rides the feedback channel alongside the remaining input and accumulated output. Build-foldr fusion is preserved because the list structure passes through the Trace boundary unchanged.

```haskell
encode :: Mealy a b -> Trace Either (->) [a] [b]
encode (Mealy inject step extract) = Trace body
  where
    body (Right [])           = Right []
    body (Right (a : as))     = let s = inject a
                                 in Left (s, as, [extract s])
    body (Left (_, [], bs))   = Right (reverse bs)
    body (Left (s, a : as, bs)) =
      let s' = step s a
       in Left (s', as, extract s' : bs)
```

The feedback type is `(s, [a], [b])` — current Mealy state, remaining input, accumulated output (reversed for O(1) prepend).

### the triangle

```haskell
reify . encode = scan
```

`realise` uses `Traced (->) Either` — the while-loop — which iterates through the list element by element. The Either trace in `(->)` drives the loop; the Mealy triple runs each step internally. The result is `[a] -> [b]`, exactly `scan`.

Proof sketch: `realise (Trace body) [a0, a1, ...]` enters at `Right [a0, a1, ...]`, the body produces `Left (s0, [a1, ...], [b0])`, the type-trace feeds that back as `Left (...)`, and so on until the list is exhausted, at which point `Right (reverse bs)` exits. This is `scanl'` by construction.

## encodeElem → Hyper (pointwise)

The primal form: encode a Mealy as a self-referential Hyper that processes one element per invocation. No list in the type. Observation is pointwise.

```haskell
import Circuit.Hyper (Hyper (..), encodeEither, runEither)

encodeElem :: Mealy a b -> Hyper (Either a a -> b) (Either a a -> b)
encodeElem (Mealy inject step extract) = encodeEither body
  where
    body (Right a)       = Left a          -- first call: seed state, output input
    body (Left a)        = Right (extract s)  -- second call: extract from state
      where s = ...     -- state lives in Hyper's continuation, not here
```

This version needs care — state persistence across calls requires the continuation to carry it. The simpler route:

```haskell
-- Run a Mealy pointwise by closing the Hyper knot manually
-- This uses encodeEither's pattern: recurse on Left until Right
runMealy :: Mealy a b -> a -> b
runMealy (Mealy inject step extract) a =
  runEither (body a) (Right a)
  where
    body a0 s0 = case s0 of
      Right a -> Left (inject a)          -- initialize
      Left s  -> Right (extract s)        -- observe
```

But this loses state between calls. For persistent state, the pointwise form needs an external accumulator — which is what the list-shot `encode` provides naturally. The pointwise form is better seen as what the list-shot form reduces to internally: each element is one iteration of the Either trace.

## scan = scanl'

Mealy's scan is isomorphic to `Data.List.scanl'`. The isomorphism is direct:

```haskell
-- scanl' to Mealy
scanl'ToMealy :: (s -> a -> s) -> s -> Mealy a s
scanl'ToMealy step init = Mealy (const init) (const . step) id

-- Mealy to scanl'
mealyToScanl' :: Mealy a b -> [a] -> [b]
mealyToScanl' = scan
```

The Category instance preserves this: composing two Mealys is composing their step functions with the intermediate state threaded through. The producer-consumer identity holds.

## producer / consumer through circuits

```haskell
producer :: [a] -> Mealy () a
producer as = Mealy inject step extract
  where
    inject () = as
    step (_ : xs) () = xs
    step [] () = []
    extract (x : _) = x
    extract [] = error "producer: exhausted"

consumer :: Mealy a [a]
consumer = Mealy inject step extract
  where
    inject a = [a]
    step acc a = a : acc
    extract = reverse

-- Composition in Mealy land
-- producer [1,2,3] . consumer ≡ id :: Mealy () [Int]
-- fold (consumer . producer [1,2,3]) (replicate 3 ()) = [1,2,3]

-- The same composition, encoded into Trace
circuitPipeline :: Trace Either (->) [()] [[Int]]
circuitPipeline = encode consumer >>> encode (producer [1,2,3])

-- realise circuitPipeline [(), (), ()] = [[1,2,3]]
-- This is scan (consumer . producer [1,2,3]) [(), (), ()]
```

The list boundary carries through. The circuit is an optional structural layer — you get the same result whether you compose in Mealy directly or encode first then compose in Trace.

## why Either, why not (,)

A `(,)` trace requires feedback on the first call, before any output exists:

```haskell
-- Hypothetical (,) trace — diverges for most Mealys
inject' a = let s = inject (x, a); x = fst (extract s) in s
```

If `inject` inspects `x` (which any realistic Mealy does — it needs to create state), `mfix` diverges. The lazy knot ties `x` to `extract (inject (x, a))`, and `inject` can't produce state without `x`.

`Either` sidesteps this: `Right a` carries no feedback, so `inject (Right a)` is productive. Feedback only enters on subsequent `Left` iterations, after state exists. The phases don't overlap.

This is why Mealy pairs naturally with `Either` as a Trace tensor but not with `(,)`. The mealy.md card in mg/examples/ has the full derivation.

## open questions

1. **The `encode` name.** `realise`, `encode`, `run`, `lower` are all interpreters — they close a delayed structure. A single class `Encode arr t code` where `encode :: Trace t arr a b -> code a b` would unify them. `encode @(->) @Either @Mealy` would be the identity. `encode @Mealy @Either @(Trace Either (->))` would be the list-shot encoding shown here. The triangle `realise . encode = scan` becomes `lower . encode . encode = scan` which is ugly — the class needs careful design.

2. **Where does the Traced instance live?** Mealy is standalone and doesn't depend on circuits. `Traced Mealy Either` needs `Traced` from `Circuit.Traced`. This creates a dependency in one direction. Options: (a) provide the instance in circuits (but circuits doesn't depend on mealy), (b) provide it in a bridge package, (c) keep it as example-only code in this card. Currently it's (c) — the instance is shown here but doesn't live in either library.

3. **Pointwise encoding.** The list-shot `encode` handles the list externally. A pointwise `encode` that gives `Trace Either (->) a b` (element-at-a-time, state hidden in the Trace) would be cleaner for circuit composition but requires the caller to drive input one element per `realise` call. Which pattern is more useful depends on whether you're composing with other circuits or just observing the Mealy.
