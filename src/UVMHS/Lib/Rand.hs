module UVMHS.Lib.Rand where

import UVMHS.Core

import System.Random as R


--------
-- RG --
--------

class MonadRand m where
  rng ∷ State RG a → m a

newtype RG = RG { unRG ∷ R.StdGen }

instance MonadRand IO where
  rng f = R.getStdRandom $ \ ℊ →
    let RG ℊ' :* x = runState (RG ℊ) f
    in (x,ℊ')

rngSeed ∷ ℕ64 → IO ()
rngSeed = R.setStdGen ∘ R.mkStdGen ∘ tohs ∘ frBitsℤ64

wrapPrimRandu ∷ (R.StdGen → (a,R.StdGen)) → State RG a
wrapPrimRandu f = do
  RG ℊ ← get
  let (x,ℊ') = f ℊ
  put $ RG ℊ'
  return x

wrapPrimRandr ∷ ((a,a) → R.StdGen → (a,R.StdGen)) → a → a → State RG a
wrapPrimRandr f xl xh = do
  RG ℊ ← get
  let (x,ℊ') = f (xl,xh) ℊ
  put $ RG ℊ'
  return x

------------------------
-- BASE VALUE CLASSES --
------------------------

class RandUniform a where
  prandu ∷ State RG a

class RandRange a where
  -- both bounds inclusive
  prandr ∷ a → a → State RG a

prandrRadius ∷ (RandRange a,Zero a,Minus a) ⇒ a → State RG a
prandrRadius x = prandr (neg x) x

--------------------------
-- BASE VALUE INSTANCES --
--------------------------

instance RandUniform 𝔹   where prandu = wrapPrimRandu R.uniform

instance RandUniform ℕ64 where prandu = wrapPrimRandu R.uniform
instance RandUniform ℕ32 where prandu = wrapPrimRandu R.uniform
instance RandUniform ℕ16 where prandu = wrapPrimRandu R.uniform
instance RandUniform ℕ8  where prandu = wrapPrimRandu R.uniform

instance RandUniform ℤ64 where prandu = wrapPrimRandu R.uniform
instance RandUniform ℤ32 where prandu = wrapPrimRandu R.uniform
instance RandUniform ℤ16 where prandu = wrapPrimRandu R.uniform
instance RandUniform ℤ8  where prandu = wrapPrimRandu R.uniform

instance RandRange ℕ64   where prandr = wrapPrimRandr R.uniformR
instance RandRange ℕ32   where prandr = wrapPrimRandr R.uniformR
instance RandRange ℕ16   where prandr = wrapPrimRandr R.uniformR
instance RandRange ℕ8    where prandr = wrapPrimRandr R.uniformR

instance RandRange ℤ64   where prandr = wrapPrimRandr R.uniformR
instance RandRange ℤ32   where prandr = wrapPrimRandr R.uniformR
instance RandRange ℤ16   where prandr = wrapPrimRandr R.uniformR
instance RandRange ℤ8    where prandr = wrapPrimRandr R.uniformR

instance RandRange 𝔻     where prandr = wrapPrimRandr R.uniformR

-----------------
-- COMBINATORS --
-----------------

randu ∷ ∀ a m. (MonadRand m,RandUniform a) ⇒ m a
randu = rng prandu

-- both bounds inclusive
randr ∷ ∀ a m. (MonadRand m,RandRange a) ⇒ a → a → m a
randr lb hb = rng $ prandr lb hb

randrRadius ∷ ∀ a m. (MonadRand m,RandRange a,Zero a,Minus a) ⇒ a → m a
randrRadius = rng ∘ prandrRadius

wrchoose ∷ ∀ t m a. (Monad m,MonadRand m,ToIter (ℕ64 ∧ (() → m a)) t) ⇒ t → m a
wrchoose wxs
  | isEmpty wxs = error "wrchoose not defined for zero elements"
  | otherwise   = do
      let ws = map fst $ iter wxs
          w₀ = sum ws
      let _ = if w₀ ≡ 0 then error $ "wrchoose not defined for zero total weight: " ⧺ show𝕊 ws else ()
      n ← randr 1 w₀
      runContT (\ n' → error $ "impossible" ⧺ show𝕊 n') $ mfoldOnFrom wxs 0 $ \ (w :* xM) wᵢ →
        let wᵢ' = wᵢ+w
        in
        if n ≤ wᵢ'
        then callCC $ \ _𝓀 → lift $ xM ()
        else return wᵢ'

rchoose ∷ (Monad m,MonadRand m,ToIter (() → m a) t) ⇒ t → m a
rchoose xMs
  | isEmpty xMs = error "rchoose not defined for zero elements"
  | otherwise   = wrchoose $ map (one :*) $ iter xMs

untilPass ∷ (Monad m) ⇒ (a → 𝔹) → m a → m a
untilPass f xM = loop
  where
    loop = do
      x ← xM
      if f x
      then return x
      else loop

