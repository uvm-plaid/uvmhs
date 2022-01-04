module UVMHS.Lib.Rand where

import UVMHS.Core

import System.Random as R

newtype RG = RG { unRG ∷ R.StdGen }

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

class RandUniform a where
  prandu ∷ State RG a

class RandRange a where
  prandr ∷ a → a → State RG a

class Rand a where
  prand ∷ ℕ64 → ℕ64 → State RG a

instance RandUniform 𝔹 where prandu = wrapPrimRandu R.uniform

instance RandUniform ℕ64 where prandu = wrapPrimRandu R.uniform
instance RandUniform ℕ32 where prandu = wrapPrimRandu R.uniform
instance RandUniform ℕ16 where prandu = wrapPrimRandu R.uniform
instance RandUniform ℕ8  where prandu = wrapPrimRandu R.uniform

instance RandUniform ℤ64 where prandu = wrapPrimRandu R.uniform
instance RandUniform ℤ32 where prandu = wrapPrimRandu R.uniform
instance RandUniform ℤ16 where prandu = wrapPrimRandu R.uniform
instance RandUniform ℤ8  where prandu = wrapPrimRandu R.uniform

instance RandRange ℕ64 where prandr = wrapPrimRandr R.uniformR
instance RandRange ℕ32 where prandr = wrapPrimRandr R.uniformR
instance RandRange ℕ16 where prandr = wrapPrimRandr R.uniformR
instance RandRange ℕ8  where prandr = wrapPrimRandr R.uniformR

instance RandRange ℤ64 where prandr = wrapPrimRandr R.uniformR
instance RandRange ℤ32 where prandr = wrapPrimRandr R.uniformR
instance RandRange ℤ16 where prandr = wrapPrimRandr R.uniformR
instance RandRange ℤ8  where prandr = wrapPrimRandr R.uniformR

instance RandRange 𝔻   where prandr = wrapPrimRandr R.uniformR

instance Rand ℕ64 where prand nˢ _nᵈ = prandr zero nˢ
instance Rand ℕ32 where prand nˢ _nᵈ = prandr zero $ natΩ32 nˢ
instance Rand ℕ16 where prand nˢ _nᵈ = prandr zero $ natΩ16 nˢ
instance Rand ℕ8  where prand nˢ _nᵈ = prandr zero $ natΩ8 nˢ

instance Rand ℤ64 where prand nˢ _nᵈ = prandr (neg $ intΩ64 nˢ) $ intΩ64 nˢ
instance Rand ℤ32 where prand nˢ _nᵈ = prandr (neg $ intΩ32 nˢ) $ intΩ32 nˢ
instance Rand ℤ16 where prand nˢ _nᵈ = prandr (neg $ intΩ16 nˢ) $ intΩ16 nˢ
instance Rand ℤ8  where prand nˢ _nᵈ = prandr (neg $ intΩ8 nˢ) $ intΩ8 nˢ

instance Rand 𝔻 where prand nˢ _nᵈ = prandr (neg $ dbl nˢ) $ dbl nˢ

prchoose ∷ (ToIter (() → a) t) ⇒ t → State RG a
prchoose = prwchoose ∘ map (one :*) ∘ iter

prwchoose ∷ (ToIter (ℕ64 ∧ (() → a)) t) ⇒ t → State RG a
prwchoose wxs = do
  let ixs = vec $ do
        w :* x ← iter wxs
        replicate w x
  n ← prandr zero $ csize ixs - one
  return $ (ixs ⋕! n) ()

prand𝑂 ∷ (ℕ64 → ℕ64 → State RG a) → ℕ64 → ℕ64 → State RG (𝑂 a)
prand𝑂 prandA nˢ nᵈ = mjoin $ prchoose
  [ \ () → return None
  , \ () → Some ^$ prandA nˢ nᵈ
  ]

prandChoice ∷ (ℕ64 → ℕ64 → State RG a) → (ℕ64 → ℕ64 → State RG b) → ℕ64 → ℕ64 → State RG (a ∨ b)
prandChoice prandA prandB nˢ nᵈ = mjoin $ prchoose
  [ \ () → Inl ^$ prandA nˢ nᵈ
  , \ () → Inr ^$ prandB nˢ nᵈ
  ]

prandPair ∷ (ℕ64 → ℕ64 → State RG a) → (ℕ64 → ℕ64 → State RG b) → ℕ64 → ℕ64 → State RG (a ∧ b)
prandPair prandA prandB nˢ nᵈ = do
  x ← prandA nˢ nᵈ
  y ← prandB nˢ nᵈ
  return $ x :* y

prandList ∷ (ℕ64 → ℕ64 → State RG a) → ℕ64 → ℕ64 → State RG (𝐿 a)
prandList prandA nˢ nᵈ = mjoin $ prwchoose
  [ (one :*) $ \ () → do
      return Nil
  , (nᵈ :*) $ \ () → do 
      x ← prandA nˢ nᵈ
      xs ← prandList prandA nˢ $ nᵈ-one
      return $ x :& xs
  ]

instance (Rand a) ⇒ Rand (𝑂 a) where prand = prand𝑂 prand
instance (Rand a,Rand b) ⇒ Rand (a ∨ b) where prand = prandChoice prand prand
instance (Rand a,Rand b) ⇒ Rand (a ∧ b) where prand = prandPair prand prand
instance (Rand a) ⇒ Rand (𝐿 a) where prand = prandList prand

class MonadRand m where
  rgen ∷ (State RG a) → m a

instance MonadRand IO where
  rgen f = R.getStdRandom $ \ ℊ → 
    let RG ℊ' :* x = runState (RG ℊ) f
    in (x,ℊ')

rgenSL ∷ (Monad m,MonadState s m,HasLens s RG) ⇒ (RG → RG ∧ a) → m a
rgenSL f = do
  ℊ ← getL hasLens
  let ℊ' :* x = f ℊ
  putL hasLens ℊ'
  return x

randu ∷ ∀ a m. (MonadRand m,RandUniform a) ⇒ m a
randu = rgen prandu

randr ∷ ∀ a m. (MonadRand m,RandRange a) ⇒ a → a → m a
randr lb hb = rgen $ prandr lb hb

rand ∷ ∀ a m. (MonadRand m,Rand a) ⇒ ℕ64 → ℕ64 → m a
rand nˢ nᵈ = rgen $ prand nˢ nᵈ

randSml ∷ ∀ a m. (MonadRand m,Rand a) ⇒ m a
randSml = rand (𝕟64 5) $ 𝕟64 5

randMed ∷ ∀ a m. (MonadRand m,Rand a) ⇒ m a
randMed = rand (𝕟64 20) $ 𝕟64 20

randLrg ∷ ∀ a m. (MonadRand m,Rand a) ⇒ m a
randLrg = rand (𝕟64 100) $ 𝕟64 100

rchoose ∷ (Monad m,MonadRand m,ToIter (() → a) t) ⇒ t → m a
rchoose = rgen ∘ prchoose

rwchoose ∷ (Monad m,MonadRand m,ToIter (ℕ64 ∧ (() → a)) t) ⇒ t → m a
rwchoose = rgen ∘ prwchoose

untilTry ∷ (Monad m) ⇒ (a → 𝔹) → m a → m a
untilTry f xM = loop
  where
    loop = do
      x ← xM
      if f x 
      then return x
      else loop
