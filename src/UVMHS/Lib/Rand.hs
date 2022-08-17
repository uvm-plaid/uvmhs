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
  prandr ∷ a → a → State RG a

prandrRadius ∷ (RandRange a,Zero a,Minus a) ⇒ a → State RG a
prandrRadius x = prandr (neg x) x

-----------------
-- FUZZY CLASS --
-----------------

class Fuzzy a where
  fuzzy ∷ FuzzyM a

data FuzzyEnv = FuzzyEnv
  { fuzzyEnvRadius ∷ ℕ64
  , fuzzyEnvDepth  ∷ ℕ64
  }

newtype FuzzyM a = FuzzyM { unRandM ∷ RWS FuzzyEnv () RG a }
  deriving
  ( Return,Bind,Functor,Monad
  , MonadReader FuzzyEnv
  , MonadState RG
  )

makeLenses ''FuzzyEnv

mkFuzzyM ∷ (FuzzyEnv → RG → RG ∧ a) → FuzzyM a
mkFuzzyM f = FuzzyM $ mkRWS $ \ γ ℊ → mapFst (flip (:*) ()) $ f γ ℊ

runFuzzyM ∷ FuzzyEnv → RG → FuzzyM a → RG ∧ a
runFuzzyM γ ℊ = mapFst fst ∘ runRWS γ ℊ ∘ unRandM

runFuzzyMRG ∷ FuzzyEnv → FuzzyM a → State RG a
runFuzzyMRG γ xM = mkState $ \ ℊ → runFuzzyM γ ℊ xM

fuzzyRec ∷ FuzzyM a → FuzzyM a
fuzzyRec = mapEnvL fuzzyEnvDepthL (\ d → d-1)

instance MonadRand FuzzyM where
  rng xM = FuzzyM $ mkRWS $ \ _γ ℊ → mapFst (flip (:*) ()) $ runState ℊ xM

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

randr ∷ ∀ a m. (MonadRand m,RandRange a) ⇒ a → a → m a
randr lb hb = rng $ prandr lb hb

randrRadius ∷ ∀ a m. (MonadRand m,RandRange a,Zero a,Minus a) ⇒ a → m a
randrRadius = rng ∘ prandrRadius

rand ∷ ∀ a m. (MonadRand m,Fuzzy a) ⇒ ℕ64 → ℕ64 → m a
rand r d = rng $ runFuzzyMRG (FuzzyEnv r d) fuzzy

wrchoose ∷ ∀ t m a. (Monad m,MonadRand m,ToIter (ℕ64 ∧ (() → m a)) t) ⇒ t → m a
wrchoose wxs 
  | isEmpty wxs = error "wrchoose not defined for zero elements"
  | otherwise   = do
      let w₀ = sum $ map fst $ iter wxs
      let _ = if w₀ ≡ 0 then error "wrchoose not defined for zero total weight" else ()
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

randTny ∷ ∀ a m. (MonadRand m,Fuzzy a) ⇒ m a
randTny = rand 1 1

randSml ∷ ∀ a m. (MonadRand m,Fuzzy a) ⇒ m a
randSml = rand 4 4

randMed ∷ ∀ a m. (MonadRand m,Fuzzy a) ⇒ m a
randMed = rand 16 16 

randLrg ∷ ∀ a m. (MonadRand m,Fuzzy a) ⇒ m a
randLrg = rand 64 64

untilPass ∷ (Monad m) ⇒ (a → 𝔹) → m a → m a
untilPass f xM = loop
  where
    loop = do
      x ← xM
      if f x 
      then return x
      else loop

---------------------
-- FUZZY INSTANCES --
---------------------

instance Fuzzy ℕ64 where fuzzy = randr zero          ∘ (×2) *$ askL fuzzyEnvRadiusL
instance Fuzzy ℕ32 where fuzzy = randr zero ∘ natΩ32 ∘ (×2) *$ askL fuzzyEnvRadiusL
instance Fuzzy ℕ16 where fuzzy = randr zero ∘ natΩ16 ∘ (×2) *$ askL fuzzyEnvRadiusL
instance Fuzzy ℕ8  where fuzzy = randr zero ∘ natΩ8  ∘ (×2) *$ askL fuzzyEnvRadiusL

instance Fuzzy ℤ64 where fuzzy = randrRadius ∘ intΩ64 *$ askL fuzzyEnvRadiusL
instance Fuzzy ℤ32 where fuzzy = randrRadius ∘ intΩ32 *$ askL fuzzyEnvRadiusL
instance Fuzzy ℤ16 where fuzzy = randrRadius ∘ intΩ16 *$ askL fuzzyEnvRadiusL
instance Fuzzy ℤ8  where fuzzy = randrRadius ∘ intΩ8  *$ askL fuzzyEnvRadiusL

instance Fuzzy 𝔻   where fuzzy = randrRadius ∘ dbl    *$ askL fuzzyEnvRadiusL

instance Fuzzy () where fuzzy = return ()
instance (Fuzzy a) ⇒ Fuzzy (() → a) where fuzzy = const ^$ fuzzy

instance (Fuzzy a) ⇒ Fuzzy (𝑂 a) where 
  fuzzy = rchoose
    [ const $ return None
    , const $ Some ^$ fuzzy
    ]

instance (Fuzzy a,Fuzzy b) ⇒ Fuzzy (a ∨ b) where 
  fuzzy = rchoose
    [ const $ Inl ^$ fuzzy
    , const $ Inr ^$ fuzzy
    ]

instance (Fuzzy a,Fuzzy b) ⇒ Fuzzy (a ∧ b) where 
  fuzzy = do
    x ← fuzzy
    y ← fuzzy
    return $ x :* y

instance (Fuzzy a) ⇒ Fuzzy (𝐿 a) where 
  fuzzy = do
    w ← (×2) ^$ askL fuzzyEnvRadiusL
    list ^$ mapMOn (upto w) $ const fuzzy

instance (Ord k,Fuzzy k,Fuzzy v) ⇒ Fuzzy (k ⇰ v) where 
  fuzzy = assoc ^$ fuzzy @(𝐿 _)
