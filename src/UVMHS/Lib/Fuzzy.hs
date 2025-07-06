module UVMHS.Lib.Fuzzy where

import UVMHS.Core
import UVMHS.Lib.Rand

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

-- | Use this to ensure termination when building recursive datatypes.  Note that this will
-- underflow to the maximum natural when `d` is zero, so you should only every use this when you're
-- sure `d` is not zero, e.g. guarded behind a `wrchoose` with coefficient `d`.
fuzzyDec ∷ FuzzyM a → FuzzyM a
fuzzyDec xM = do
  d ← askL fuzzyEnvDepthL
  assertM $ \ () → d ≢ 0
  localL fuzzyEnvDepthL (d - 1) xM

fuzzyRec ∷ (Fuzzy a) ⇒ FuzzyM a
fuzzyRec = fuzzyDec fuzzy

instance MonadRand FuzzyM where
  rng xM = FuzzyM $ mkRWS $ \ _γ ℊ → mapFst (flip (:*) ()) $ runState ℊ xM

rand ∷ ∀ a m. (MonadRand m,Fuzzy a) ⇒ ℕ64 → ℕ64 → m a
rand r d = rng $ runFuzzyMRG (FuzzyEnv r d) fuzzy

randTny ∷ ∀ a m. (MonadRand m,Fuzzy a) ⇒ m a
randTny = rand 1 1

randSml ∷ ∀ a m. (MonadRand m,Fuzzy a) ⇒ m a
randSml = rand 4 4

randMed ∷ ∀ a m. (MonadRand m,Fuzzy a) ⇒ m a
randMed = rand 16 16

randLrg ∷ ∀ a m. (MonadRand m,Fuzzy a) ⇒ m a
randLrg = rand 64 64

fuzzyRadius ∷ FuzzyM ℕ64
fuzzyRadius = randr 0 *$ askL fuzzyEnvRadiusL

fuzzyDepth ∷ FuzzyM ℕ64
fuzzyDepth = randr 0 *$ askL fuzzyEnvDepthL

---------------------
-- FUZZY INSTANCES --
---------------------

isoFuzzy ∷ (a ⇄ b,Fuzzy b) ⇒ FuzzyM a
isoFuzzy = isofr ^$ fuzzy

instance Fuzzy ℕ64 where fuzzy = randr zero          ∘ (×2) *$ askL fuzzyEnvRadiusL
instance Fuzzy ℕ32 where fuzzy = randr zero ∘ natΩ32 ∘ (×2) *$ askL fuzzyEnvRadiusL
instance Fuzzy ℕ16 where fuzzy = randr zero ∘ natΩ16 ∘ (×2) *$ askL fuzzyEnvRadiusL
instance Fuzzy ℕ8  where fuzzy = randr zero ∘ natΩ8  ∘ (×2) *$ askL fuzzyEnvRadiusL

instance Fuzzy ℤ64 where fuzzy = randrRadius ∘ intΩ64 *$ askL fuzzyEnvRadiusL
instance Fuzzy ℤ32 where fuzzy = randrRadius ∘ intΩ32 *$ askL fuzzyEnvRadiusL
instance Fuzzy ℤ16 where fuzzy = randrRadius ∘ intΩ16 *$ askL fuzzyEnvRadiusL
instance Fuzzy ℤ8  where fuzzy = randrRadius ∘ intΩ8  *$ askL fuzzyEnvRadiusL

instance Fuzzy 𝔻   where 
  fuzzy = truncateDecimals 2 ^∘ randrRadius ∘ dbl *$ askL fuzzyEnvRadiusL

instance Fuzzy () where fuzzy = return ()

instance Fuzzy 𝔹 where
  fuzzy = rchoose
    [ \ () → return True
    , \ () → return False
    ]

instance (Fuzzy a) ⇒ Fuzzy (𝑂 a) where
  fuzzy = rchoose
    [ \ () → return None
    , \ () → Some ^$ fuzzy
    ]

instance (Fuzzy a,Fuzzy b) ⇒ Fuzzy (a ∨ b) where
  fuzzy = rchoose
    [ \ () → Inl ^$ fuzzy
    , \ () → Inr ^$ fuzzy
    ]

instance (Fuzzy a,Fuzzy b) ⇒ Fuzzy (a ∧ b) where
  fuzzy = do
    x ← fuzzy
    y ← fuzzy
    return $ x :* y

instance (Fuzzy a) ⇒ Fuzzy (𝐿 a) where
  fuzzy = do
    w ← fuzzyDepth
    list ^$ mapMOn (upto w) $ const fuzzy

instance (Ord k,Fuzzy k,Fuzzy v) ⇒ Fuzzy (k ⇰ v) where
  fuzzy = assoc ^$ fuzzy @(𝐿 _)

instance (Fuzzy a) ⇒ Fuzzy (() → a) where fuzzy = const ^$ fuzzy


