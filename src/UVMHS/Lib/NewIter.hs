module UVMHS.Lib.NewIter where

import UVMHS.Core

newtype 𝐼' a = 𝐼' 
  { un𝐼' ∷ ∀ b. (a → b → (b → b) → b) → b → (b → b) → b 
  }

iterPair ∷ a ∧ a → 𝐼' a
iterPair (x₁ :* x₂) = 𝐼' $ \ f i 𝓀 → 
  f x₁ i $ \ i' →
  f x₂ i' 𝓀

iterList ∷ 𝐿 a → 𝐼' a
iterList xs₀ = 𝐼' $ \ f → flip $ \ 𝓀 → 
  let loop xs₁ i = case xs₁ of
        Nil → 𝓀 i
        x :& xs →
          f x i $! \ i' →
          loop xs i'
  in loop xs₀ 

upTo' ∷ ℕ → 𝐼' ℕ
upTo' n₀ = 𝐼' $ \ f → flip $ \ 𝓀 → 
  let loop n i
        | n ≡ n₀ = 𝓀 i
        | otherwise = 
            f n i $! \ i' → 
            loop (n + 1) i'
  in loop 0

fold𝐼' ∷ b → (a → b → b) → 𝐼' a → b
fold𝐼' i₀ f xs = un𝐼' xs (\ x i 𝓀 → 𝓀 $! f x i) i₀ id

foldr𝐼' ∷ b → (a → b → b) → 𝐼' a → b
foldr𝐼' i₀ f xs = un𝐼' xs (\ x i 𝓀 → f x $! 𝓀 i) i₀ id

mfold𝐼' ∷ (Monad m) ⇒ b → (a → b → m b) → 𝐼' a → m b
mfold𝐼' i₀ f xs = un𝐼' xs (\ x iM 𝓀 → do i ← iM ; 𝓀 $! f x i) (return i₀) id  

mfoldr𝐼' ∷ (Monad m) ⇒ b → (a → b → m b) → 𝐼' a → m b
mfoldr𝐼' i₀ f xs = un𝐼' xs (\ x iM 𝓀 → do i ← 𝓀 iM ; f x i) (return i₀) id  

eachDo ∷ (Monad m) ⇒ (a → m ()) → 𝐼' a → m ()
eachDo f xs = un𝐼' xs (\ x uM 𝓀 → do uM ; 𝓀 $! f x) skip id 

reverse𝐼' ∷ 𝐼' a → 𝐼' a
reverse𝐼' xs = 𝐼' $ \ f i₀ 𝓀₀ → 
  un𝐼' xs (\ x 𝓀 m𝓀 → m𝓀 $ \ i → f x i 𝓀) 𝓀₀ id i₀

listIter ∷ 𝐼' a → 𝐿 a
listIter = foldr𝐼' Nil (:&)

reverseListIter ∷ 𝐼' a → 𝐿 a
reverseListIter = fold𝐼' Nil (:&)

append𝐼' ∷ 𝐼' a → 𝐼' a → 𝐼' a
append𝐼' xs ys = 𝐼' $ \ f i 𝓀 →
  un𝐼' xs f i $ \ i' →
  un𝐼' ys f i' 𝓀

single𝐼' ∷ a → 𝐼' a
single𝐼' x = 𝐼' $ appto x

-- wfold𝐼' ∷ (Comonad w) ⇒ w b → (a → w b → b) → 𝐼' a → w b
-- wfold𝐼' i₀ f xs = unID $ evalStateT i₀ $ retState $ un𝐼' xs $ \ x → do
--   iW ← get
--   let iW' = wextend (f x) iW
--   put iW'
-- 
-- newtype DelayT m a = DelayT { unDelayT ∷ () → m a }
-- 
-- instance (Functor m) ⇒ Functor (DelayT m) where
--   map f xM = DelayT $ \ () → map f $ unDelayT xM ()
-- 
-- instance (Return m) ⇒ Return (DelayT m) where
--   return x = DelayT $ \ () → return x
-- 
-- instance (Bind m) ⇒ Bind (DelayT m) where
--   xM ≫= f = DelayT $ \ () → unDelayT xM () ≫= \ x → unDelayT (f x) ()
-- 
-- instance (Monad m) ⇒ Monad (DelayT m)
-- 
-- newtype DelayList a = DelayList { unDelayList ∷ () → 𝑂 (a ∧ DelayList a) }
-- 
-- delayList𝐼' ∷ ∀ a. 𝐼' a → DelayList a
-- delayList𝐼' xs₀ = unID $ unDelayT (mfoldr𝐼' i₀ f xs₀) ()
--   where
--     i₀ = DelayList $ \ () → None
--     f x xsDM = return $
--       DelayList $ \ () → Some $ x :* unID (unDelayT xsDM ())
--       -- DelayList $ \ () → Some $ x :* DelayList (\ () → unDelayList (unID (unDelayT xsDM ())) ())
-- 
-- firstDL ∷ DelayList a → 𝑂 a
-- firstDL xs = fst ^$ unDelayList xs ()
