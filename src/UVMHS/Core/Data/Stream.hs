module UVMHS.Core.Data.Stream where

import UVMHS.Core.Init
import UVMHS.Core.Classes

import UVMHS.Core.Data.Arithmetic ()
import UVMHS.Core.Data.LazyList ()
import UVMHS.Core.Data.Iter
import UVMHS.Core.Data.Option
import UVMHS.Core.Data.Pair
import UVMHS.Core.Data.String

instance (Eq a) ⇒ Eq (𝑆 a) where (==) = eqBy𝑆 (≡)
instance (Ord a) ⇒ Ord (𝑆 a) where compare = compareBy𝑆 (⋚)
instance (Show a) ⇒ Show (𝑆 a) where show = chars ∘ showWith𝑆 show𝕊

instance Functor 𝑆 where map = map𝑆

instance Null (𝑆 a) where null = empty𝑆
instance Append (𝑆 a) where (⧺) = append𝑆
instance Monoid (𝑆 a)

instance Return 𝑆 where return = single𝑆
instance Bind 𝑆 where (≫=) = bind𝑆

instance Single a (𝑆 a) where single = single𝑆

instance ToStream a (𝑆 a) where stream = id
instance ToIter a (𝑆 a) where iter = iter𝑆

empty𝑆 ∷ 𝑆 a
empty𝑆 = 𝑆 () $ const None

single𝑆 ∷ a → 𝑆 a
single𝑆 x = 𝑆 False $ \case
  False → Some $ x :* True
  True → None

append𝑆 ∷ 𝑆 a → 𝑆 a → 𝑆 a
append𝑆 (𝑆 s₁₀ f₁) (𝑆 s₂₀ f₂) = 𝑆 (Inl s₁₀) $ \ s →
  let goLeft s₁ = case f₁ s₁ of
        None → goRight s₂₀
        Some (x :* s₁') → Some (x :* Inl s₁')
      goRight s₂ = case f₂ s₂ of
        None → None
        Some (x :* s₂') → Some (x :* Inr s₂')
  in case s of
    Inl s₁ → goLeft s₁
    Inr s₂ → goRight s₂

map𝑆 ∷ (a → b) → 𝑆 a → 𝑆 b
map𝑆 f (𝑆 s₀ g) = 𝑆 s₀ $ \ s → 
  case g s of
    None → None
    Some (x:*s') → Some (f x:*s')

mjoin𝑆 ∷ ∀ a. 𝑆 (𝑆 a) → 𝑆 a
mjoin𝑆 (𝑆 (s₀ ∷ s) (f ∷ s → 𝑂 (𝑆 a ∧ s))) = 𝑆 (𝑆 () (const None) :* s₀ ∷ 𝑆 a ∧ s) $ \ (𝑆 t g :* s) → loop₁ t g s
  where
    loop₁ ∷ ∀ s'. s' → (s' → 𝑂 (a ∧ s')) → s → 𝑂 (a ∧ (𝑆 a ∧ s))
    loop₁ t g s = case g t of
      None → loop₂ s
      Some (x :* t') → Some (x :* (𝑆 t' g :* s))
    loop₂ ∷ s → 𝑂 (a ∧ (𝑆 a ∧ s))
    loop₂ s = case f s of
      None → None
      Some (𝑆 t g :* s') → loop₁ t g s'

bind𝑆 ∷ 𝑆 a → (a → 𝑆 b) → 𝑆 b
bind𝑆 xs k = mjoin𝑆 $ map𝑆 k xs

uncons𝑆 ∷ 𝑆 a → 𝑂 (a ∧ 𝑆 a)
uncons𝑆 (𝑆 s g) = case g s of
  None → None
  Some (x :* s') → Some (x :* 𝑆 s' g)

eqBy𝑆 ∷ (a → a → 𝔹) → 𝑆 a → 𝑆 a → 𝔹
eqBy𝑆 f (𝑆 s₁₀ g₁) (𝑆 s₂₀ g₂) = loop s₁₀ s₂₀
  where
    loop s₁ s₂ = case (g₁ s₁,g₂ s₂) of
      (None,None) → True
      (Some _,None) → False
      (None,Some _) → False
      (Some (x₁ :* s₁'),Some (x₂ :* s₂')) → case f x₁ x₂ of
        True → loop s₁' s₂' 
        False → False

compareBy𝑆 ∷ (a → a → Ordering) → 𝑆 a → 𝑆 a → Ordering
compareBy𝑆 f (𝑆 s₁₀ g₁) (𝑆 s₂₀ g₂) = loop s₁₀ s₂₀
  where
    loop s₁ s₂ = case (g₁ s₁,g₂ s₂) of
      (None,None) → EQ
      (None,Some _) → LT
      (Some _,None) → GT
      (Some (x₁ :* s₁'),Some (x₂ :* s₂')) → case f x₁ x₂ of
        LT → LT
        EQ → loop s₁' s₂'
        GT → GT

showWith𝑆 ∷ (a → 𝕊) → 𝑆 a → 𝕊
showWith𝑆 = showCollection "𝑆[" "]" ","

isEmpty ∷ (ToStream a t) ⇒ t → 𝔹
isEmpty (stream → 𝑆 s g) = isNone $ g s

naturals ∷ 𝑆 ℕ
naturals = 𝑆 0 $ \ i → Some (i :* succ i)

zipWith :: (ToStream a t₁,ToStream b t₂) ⇒ (a → b → c) → t₁ → t₂ → 𝑆 c
zipWith f (stream → 𝑆 s₁₀ g₁) (stream → 𝑆 s₂₀ g₂) = 𝑆 (s₁₀ :* s₂₀) $ \ (s₁ :* s₂) → do
  (x :* s₁') ← g₁ s₁
  (y :* s₂') ← g₂ s₂
  return $ f x y :* (s₁' :* s₂')

zip ∷ (ToStream a t₁,ToStream b t₂) ⇒ t₁ → t₂ → 𝑆 (a ∧ b)
zip = zipWith (:*)

zip3With ∷ (ToStream a t₁,ToStream b t₂,ToStream c t₃) ⇒ (a → b → c → d) → t₁ → t₂ → t₃ → 𝑆 d
zip3With f (stream → 𝑆 s₁₀ g₁) (stream → 𝑆 s₂₀ g₂) (stream → 𝑆 s₃₀ g₃) =
  𝑆 (s₁₀ :* s₂₀ :* s₃₀) $ \ (s₁ :* s₂ :* s₃) → do
    (x :* s₁') ← g₁ s₁
    (y :* s₂') ← g₂ s₂
    (z :* s₃') ← g₃ s₃
    return $ f x y z :* (s₁' :* s₂' :* s₃')

zip3 ∷ (ToStream a t₁,ToStream b t₂,ToStream c t₃) ⇒ t₁ → t₂ → t₃ → 𝑆 (a ∧ b ∧ c)
zip3 = zip3With $ (:*) ∘∘ (:*)

firstN ∷ (ToStream a t) ⇒ ℕ → t → 𝑆 a
firstN n₀ (stream → 𝑆 s₀ g) = 𝑆 (s₀ :* 0) $ \ (s :* n) → case n ≡ n₀ of
  True → None 
  False → do
    (x :* s') ← g s
    return (x :* (s' :* succ n))

lastN ∷ (ToStream a t) ⇒ ℕ → t → 𝐼 a
lastN n = reverse ∘ firstN n ∘ list ∘ reverse ∘ stream

skipN ∷ (ToStream a t) ⇒ ℕ → t → 𝑆 a
skipN n₀ (stream → 𝑆 s₀ g) = 𝑆 (loop 0 s₀) g
  where
    loop n s 
      | n ≡ n₀ = s 
      | otherwise = ifNone s $ do
          s' ← snd ^$ g s
          return $ loop (succ n) s'

stripPrefix𝑆 ∷ (Eq a,ToStream a t₁,ToStream a t₂) ⇒ t₁ → t₂ → 𝑂 (𝑆 a)
stripPrefix𝑆 (stream → 𝑆 s₁₀ g₁) (stream → 𝑆 s₂₀ g₂) = loop s₁₀ s₂₀
  where
    loop s₁ s₂ = case g₁ s₁ of
      None → Some $ 𝑆 s₂ g₂
      Some (x :* s₁') → do
        (y :* s₂') ← g₂ s₂
        case x ≡ y of
          True → loop s₁' s₂' 
          False → None
    
prefixBefore𝑆 ∷ (ToStream a t) ⇒ (a → 𝔹) → t → 𝑆 a
prefixBefore𝑆 p (stream → 𝑆 s₀ g) = 𝑆 s₀ $ \ s → do
  (x :* s') ← g s
  case p x of
    True → None 
    False → Some (x :* s')

prefixBeforeN𝑆 ∷ (ToStream a t) ⇒ ℕ → (a → ℕ) → t → 𝑆 a
prefixBeforeN𝑆 n₀ p (stream → 𝑆 s₀ g) 
  | n₀ ≡ 0 = empty𝑆
  | otherwise = 𝑆 (0 :* s₀) $ \ (n :* s) → do
      (x :* s') ← g s
      let n' = n + p x
      case n' ≥ n₀ of
        True → None 
        False → return (x :* (n' :* s'))

postfixAfter𝑆 ∷ (ToStream a t) ⇒ (a → 𝔹) → t → 𝑆 a
postfixAfter𝑆 p (stream → 𝑆 s₀ g) = ifNone empty𝑆 $ loop s₀
  where
    loop s = do
      (x :* s') ← g s
      case p x of
        True → Some (𝑆 s' g) 
        False → loop s'

inbetween𝑆 ∷ (ToStream a t) ⇒ a → t → 𝑆 a
inbetween𝑆 i (stream → 𝑆 s₀ g) = 𝑆 (s₀ :* None) $ \ (s :* xMM) → do
  case xMM of
    None → do
      x :* s' ← g s
      return $ x :* (s' :* Some None)
    Some None → do
      x :* s' ← g s
      return $ i :* (s' :* Some (Some x))
    Some (Some x) → do
      return $ x :* (s :* Some None)

coredata_stream_e1 ∷ 𝑆 ℕ
coredata_stream_e1 = stream [1,2,3,4,5,4,3,2,1]

split ∷ (ToStream (a ∧ b) t) ⇒ t → 𝑆 a ∧ 𝑆 b
split (stream → xs) = map fst xs :* map snd xs
