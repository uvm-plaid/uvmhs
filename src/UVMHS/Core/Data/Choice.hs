module UVMHS.Core.Data.Choice where

import UVMHS.Core.Init
import UVMHS.Core.Classes

instance Functor ((∨) a) where
  map f = \case
    Inl x → Inl x
    Inr y → Inr $ f y
instance Return ((∨) a) where
  return = Inr
instance Bind ((∨) a) where
  Inl x ≫= _ = Inl x
  Inr y ≫= k = k y
instance Monad ((∨) a)
instance FunctorM ((∨) a) where
  mapM f = \case
    Inl x → return $ Inl x
    Inr y → do
      y' ← f y
      return $ Inr y'

instance (Null b) ⇒ Null (a ∨ b) where
  null = Inr null
instance (Append a,Append b) ⇒ Append (a ∨ b) where
  Inl x ⧺ Inl y = Inl (x ⧺ y)
  Inl x ⧺ Inr _ = Inl x
  Inr _ ⧺ Inl y = Inl y
  Inr x ⧺ Inr y = Inr (x ⧺ y)
instance (Append a,Monoid b) ⇒ Monoid (a ∨ b)

elimChoice ∷ (a → c) → (b → c) → a ∨ b → c
elimChoice f₁ f₂ = \case
  Inl x → f₁ x
  Inr y → f₂ y

mapChoice ∷ (a₁ → a₂) → (b₁ → b₂) → a₁ ∨ b₁ → a₂ ∨ b₂
mapChoice f g = elimChoice (Inl ∘ f) (Inr ∘ g)

mapInl ∷ (a₁ → a₂) → a₁ ∨ b → a₂ ∨ b
mapInl f = mapChoice f id

mapInr ∷ (b₁ → b₂) → a ∨ b₁ → a ∨ b₂
mapInr f = mapChoice id f

mapMChoice ∷ (Monad m) ⇒ (a → m a') → (b → m b') → a ∨ b → m (a' ∨ b')
mapMChoice f g = \case
  Inl x → do
    x' ← f x
    return $ Inl x'
  Inr y → do
    y' ← g y
    return $ Inr y'

mapMInl ∷ (Monad m) ⇒ (a → m a') → a ∨ b → m (a' ∨ b)
mapMInl = flip mapMChoice return

mapMInr ∷ (Monad m) ⇒ (b → m b') → a ∨ b → m (a ∨ b')
mapMInr = mapMChoice return
