module UVMHS.Core.Data.Choice where

import UVMHS.Init
import UVMHS.Core.Classes

instance Functor ((∨) a) where 
  map f = \case
    Inl x → Inl x
    Inr y → Inr $ f y
instance Return ((∨) a) where return = Inr
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

instance (Null b) ⇒ Null (a ∨ b) where null = Inr null
instance (Append a,Append b) ⇒ Append (a ∨ b) where 
  Inl x ⧺ Inl y = Inl (x ⧺ y)
  Inl x ⧺ Inr _ = Inl x
  Inr _ ⧺ Inl y = Inl y
  Inr x ⧺ Inr y = Inr (x ⧺ y)
instance (Append a,Monoid b) ⇒ Monoid (a ∨ b) 

elimAlt ∷ (a → c) → (b → c) → a ∨ b → c
elimAlt f₁ f₂ = \case
  Inl x → f₁ x
  Inr y → f₂ y

mapAlt ∷ (a₁ → a₂) → (b₁ → b₂) → a₁ ∨ b₁ → a₂ ∨ b₂
mapAlt f g = elimAlt (Inl ∘ f) (Inr ∘ g)

mapInl ∷ (a₁ → a₂) → a₁ ∨ b → a₂ ∨ b
mapInl f = mapAlt f id

mapInr ∷ (b₁ → b₂) → a ∨ b₁ → a ∨ b₂
mapInr f = mapAlt id f
