module UVMHS.Core.Classes.Constraints where

import UVMHS.Core.Init

infixr 1 :⇒:
infixl 3 :∧:
infixl 7 :∘:

class U a
instance U a

class (c₁ a,c₂ a) ⇒ (c₁ :∧: c₂) a
instance (c₁ a,c₂ a) ⇒ (c₁ :∧: c₂) a

class (t (u a)) ⇒ (t :∘: u) a
instance (t (u a)) ⇒ (t :∘: u) a

class (:⇒:) c₁ c₂ where impl ∷ W c₁ → W c₂
