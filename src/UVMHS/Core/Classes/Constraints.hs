module UVMHS.Core.Classes.Constraints where

import UVMHS.Core.Init

infixr 2 :⇒:
infixl 4 :∧:
infixl 6 :∘:

class U a
instance U a

class (c₁ a,c₂ a) ⇒ (c₁ :∧: c₂) a
instance (c₁ a,c₂ a) ⇒ (c₁ :∧: c₂) a

class (t (u a)) ⇒ (t :∘: u) a
instance (t (u a)) ⇒ (t :∘: u) a

class (:⇒:) c₁ c₂ where impl ∷ W c₁ → W c₂
