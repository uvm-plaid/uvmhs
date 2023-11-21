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

type FCoercibleRel t t' = (∀ x x'. (Coercible x x') ⇒ Coercible (t x) (t' x')) ∷ Constraint
type FCoercible t = FCoercibleRel t t

fcoercibleW_UNSAFE ∷ W (FCoercible m)
fcoercibleW_UNSAFE = coerce_UNSAFE (W ∷ W (FCoercible 𝑂))

type Func (c ∷ ★ → Constraint) (t ∷ ★ → ★) = (∀ x. (c x) ⇒ c (t x)) ∷ Constraint
type Const (c ∷ ★ → Constraint) (t ∷ ★ → ★) = (∀ x. c (t x)) ∷ Constraint

