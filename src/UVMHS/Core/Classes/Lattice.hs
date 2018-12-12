module UVMHS.Core.Classes.Lattice where

import UVMHS.Init
-- import Core.Classes.Constraints
import UVMHS.Core.Classes.Order

infix  3 ∇,⊑,⊒,⪤
infixl 4 ⊔,⊟
infixl 5 ⊓

class POrd a where (⊑) ∷ a → a → 𝔹

class Bot a where bot ∷ a
class Join a where (⊔) ∷ a → a → a
class (Bot a,Join a) ⇒ JoinLattice a
class Top a where top ∷ a
class Meet a where (⊓) ∷ a → a → a
class (Top a,Meet a) ⇒ MeetLattice a
class (JoinLattice a,MeetLattice a) ⇒ Lattice a

class Dual a where dual ∷ a → a
class Difference a where (⊟) ∷ a → a → a   

data PartialOrdering = PLT | PEQ | PGT | PUN

(∇) ∷ (POrd a) ⇒ a → a → PartialOrdering
x ∇ y = case (x ⊑ y,y ⊑ x) of
  (True,True) → PEQ
  (True,False) → PLT
  (False,True) → PGT
  (False,False) → PUN

(⊒) ∷ (POrd a) ⇒ a → a → 𝔹
(⊒) = flip (⊑)

(⪤) ∷ (POrd a) ⇒ a → a → 𝔹
x ⪤ y = ((x ⊑ y) ≡ True) ⩓ ((y ⊑ x) ≡ False)

lfp ∷ (POrd a) ⇒ a → (a → a) → a
lfp i f = loop i where
  loop x =
    let x' = f x
    in case x' ⊑ x of
      True → x 
      False → loop x'
