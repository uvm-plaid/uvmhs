module UVMHS.Core.Classes.Lattice where

import UVMHS.Init
-- import Core.Classes.Constraints
import UVMHS.Core.Classes.Order

infix  3 ∇,⊑,⊒,⪤
infixl 4 ⊔,⊟
infixl 5 ⊓

data PartialOrdering = PLT | PEQ | PGT | PUN

class POrd a where (∇) ∷ a → a → PartialOrdering

class Bot a where bot ∷ a
class Join a where (⊔) ∷ a → a → a
class (Bot a,Join a) ⇒ JoinLattice a
class Top a where top ∷ a
class Meet a where (⊓) ∷ a → a → a
class (Top a,Meet a) ⇒ MeetLattice a
class (JoinLattice a,MeetLattice a) ⇒ Lattice a

class Dual a where dual ∷ a → a
class Difference a where (⊟) ∷ a → a → a   

partialOrdering ∷ Ordering → PartialOrdering
partialOrdering = \case {LT → PLT;EQ → PEQ;GT → PGT}

(⊑) ∷ (POrd a) ⇒ a → a → 𝔹
x ⊑ y = case x ∇ y of {PLT → True;PEQ → True;PGT → False;PUN → False}

(⊒) ∷ (POrd a) ⇒ a → a → 𝔹
x ⊒ y = case x ∇ y of {PLT → False;PEQ → True;PGT → True;PUN → False}

(⪤) ∷ (POrd a) ⇒ a → a → 𝔹
x ⪤ y = case x ∇ y of {PLT → False;PEQ → False;PGT → False;PUN → True}

lfp ∷ (POrd a) ⇒ a → (a → a) → a
lfp i f = loop i where
  loop x =
    let x' = f x
    in case x' ⊑ x of
      True → x 
      False → loop x'

partialCompare ∷ (a → a → 𝔹) → a → a → PartialOrdering
partialCompare lte x y = case (lte x y,lte y x) of
  (True,True) → PEQ
  (True,False) → PLT
  (False,True) → PGT
  (False,False) → PUN

discretePartialOrder ∷ (Eq a) ⇒ a → a → PartialOrdering
discretePartialOrder x y 
  | x ≡ y = PEQ 
  | otherwise = PUN
