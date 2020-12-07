module UVMHS.Core.Classes.Bitty where

import UVMHS.Core.Classes.Order

import UVMHS.Core.Init

infixl 5 ⟇,⊻
infixl 6 ⟑
infixl 7 ⋘,⋙

class BitZero       a where bzero  ∷ a
class BitOne        a where bone   ∷ a
class BitComp       a where comp   ∷ a → a
class BitAnd        a where (⟑)    ∷ a → a → a
class BitOr         a where (⟇)    ∷ a → a → a
class BitXor        a where (⊻)    ∷ a → a → a
class BitShiftL     a where (⋘)    ∷ a → ℕ64 → a
class BitShiftR     a where (⋙)    ∷ a → ℕ64 → a
class BitSize       a where bsize  ∷ P a → ℕ64
class
  (   BitZero   a
  ,   BitOne    a
  ,   BitComp   a
  ,   BitAnd    a
  ,   BitOr     a
  ,   BitXor    a
  ,   BitShiftL a
  ,   BitShiftR a
  ,   BitSize   a
  ) ⇒ Bitty     a

{-# INLINE bit #-}
bit ∷ (BitOne a,BitShiftL a) ⇒ ℕ64 → a
bit n = bone ⋘ n

{-# INLINE bget #-}
bget ∷ (Eq a,BitZero a,BitOne a,BitAnd a,BitShiftL a) ⇒ ℕ64 → a → Bool
bget n x = (x ⟑ bit n) ≢ bzero

{-# INLINE bset #-}
bset ∷ (BitOne a,BitOr a,BitShiftL a) ⇒ ℕ64 → a → a
bset n x = x ⟇ bit n

{-# INLINE bflp #-}
bflp ∷ (BitOne a,BitXor a,BitShiftL a) ⇒ ℕ64 → a → a
bflp n x = x ⊻ bit n

{-# INLINE bclr #-}
bclr ∷ (BitOne a,BitComp a,BitAnd a,BitShiftL a) ⇒ ℕ64 → a → a
bclr n x = x ⟑ comp (bit n)

