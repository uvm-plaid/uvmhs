module UVMHS.Core.Classes.Arithmetic where

import UVMHS.Init

import UVMHS.Core.Classes.Order

infixl 4 +,-
infixl 5 ×,/,⌿,÷
infixl 7 ^

class Additive a where {zero ∷ a;(+) ∷ a → a → a}
class (Additive a) ⇒ Subtractive a where (-) ∷ a → a → a
class (Additive a) ⇒ Multiplicative a where {one ∷ a;(×) ∷ a → a → a}
class (Multiplicative a) ⇒ Divisible a where (/) ∷ a → a → a
class (Multiplicative a) ⇒ TruncateDivisible a where {(⌿) ∷ a → a → a;(÷) ∷ a → a → a}
class (Multiplicative a) ⇒ Exponential a where (^) ∷ a → a → a

even ∷ (Eq a,TruncateDivisible a) ⇒ a → 𝔹
even x = x ÷ (one + one) ≡ zero

odd ∷ (Eq a,TruncateDivisible a) ⇒ a → 𝔹
odd x = x ÷ (one + one) ≢ zero

succ ∷ (Additive a,Multiplicative a) ⇒ a → a
succ x = x + one

neg ∷ (Subtractive a) ⇒ a → a
neg x = zero - x

class ToNat   a where nat   ∷ a → ℕ
class ToNat64 a where nat64 ∷ a → ℕ64
class ToNat32 a where nat32 ∷ a → ℕ32
class ToNat16 a where nat16 ∷ a → ℕ16
class ToNat8  a where nat8  ∷ a → ℕ8

class ToNatO   a where natO   ∷ a → 𝑂 ℕ
class ToNatO64 a where natO64 ∷ a → 𝑂 ℕ64
class ToNatO32 a where natO32 ∷ a → 𝑂 ℕ32
class ToNatO16 a where natO16 ∷ a → 𝑂 ℕ16
class ToNatO8  a where natO8  ∷ a → 𝑂 ℕ8

natΩ ∷ (ToNatO a) ⇒ a → ℕ
natΩ x = case natO x of {None → error "failed natΩ conversion";Some n → n}
natΩ64 ∷ (ToNatO64 a) ⇒ a → ℕ64
natΩ64 x = case natO64 x of {None → error "failed natΩ64 conversion";Some n → n}
natΩ32 ∷ (ToNatO32 a) ⇒ a → ℕ32
natΩ32 x = case natO32 x of {None → error "failed natΩ32 conversion";Some n → n}
natΩ16 ∷ (ToNatO16 a) ⇒ a → ℕ16
natΩ16 x = case natO16 x of {None → error "failed natΩ16 conversion";Some n → n}
natΩ8 ∷ (ToNatO8 a) ⇒ a → ℕ8
natΩ8 x = case natO8 x of {None → error "failed natΩ8 conversion";Some n → n}

class ToInt   a where int   ∷ a → ℤ
class ToInt64 a where int64 ∷ a → ℤ64
class ToInt32 a where int32 ∷ a → ℤ32
class ToInt16 a where int16 ∷ a → ℤ16
class ToInt8  a where int8  ∷ a → ℤ8

class ToIntO64 a where intO64 ∷ a → 𝑂 ℤ64
class ToIntO32 a where intO32 ∷ a → 𝑂 ℤ32
class ToIntO16 a where intO16 ∷ a → 𝑂 ℤ16
class ToIntO8  a where intO8  ∷ a → 𝑂 ℤ8

intΩ64 ∷ (ToIntO64 a) ⇒ a → ℤ64
intΩ64 x = case intO64 x of {None → error "failed intΩ64 conversion";Some n → n}
intΩ32 ∷ (ToIntO32 a) ⇒ a → ℤ32
intΩ32 x = case intO32 x of {None → error "failed intΩ32 conversion";Some n → n}
intΩ16 ∷ (ToIntO16 a) ⇒ a → ℤ16
intΩ16 x = case intO16 x of {None → error "failed intΩ16 conversion";Some n → n}
intΩ8 ∷ (ToIntO8 a) ⇒ a → ℤ8
intΩ8 x = case intO8 x of {None → error "failed intΩ8 conversion";Some n → n}

class ToDouble a where dbl ∷ a → 𝔻
