module UVMHS.Core.Classes.Arithmetic where

import UVMHS.Core.Init

import UVMHS.Core.Classes.Order
import UVMHS.Core.Classes.Functor

infixl 5 +,-
infixl 6 ×,⨵,/,⌿,÷
infixl 7 ^

class Zero a where zero ∷ a
class Plus a where (+) ∷ a → a → a
class Minus a where (-) ∷ a → a → a
class One a where one ∷ a
class Times a where (×) ∷ a → a → a
class Divide a where (/) ∷ a → a → a
class DivMod a where {(⌿) ∷ a → a → a;(÷) ∷ a → a → a}
class Pon a where (^^) ∷ a → ℕ → a
class Pow a where (^) ∷ a → a → a
class Root a where root ∷ a → a
class Log a where log ∷ a → a
class Efn a where efn ∷ a → a
class Sin a where sin ∷ a → a
class Cos a where cos ∷ a → a

class (Zero a,Plus a) ⇒ Additive a
class (Additive a,One a,Times a) ⇒ Multiplicative a

succ ∷ (One a,Plus a) ⇒ a → a
succ x = one + x

even ∷ (Eq a,Additive a,One a,DivMod a) ⇒ a → 𝔹
even x = x ÷ (one + one) ≡ zero

odd ∷ (Eq a,Additive a,One a,DivMod a) ⇒ a → 𝔹
odd x = x ÷ (one + one) ≢ zero

neg ∷ (Zero a,Minus a) ⇒ a → a
neg x = zero - x

(⨵) ∷ (Functor f,Multiplicative a) ⇒ a → f a → f a
x ⨵ xs = map (x ×) xs

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

natΩ ∷ (ToNatO a,STACK) ⇒ a → ℕ
natΩ x = case natO x of {None → error "failed natΩ conversion";Some n → n}
natΩ64 ∷ (ToNatO64 a,STACK) ⇒ a → ℕ64
natΩ64 x = case natO64 x of {None → error "failed natΩ64 conversion";Some n → n}
natΩ32 ∷ (ToNatO32 a,STACK) ⇒ a → ℕ32
natΩ32 x = case natO32 x of {None → error "failed natΩ32 conversion";Some n → n}
natΩ16 ∷ (ToNatO16 a,STACK) ⇒ a → ℕ16
natΩ16 x = case natO16 x of {None → error "failed natΩ16 conversion";Some n → n}
natΩ8 ∷ (ToNatO8 a,STACK) ⇒ a → ℕ8
natΩ8 x = case natO8 x of {None → error "failed natΩ8 conversion";Some n → n}

class ToInt   a where int   ∷ a → ℤ
class ToInt64 a where int64 ∷ a → ℤ64
class ToInt32 a where int32 ∷ a → ℤ32
class ToInt16 a where int16 ∷ a → ℤ16
class ToInt8  a where int8  ∷ a → ℤ8

class ToIntO   a where intO   ∷ a → 𝑂 ℤ
class ToIntO64 a where intO64 ∷ a → 𝑂 ℤ64
class ToIntO32 a where intO32 ∷ a → 𝑂 ℤ32
class ToIntO16 a where intO16 ∷ a → 𝑂 ℤ16
class ToIntO8  a where intO8  ∷ a → 𝑂 ℤ8

intΩ ∷ (ToIntO a,STACK) ⇒ a → ℤ
intΩ x = case intO x of {None → error "failed intΩ6";Some n → n}
intΩ64 ∷ (ToIntO64 a,STACK) ⇒ a → ℤ64
intΩ64 x = case intO64 x of {None → error "failed intΩ64 conversion";Some n → n}
intΩ32 ∷ (ToIntO32 a,STACK) ⇒ a → ℤ32
intΩ32 x = case intO32 x of {None → error "failed intΩ32 conversion";Some n → n}
intΩ16 ∷ (ToIntO16 a,STACK) ⇒ a → ℤ16
intΩ16 x = case intO16 x of {None → error "failed intΩ16 conversion";Some n → n}
intΩ8 ∷ (ToIntO8 a,STACK) ⇒ a → ℤ8
intΩ8 x = case intO8 x of {None → error "failed intΩ8 conversion";Some n → n}

class ToRational a where rat ∷ a → ℚ
class ToRationalO a where ratO ∷ a → 𝑂 ℚ

class ToRationalᴾ a where ratᴾ ∷ a → ℚᴾ
class ToRationalᴾO a where ratᴾO ∷ a → 𝑂 ℚᴾ

class ToDouble a where dbl ∷ a → 𝔻
class ToDoubleO a where dblO ∷ a → 𝑂 𝔻

class ToDoubleᴾ a where dblᴾ ∷ a → 𝔻ᴾ
class ToDoubleᴾO a where dblᴾO ∷ a → 𝑂 𝔻ᴾ

class ToNumber a where num ∷ a → ℝ

class ToNumberᴾ a where numᴾ ∷ a → ℝᴾ
class ToNumberᴾO a where numᴾO ∷ a → 𝑂 ℝᴾ

ratΩ ∷ (ToRationalO a,STACK) ⇒ a → ℚ
ratΩ x = case ratO x of {None → error "failed ratΩ conversion";Some n → n}

ratᴾΩ ∷ (ToRationalᴾO a,STACK) ⇒ a → ℚᴾ
ratᴾΩ x = case ratᴾO x of {None → error "failed ratᴾΩ conversion";Some n → n}

dblΩ ∷ (ToDoubleO a,STACK) ⇒ a → 𝔻
dblΩ x = case dblO x of {None → error "failed dblΩ conversion";Some n → n}

dblᴾΩ ∷ (ToDoubleᴾO a,STACK) ⇒ a → 𝔻ᴾ
dblᴾΩ x = case dblᴾO x of {None → error "failed dblᴾΩ conversion";Some n → n}

numᴾΩ ∷ (ToNumberᴾO a,STACK) ⇒ a → ℝᴾ
numᴾΩ x = case numᴾO x of {None → error "failed numᴾΩ conversion";Some n → n}
