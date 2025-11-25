module UVMHS.Core.Data.Arithmetic where

import UVMHS.Core.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data.Option ()

import qualified Prelude as HS
import qualified Data.Ratio as HS

zabs ∷ ℤ → ℕ
zabs = natΩ ∘ abs

qabs ∷ ℚ → ℚᴾ
qabs = ratᴾΩ ∘ abs

dabs ∷ 𝔻 → 𝔻ᴾ
dabs = dblᴾΩ ∘ abs

numer ∷ ℚ → ℤ
numer = HS.numerator

denom ∷ ℚ → ℕ
denom = HS.fromIntegral ∘ HS.denominator

numerᴾ ∷ ℚᴾ → ℕ
numerᴾ = HS.numerator

denomᴾ ∷ ℚᴾ → ℕ
denomᴾ = HS.denominator

round ∷ 𝔻 → ℤ
round = HS.round

truncate ∷ 𝔻 → ℤ
truncate = HS.truncate

ceiling ∷ 𝔻 → ℤ
ceiling = HS.ceiling

floor ∷ 𝔻 → ℤ
floor = HS.floor

truncateᴾ ∷ 𝔻ᴾ → ℕ
truncateᴾ = HS.truncate ∘ un𝔻ᴾ

ceilingᴾ ∷ 𝔻ᴾ → ℕ
ceilingᴾ = HS.ceiling ∘ un𝔻ᴾ

floorᴾ ∷ 𝔻ᴾ → ℕ
floorᴾ = HS.floor ∘ un𝔻ᴾ

truncateDecimals ∷ ℕ64 → 𝔻 → 𝔻
truncateDecimals n d =
  let scale = 10 ^^ nat n
  in dbl (truncate (d × scale)) / scale

-- ℕ --

instance Zero   ℕ where zero = 0
instance Plus   ℕ where (+)  = (HS.+)
instance Minus  ℕ where (-)  = (HS.-)
instance One    ℕ where one  = 1
instance Times  ℕ where (×)  = (HS.*)
instance DivMod ℕ where (⌿)  = HS.div ; (÷) = HS.mod
instance Pon    ℕ where (^^) = (HS.^)
instance Pow    ℕ where (^)  = (HS.^)

instance POrd   ℕ where (⊑) = (≤)

instance Bot        ℕ where bot  = 0
instance Join       ℕ where (⊔)  = (⩏)
instance Meet       ℕ where (⊓)  = (⩎)
instance Null       ℕ where null = 0
instance Append     ℕ where (⧺)  = (+)

instance Additive       ℕ
instance Multiplicative ℕ
instance JoinLattice    ℕ
instance Monoid         ℕ

instance ToNat ℕ where nat = id
instance ToNatO64 ℕ where
  natO64 n
    | n > HS.fromIntegral (HS.maxBound @ℕ64) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToNatO32 ℕ where
  natO32 n
    | n > HS.fromIntegral (HS.maxBound @ℕ32) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToNatO16 ℕ where
  natO16 n
    | n > HS.fromIntegral (HS.maxBound @ℕ16) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToNatO8 ℕ where
  natO8 n
    | n > HS.fromIntegral (HS.maxBound @ℕ8) = None
    | otherwise = Some $ HS.fromIntegral n

instance ToInt ℕ where int = HS.fromIntegral
instance ToIntO64 ℕ where
  intO64 n
    | n > HS.fromIntegral (HS.maxBound @ℤ64) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToIntO32 ℕ where
  intO32 n
    | n > HS.fromIntegral (HS.maxBound @ℤ32) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToIntO16 ℕ where
  intO16 n
    | n > HS.fromIntegral (HS.maxBound @ℤ16) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToIntO8 ℕ where
  intO8 n
    | n > HS.fromIntegral (HS.maxBound @ℤ8) = None
    | otherwise = Some $ HS.fromIntegral n

instance ToRational  ℕ where rat  = HS.fromIntegral
instance ToRationalᴾ ℕ where ratᴾ = HS.fromIntegral
instance ToDouble    ℕ where dbl  = HS.fromIntegral
instance ToDoubleᴾ   ℕ where dblᴾ = 𝔻ᴾ ∘ HS.fromIntegral
instance ToNumber    ℕ where num  = Integer ∘ int
instance ToNumberᴾ   ℕ where numᴾ = Natural

-- ℕ64 --

instance Zero   ℕ64 where zero = 𝕟64 0
instance Plus   ℕ64 where (+)  = (HS.+)
instance Minus  ℕ64 where (-)  = (HS.-)
instance One    ℕ64 where one  = 𝕟64 1
instance Times  ℕ64 where (×)  = (HS.*)
instance DivMod ℕ64 where (⌿)  = HS.div ; (÷) = HS.mod
instance Pon    ℕ64 where (^^) = (HS.^)
instance Pow    ℕ64 where (^)  = (HS.^)

instance POrd   ℕ64 where (⊑) = (≤)

instance Bot    ℕ64 where bot  = zero
instance Join   ℕ64 where (⊔)  = (⩏)
instance Top    ℕ64 where top  = HS.maxBound
instance Meet   ℕ64 where (⊓)  = (⩎)
instance Null   ℕ64 where null = zero
instance Append ℕ64 where (⧺)  = (+)

instance Additive       ℕ64
instance Multiplicative ℕ64
instance JoinLattice    ℕ64
instance MeetLattice    ℕ64
instance Monoid         ℕ64

instance ToNat   ℕ64 where nat   = HS.fromIntegral
instance ToNat64 ℕ64 where nat64 = id
instance ToNatO32 ℕ64 where
  natO32 n
    | n > HS.fromIntegral (HS.maxBound @ℕ32) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToNatO16 ℕ64 where
  natO16 n
    | n > HS.fromIntegral (HS.maxBound @ℕ16) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToNatO8 ℕ64 where
  natO8 n
    | n > HS.fromIntegral (HS.maxBound @ℕ8) = None
    | otherwise = Some $ HS.fromIntegral n

instance ToInt ℕ64 where int = HS.fromIntegral
instance ToIntO64 ℕ64 where
  intO64 n
    | n > HS.fromIntegral (HS.maxBound @ℤ64) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToIntO32 ℕ64 where
  intO32 n
    | n > HS.fromIntegral (HS.maxBound @ℤ32) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToIntO16 ℕ64 where
  intO16 n
    | n > HS.fromIntegral (HS.maxBound @ℤ16) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToIntO8 ℕ64 where
  intO8 n
    | n > HS.fromIntegral (HS.maxBound @ℤ8) = None
    | otherwise = Some $ HS.fromIntegral n

instance ToRational  ℕ64 where rat  = HS.fromIntegral
instance ToRationalᴾ ℕ64 where ratᴾ = HS.fromIntegral
instance ToDouble    ℕ64 where dbl  = HS.fromIntegral
instance ToDoubleᴾ   ℕ64 where dblᴾ = 𝔻ᴾ ∘ HS.fromIntegral
instance ToNumber    ℕ64 where num  = Integer ∘ int
instance ToNumberᴾ   ℕ64 where numᴾ = Natural ∘ nat

-- ℕ32 --

instance Zero   ℕ32 where zero = 𝕟32 0
instance Plus   ℕ32 where (+)  = (HS.+)
instance Minus  ℕ32 where (-)  = (HS.-)
instance One    ℕ32 where one  = 𝕟32 1
instance Times  ℕ32 where (×)  = (HS.*)
instance DivMod ℕ32 where (⌿)  = HS.div ; (÷) = HS.mod
instance Pon    ℕ32 where (^^) = (HS.^)
instance Pow    ℕ32 where (^)  = (HS.^)

instance POrd   ℕ32 where (⊑) = (≤)

instance Bot    ℕ32 where bot  = 𝕟32 0
instance Join   ℕ32 where (⊔)  = (⩏)
instance Top    ℕ32 where top  = HS.maxBound
instance Meet   ℕ32 where (⊓)  = (⩎)
instance Null   ℕ32 where null = zero
instance Append ℕ32 where (⧺)  = (+)

instance Additive       ℕ32
instance Multiplicative ℕ32
instance JoinLattice    ℕ32
instance MeetLattice    ℕ32
instance Monoid         ℕ32

instance ToNat    ℕ32 where nat   = HS.fromIntegral
instance ToNat64  ℕ32 where nat64 = HS.fromIntegral
instance ToNat32  ℕ32 where nat32 = id
instance ToNatO16 ℕ32 where
  natO16 n
    | n > HS.fromIntegral (HS.maxBound @ℕ16) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToNatO8 ℕ32 where
  natO8 n
    | n > HS.fromIntegral (HS.maxBound @ℕ8) = None
    | otherwise = Some $ HS.fromIntegral n

instance ToInt   ℕ32 where int   = HS.fromIntegral
instance ToInt64 ℕ32 where int64 = HS.fromIntegral
instance ToIntO32 ℕ32 where
  intO32 n
    | n > HS.fromIntegral (HS.maxBound @ℤ32) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToIntO16 ℕ32 where
  intO16 n
    | n > HS.fromIntegral (HS.maxBound @ℤ16) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToIntO8 ℕ32 where
  intO8 n
    | n > HS.fromIntegral (HS.maxBound @ℤ8) = None
    | otherwise = Some $ HS.fromIntegral n

instance ToRational  ℕ32 where rat  = HS.fromIntegral
instance ToRationalᴾ ℕ32 where ratᴾ = HS.fromIntegral
instance ToDouble    ℕ32 where dbl  = HS.fromIntegral
instance ToDoubleᴾ   ℕ32 where dblᴾ = 𝔻ᴾ ∘ HS.fromIntegral
instance ToNumber    ℕ32 where num  = Integer ∘ int
instance ToNumberᴾ   ℕ32 where numᴾ = Natural ∘ nat

-- ℕ16 --

instance Zero   ℕ16 where zero = 𝕟16 0
instance Plus   ℕ16 where (+)  = (HS.+)
instance Minus  ℕ16 where (-)  = (HS.-)
instance One    ℕ16 where one  = 𝕟16 1
instance Times  ℕ16 where (×)  = (HS.*)
instance DivMod ℕ16 where (⌿)  = HS.div ; (÷) = HS.mod
instance Pon    ℕ16 where (^^) = (HS.^)
instance Pow    ℕ16 where (^)  = (HS.^)

instance POrd   ℕ16 where (⊑) = (≤)

instance Bot    ℕ16 where bot  = 𝕟16 0
instance Join   ℕ16 where (⊔)  = (⩏)
instance Top    ℕ16 where top  = HS.maxBound
instance Meet   ℕ16 where (⊓)  = (⩎)
instance Null   ℕ16 where null = zero
instance Append ℕ16 where (⧺)  = (+)

instance Additive       ℕ16
instance Multiplicative ℕ16
instance JoinLattice    ℕ16
instance MeetLattice    ℕ16
instance Monoid         ℕ16

instance ToNat   ℕ16 where nat   = HS.fromIntegral
instance ToNat64 ℕ16 where nat64 = HS.fromIntegral
instance ToNat32 ℕ16 where nat32 = HS.fromIntegral
instance ToNat16 ℕ16 where nat16 = id
instance ToNatO8 ℕ16 where
  natO8 n
    | n > HS.fromIntegral (HS.maxBound @ℕ8) = None
    | otherwise = Some $ HS.fromIntegral n

instance ToInt   ℕ16 where int   = HS.fromIntegral
instance ToInt64 ℕ16 where int64 = HS.fromIntegral
instance ToInt32 ℕ16 where int32 = HS.fromIntegral
instance ToIntO16 ℕ16 where
  intO16 n
    | n > HS.fromIntegral (HS.maxBound @ℤ16) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToIntO8 ℕ16 where
  intO8 n
    | n > HS.fromIntegral (HS.maxBound @ℤ8) = None
    | otherwise = Some $ HS.fromIntegral n

instance ToRational  ℕ16 where rat  = HS.fromIntegral
instance ToRationalᴾ ℕ16 where ratᴾ = HS.fromIntegral
instance ToDouble    ℕ16 where dbl  = HS.fromIntegral
instance ToDoubleᴾ   ℕ16 where dblᴾ = 𝔻ᴾ ∘ HS.fromIntegral
instance ToNumber    ℕ16 where num  = Integer ∘ int
instance ToNumberᴾ   ℕ16 where numᴾ = Natural ∘ nat

-- ℕ8 --

instance Zero   ℕ8 where zero = 𝕟8 0
instance Plus   ℕ8 where (+)  = (HS.+)
instance Minus  ℕ8 where (-)  = (HS.-)
instance One    ℕ8 where one  = 𝕟8 1
instance Times  ℕ8 where (×)  = (HS.*)
instance DivMod ℕ8 where (⌿)  = HS.div ; (÷) = HS.mod
instance Pon    ℕ8 where (^^) = (HS.^)
instance Pow    ℕ8 where (^)  = (HS.^)

instance POrd   ℕ8 where (⊑) = (≤)

instance Bot    ℕ8 where bot  = 𝕟8 0
instance Join   ℕ8 where (⊔)  = (⩏)
instance Top    ℕ8 where top  = HS.maxBound
instance Meet   ℕ8 where (⊓)  = (⩎)
instance Null   ℕ8 where null = zero
instance Append ℕ8 where (⧺)  = (+)

instance Additive       ℕ8
instance Multiplicative ℕ8
instance JoinLattice    ℕ8
instance MeetLattice    ℕ8
instance Monoid         ℕ8

instance ToNat   ℕ8 where nat   = HS.fromIntegral
instance ToNat64 ℕ8 where nat64 = HS.fromIntegral
instance ToNat32 ℕ8 where nat32 = HS.fromIntegral
instance ToNat16 ℕ8 where nat16 = HS.fromIntegral
instance ToNat8  ℕ8 where nat8  = id

instance ToInt   ℕ8 where int   = HS.fromIntegral
instance ToInt64 ℕ8 where int64 = HS.fromIntegral
instance ToInt32 ℕ8 where int32 = HS.fromIntegral
instance ToInt16 ℕ8 where int16 = HS.fromIntegral
instance ToIntO8 ℕ8 where
  intO8 n
    | n > HS.fromIntegral (HS.maxBound @ℤ8) = None
    | otherwise = Some $ HS.fromIntegral n

instance ToRational  ℕ8 where rat  = HS.fromIntegral
instance ToRationalᴾ ℕ8 where ratᴾ = HS.fromIntegral
instance ToDouble    ℕ8 where dbl  = HS.fromIntegral
instance ToDoubleᴾ   ℕ8 where dblᴾ = 𝔻ᴾ ∘ HS.fromIntegral
instance ToNumber    ℕ8 where num  = Integer ∘ int
instance ToNumberᴾ   ℕ8 where numᴾ = Natural ∘ nat

-- ℤ --

instance Zero   ℤ where zero = 𝕫 0
instance Plus   ℤ where (+)  = (HS.+)
instance Minus  ℤ where (-)  = (HS.-)
instance One    ℤ where one  = 𝕫 1
instance Times  ℤ where (×)  = (HS.*)
instance DivMod ℤ where (⌿)  = HS.div ; (÷) = HS.mod
instance Pon    ℤ where (^^) = (HS.^)
instance Abs    ℤ where abs  = HS.abs

instance POrd   ℤ where (⊑) = (≤)

instance Join   ℤ where (⊔)  = (⩏)
instance Meet   ℤ where (⊓)  = (⩎)
instance Null   ℤ where null = zero
instance Append ℤ where (⧺)  = (+)

instance Additive       ℤ
instance Multiplicative ℤ
instance Monoid         ℤ

instance ToNatO ℤ where
  natO i
    | i < 𝕫 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO64 ℤ where
  natO64 i
    | i < 𝕫 0 = None
    | i > HS.fromIntegral (HS.maxBound @ℕ64) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO32 ℤ where
  natO32 i
    | i < 𝕫 0 = None
    | i > HS.fromIntegral (HS.maxBound @ℕ32) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO16 ℤ where
  natO16 i
    | i < 𝕫 0 = None
    | i > HS.fromIntegral (HS.maxBound @ℕ16) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO8 ℤ where
  natO8 i
    | i < 𝕫 0 = None
    | i > HS.fromIntegral (HS.maxBound @ℕ8) = None
    | otherwise = Some $ HS.fromIntegral i

instance ToInt ℤ where int = id
instance ToIntO64 ℤ where
  intO64 i
    | i < HS.fromIntegral (HS.minBound @ℤ64) = None
    | i > HS.fromIntegral (HS.maxBound @ℤ64) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToIntO32 ℤ where
  intO32 i
    | i < HS.fromIntegral (HS.minBound @ℤ32) = None
    | i > HS.fromIntegral (HS.maxBound @ℤ32) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToIntO16 ℤ where
  intO16 i
    | i < HS.fromIntegral (HS.minBound @ℤ16) = None
    | i > HS.fromIntegral (HS.maxBound @ℤ16) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToIntO8 ℤ where
  intO8 i
    | i < HS.fromIntegral (HS.minBound @ℤ8) = None
    | i > HS.fromIntegral (HS.maxBound @ℤ8) = None
    | otherwise = Some $ HS.fromIntegral i

instance ToRational ℤ where rat = HS.fromIntegral
instance ToRationalᴾO ℤ where
  ratᴾO i
    | i < 𝕫 0 = None
    | otherwise = Some $ HS.fromIntegral i

instance ToDouble ℤ where dbl = HS.fromIntegral
instance ToDoubleᴾO ℤ where
  dblᴾO i
    | i < 𝕫 0 = None
    | otherwise = Some $ 𝔻ᴾ $ HS.fromIntegral i

instance ToNumber ℤ where num = Integer
instance ToNumberᴾO ℤ where
  numᴾO i = case natO i of
    None → None
    Some n → Some $ Natural n

-- ℤ64 --

instance Zero   ℤ64 where zero = 𝕫64 0
instance Plus   ℤ64 where (+)  = (HS.+)
instance Minus  ℤ64 where (-)  = (HS.-)
instance One    ℤ64 where one  = 𝕫64 1
instance Times  ℤ64 where (×)  = (HS.*)
instance DivMod ℤ64 where (⌿)  = HS.div ; (÷) = HS.mod
instance Pon    ℤ64 where (^^) = (HS.^)
instance Abs    ℤ64 where abs  = HS.abs

instance POrd   ℤ64 where (⊑) = (≤)

instance Bot    ℤ64 where bot  = HS.minBound
instance Join   ℤ64 where (⊔)  = (⩏)
instance Top    ℤ64 where top  = HS.maxBound
instance Meet   ℤ64 where (⊓)  = (⩎)
instance Null   ℤ64 where null = zero
instance Append ℤ64 where (⧺)  = (+)

instance Additive       ℤ64
instance Multiplicative ℤ64
instance JoinLattice    ℤ64
instance MeetLattice    ℤ64
instance Monoid         ℤ64

instance ToNatO ℤ64 where
  natO i
    | i < 𝕫64 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO64 ℤ64 where
  natO64 i
    | i < 𝕫64 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO32 ℤ64 where
  natO32 i
    | i < 𝕫64 0 = None
    | i > HS.fromIntegral (HS.maxBound @ℕ32) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO16 ℤ64 where
  natO16 i
    | i < 𝕫64 0 = None
    | i > HS.fromIntegral (HS.maxBound @ℕ16) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO8 ℤ64 where
  natO8 i
    | i < 𝕫64 0 = None
    | i > HS.fromIntegral (HS.maxBound @ℕ8) = None
    | otherwise = Some $ HS.fromIntegral i

instance ToInt    ℤ64 where int   = HS.fromIntegral
instance ToInt64  ℤ64 where int64 = id
instance ToIntO32 ℤ64 where
  intO32 i
    | i < HS.fromIntegral (HS.minBound @ℤ32) = None
    | i > HS.fromIntegral (HS.maxBound @ℤ32) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToIntO16 ℤ64 where
  intO16 i
    | i < HS.fromIntegral (HS.minBound @ℤ16) = None
    | i > HS.fromIntegral (HS.maxBound @ℤ16) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToIntO8 ℤ64 where
  intO8 i
    | i < HS.fromIntegral (HS.minBound @ℤ8) = None
    | i > HS.fromIntegral (HS.maxBound @ℤ8) = None
    | otherwise = Some $ HS.fromIntegral i

instance ToRational ℤ64 where rat = HS.fromIntegral
instance ToRationalᴾO ℤ64 where
  ratᴾO i
    | i < 𝕫64 0 = None
    | otherwise = Some $ HS.fromIntegral i

instance ToDouble ℤ64 where dbl = HS.fromIntegral
instance ToDoubleᴾO ℤ64 where
  dblᴾO i
    | i < 𝕫64 0 = None
    | otherwise = Some $ 𝔻ᴾ $ HS.fromIntegral i

instance ToNumber ℤ64 where num = Integer ∘ int
instance ToNumberᴾO ℤ64 where
  numᴾO i = case natO i of
    None → None
    Some n → Some $ Natural n

-- ℤ32 --

instance Zero   ℤ32 where zero = 𝕫32 0
instance Plus   ℤ32 where (+)  = (HS.+)
instance Minus  ℤ32 where (-)  = (HS.-)
instance One    ℤ32 where one  = 𝕫32 1
instance Times  ℤ32 where (×)  = (HS.*)
instance DivMod ℤ32 where (⌿)  = HS.div ; (÷) = HS.mod
instance Pon    ℤ32 where (^^) = (HS.^)
instance Abs    ℤ32 where abs  = HS.abs

instance POrd   ℤ32 where (⊑) = (≤)

instance Bot    ℤ32 where bot  = HS.minBound
instance Join   ℤ32 where (⊔)  = (⩏)
instance Top    ℤ32 where top  = HS.maxBound
instance Meet   ℤ32 where (⊓)  = (⩎)
instance Null   ℤ32 where null = zero
instance Append ℤ32 where (⧺)  = (+)

instance Additive       ℤ32
instance Multiplicative ℤ32
instance JoinLattice    ℤ32
instance MeetLattice    ℤ32
instance Monoid         ℤ32

instance ToNatO ℤ32 where
  natO i
    | i < 𝕫32 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO64 ℤ32 where
  natO64 i
    | i < 𝕫32 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO32 ℤ32 where
  natO32 i
    | i < 𝕫32 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO16 ℤ32 where
  natO16 i
    | i < 𝕫32 0 = None
    | i > HS.fromIntegral (HS.maxBound @ℕ16) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO8 ℤ32 where
  natO8 i
    | i < 𝕫32 0 = None
    | i > HS.fromIntegral (HS.maxBound @ℕ8) = None
    | otherwise = Some $ HS.fromIntegral i

instance ToInt   ℤ32 where int   = HS.fromIntegral
instance ToInt64 ℤ32 where int64 = HS.fromIntegral
instance ToInt32 ℤ32 where int32 = id
instance ToIntO16 ℤ32 where
  intO16 i
    | i < HS.fromIntegral (HS.minBound @ℤ16) = None
    | i > HS.fromIntegral (HS.maxBound @ℤ16) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToIntO8 ℤ32 where
  intO8 i
    | i < HS.fromIntegral (HS.minBound @ℤ8) = None
    | i > HS.fromIntegral (HS.maxBound @ℤ8) = None
    | otherwise = Some $ HS.fromIntegral i

instance ToRational ℤ32 where rat = HS.fromIntegral
instance ToRationalᴾO ℤ32 where
  ratᴾO i
    | i < 𝕫32 0 = None
    | otherwise = Some $ HS.fromIntegral i

instance ToDouble ℤ32 where dbl = HS.fromIntegral
instance ToDoubleᴾO ℤ32 where
  dblᴾO i
    | i < 𝕫32 0 = None
    | otherwise = Some $ 𝔻ᴾ $ HS.fromIntegral i

instance ToNumber ℤ32 where num = Integer ∘ int
instance ToNumberᴾO ℤ32 where
  numᴾO i = case natO i of
    None → None
    Some n → Some $ Natural n

-- ℤ16 --

instance Zero   ℤ16 where zero = 𝕫16 0
instance Plus   ℤ16 where (+)  = (HS.+)
instance Minus  ℤ16 where (-)  = (HS.-)
instance One    ℤ16 where one  = 𝕫16 1
instance Times  ℤ16 where (×)  = (HS.*)
instance DivMod ℤ16 where (⌿)  = HS.div ; (÷) = HS.mod
instance Pon    ℤ16 where (^^) = (HS.^)
instance Abs    ℤ16 where abs  = HS.abs

instance POrd   ℤ16 where (⊑) = (≤)

instance Bot    ℤ16 where bot  = HS.minBound
instance Join   ℤ16 where (⊔)  = (⩏)
instance Top    ℤ16 where top  = HS.maxBound
instance Meet   ℤ16 where (⊓)  = (⩎)
instance Null   ℤ16 where null = zero
instance Append ℤ16 where (⧺)  = (+)

instance Additive       ℤ16
instance Multiplicative ℤ16
instance JoinLattice    ℤ16
instance MeetLattice    ℤ16
instance Monoid         ℤ16

instance ToNatO ℤ16 where
  natO i
    | i < 𝕫16 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO64 ℤ16 where
  natO64 i
    | i < 𝕫16 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO32 ℤ16 where
  natO32 i
    | i < 𝕫16 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO16 ℤ16 where
  natO16 i
    | i < 𝕫16 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO8 ℤ16 where
  natO8 i
    | i < 𝕫16 0 = None
    | i > HS.fromIntegral (HS.maxBound @ℕ8) = None
    | otherwise = Some $ HS.fromIntegral i

instance ToInt   ℤ16 where int   = HS.fromIntegral
instance ToInt64 ℤ16 where int64 = HS.fromIntegral
instance ToInt32 ℤ16 where int32 = HS.fromIntegral
instance ToInt16 ℤ16 where int16 = id
instance ToIntO8 ℤ16 where
  intO8 i
    | i < HS.fromIntegral (HS.minBound @ℤ8) = None
    | i > HS.fromIntegral (HS.maxBound @ℤ8) = None
    | otherwise = Some $ HS.fromIntegral i

instance ToRational ℤ16 where rat = HS.fromIntegral
instance ToRationalᴾO ℤ16 where
  ratᴾO i
    | i < 𝕫16 0 = None
    | otherwise = Some $ HS.fromIntegral i

instance ToDouble ℤ16 where dbl = HS.fromIntegral
instance ToDoubleᴾO ℤ16 where
  dblᴾO i
    | i < 𝕫16 0 = None
    | otherwise = Some $ 𝔻ᴾ $ HS.fromIntegral i

instance ToNumber ℤ16 where num = Integer ∘ int
instance ToNumberᴾO ℤ16 where
  numᴾO i = case natO i of
    None → None
    Some n → Some $ Natural n

-- ℤ8 --

instance Zero   ℤ8 where zero = 𝕫8 0
instance Plus   ℤ8 where (+)  = (HS.+)
instance Minus  ℤ8 where (-)  = (HS.-)
instance One    ℤ8 where one  = 𝕫8 1
instance Times  ℤ8 where (×)  = (HS.*)
instance DivMod ℤ8 where (⌿)  = HS.div ; (÷) = HS.mod
instance Pon    ℤ8 where (^^) = (HS.^)
instance Abs    ℤ8 where abs  = HS.abs

instance POrd   ℤ8 where (⊑) = (≤)

instance Bot    ℤ8 where bot  = HS.minBound
instance Join   ℤ8 where (⊔)  = (⩏)
instance Top    ℤ8 where top  = HS.maxBound
instance Meet   ℤ8 where (⊓)  = (⩎)
instance Null   ℤ8 where null = zero
instance Append ℤ8 where (⧺)  = (+)

instance Additive       ℤ8
instance Multiplicative ℤ8
instance JoinLattice    ℤ8
instance MeetLattice    ℤ8
instance Monoid         ℤ8

instance ToNatO ℤ8 where
  natO i
    | i < 𝕫8 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO64 ℤ8 where
  natO64 i
    | i < 𝕫8 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO32 ℤ8 where
  natO32 i
    | i < 𝕫8 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO16 ℤ8 where
  natO16 i
    | i < 𝕫8 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO8  ℤ8 where
  natO8 i
    | i < 𝕫8 0 = None
    | otherwise = Some $ HS.fromIntegral i

instance ToInt   ℤ8 where int   = HS.fromIntegral
instance ToInt64 ℤ8 where int64 = HS.fromIntegral
instance ToInt32 ℤ8 where int32 = HS.fromIntegral
instance ToInt16 ℤ8 where int16 = HS.fromIntegral
instance ToInt8  ℤ8 where int8  = id

instance ToRational ℤ8 where rat = HS.fromIntegral
instance ToRationalᴾO ℤ8 where
  ratᴾO i
    | i < 𝕫8 0 = None
    | otherwise = Some $ HS.fromIntegral i

instance ToDouble ℤ8 where dbl = HS.fromIntegral
instance ToDoubleᴾO ℤ8 where
  dblᴾO i
    | i < 𝕫8 0 = None
    | otherwise = Some $ 𝔻ᴾ $ HS.fromIntegral i

instance ToNumber ℤ8 where num = Integer ∘ int
instance ToNumberᴾO ℤ8 where
  numᴾO i = case natO i of
    None → None
    Some n → Some $ Natural n

-- ℚ --

instance Zero   ℚ where zero = 𝕢 0
instance Plus   ℚ where (+)  = (HS.+)
instance Minus  ℚ where (-)  = (HS.-)
instance One    ℚ where one  = 𝕢 1
instance Times  ℚ where (×)  = (HS.*)
instance Divide ℚ where (/)  = (HS./)
instance Pon    ℚ where (^^) = (HS.^)
instance Abs    ℚ where abs  = HS.abs

instance POrd   ℚ where (⊑) = (≤)

instance Bot    ℚ where bot  = 𝕢 0
instance Join   ℚ where (⊔)  = (⩏)
instance Meet   ℚ where (⊓)  = (⩎)
instance Null   ℚ where null = zero
instance Append ℚ where (⧺)  = (+)

instance Additive       ℚ
instance Multiplicative ℚ
instance JoinLattice    ℚ
instance Monoid         ℚ

instance ToNatO ℚ where
  natO q
    | denom q ≢ 𝕟 1 = None
    | otherwise = natO $ numer q
instance ToNatO64 ℚ where
  natO64 q
    | denom q ≢ 𝕟 1 = None
    | otherwise = natO64 $ numer q
instance ToNatO32 ℚ where
  natO32 q
    | denom q ≢ 𝕟 1 = None
    | otherwise = natO32 $ numer q
instance ToNatO16 ℚ where
  natO16 q
    | denom q ≢ 𝕟 1 = None
    | otherwise = natO16 $ numer q
instance ToNatO8 ℚ where
  natO8 q
    | denom q ≢ 𝕟 1 = None
    | otherwise = natO8 $ numer q
instance ToIntO ℚ where
  intO q
    | denom q ≢ 𝕟 1 = None
    | otherwise = Some $ numer q
instance ToIntO64 ℚ where
  intO64 q
    | denom q ≢ 𝕟 1 = None
    | otherwise = intO64 $ numer q
instance ToIntO32 ℚ where
  intO32 q
    | denom q ≢ 𝕟 1 = None
    | otherwise = intO32 $ numer q
instance ToIntO16 ℚ where
  intO16 q
    | denom q ≢ 𝕟 1 = None
    | otherwise = intO16 $ numer q
instance ToIntO8 ℚ where
  intO8 q
    | denom q ≢ 𝕟 1 = None
    | otherwise = intO8 $ numer q

instance ToRational ℚ where rat = id
instance ToRationalᴾO ℚ where
  ratᴾO q
    | numer q < 𝕫 0 = None
    | otherwise = Some $ HS.fromRational q

instance ToDouble ℚ where dbl = HS.fromRational
instance ToDoubleᴾO ℚ where
  dblᴾO q
    | numer q < 𝕫 0 = None
    | otherwise = Some $ 𝔻ᴾ $ HS.fromRational q

instance ToNumber ℚ where num = Rational
instance ToNumberᴾO ℚ where
  numᴾO q = case ratᴾO q of
    None → None
    Some qᴾ → Some $ Rationalᴾ qᴾ

-- ℚᴾ --

instance Zero   ℚᴾ where zero = 𝕢ᴾ 0
instance Plus   ℚᴾ where (+)  = (HS.+)
instance Minus  ℚᴾ where (-)  = (HS.-)
instance One    ℚᴾ where one  = 𝕢ᴾ 1
instance Times  ℚᴾ where (×)  = (HS.*)
instance Divide ℚᴾ where (/)  = (HS./)
instance Pon    ℚᴾ where (^^) = (HS.^)

instance POrd   ℚᴾ where (⊑) = (≤)

instance Bot    ℚᴾ where bot  = 𝕢ᴾ 0
instance Join   ℚᴾ where (⊔)  = (⩏)
instance Meet   ℚᴾ where (⊓)  = (⩎)
instance Null   ℚᴾ where null = zero
instance Append ℚᴾ where (⧺)  = (+)

instance Additive       ℚᴾ
instance Multiplicative ℚᴾ
instance JoinLattice    ℚᴾ
instance Monoid         ℚᴾ

instance ToNatO ℚᴾ where
  natO q
    | denomᴾ q ≢ 𝕟 1 = None
    | otherwise = Some $ numerᴾ q
instance ToNatO64 ℚᴾ where
  natO64 q
    | denomᴾ q ≢ 𝕟 1 = None
    | otherwise = natO64 $ numerᴾ q
instance ToNatO32 ℚᴾ where
  natO32 q
    | denomᴾ q ≢ 𝕟 1 = None
    | otherwise = natO32 $ numerᴾ q
instance ToNatO16 ℚᴾ where
  natO16 q
    | denomᴾ q ≢ 𝕟 1 = None
    | otherwise = natO16 $ numerᴾ q
instance ToNatO8 ℚᴾ where
  natO8 q
    | denomᴾ q ≢ 𝕟 1 = None
    | otherwise = natO8 $ numerᴾ q
instance ToIntO ℚᴾ where
  intO q
    | denomᴾ q ≢ 𝕟 1 = None
    | otherwise = Some $ int $ numerᴾ q
instance ToIntO64 ℚᴾ where
  intO64 q
    | denomᴾ q ≢ 𝕟 1 = None
    | otherwise = intO64 $ numerᴾ q
instance ToIntO32 ℚᴾ where
  intO32 q
    | denomᴾ q ≢ 𝕟 1 = None
    | otherwise = intO32 $ numerᴾ q
instance ToIntO16 ℚᴾ where
  intO16 q
    | denomᴾ q ≢ 𝕟 1 = None
    | otherwise = intO16 $ numerᴾ q
instance ToIntO8 ℚᴾ where
  intO8 q
    | denomᴾ q ≢ 𝕟 1 = None
    | otherwise = intO8 $ numerᴾ q

instance ToRational  ℚᴾ where rat  = HS.toRational
instance ToRationalᴾ ℚᴾ where ratᴾ = id
instance ToDouble    ℚᴾ where dbl  = HS.fromRational ∘ rat
instance ToDoubleᴾ   ℚᴾ where dblᴾ = 𝔻ᴾ ∘ dbl
instance ToNumber    ℚᴾ where num  = Rational ∘ rat
instance ToNumberᴾ   ℚᴾ where numᴾ = Rationalᴾ

-- 𝔻 --

instance Zero   𝔻 where zero = 0.0
instance Plus   𝔻 where (+)  = (HS.+)
instance Minus  𝔻 where (-)  = (HS.-)
instance One    𝔻 where one  = 1.0
instance Times  𝔻 where (×)  = (HS.*)
instance Divide 𝔻 where (/)  = (HS./)
instance Pon    𝔻 where (^^) = (HS.^)
instance Pow    𝔻 where (^)  = (HS.**)
instance Root   𝔻 where root = HS.sqrt
instance Log    𝔻 where log  = HS.log
instance Efn    𝔻 where efn  = HS.exp
instance Sin    𝔻 where sin  = HS.sin
instance Cos    𝔻 where cos  = HS.cos
instance Abs    𝔻 where abs  = HS.abs

instance POrd   𝔻 where (⊑) = (≤)

instance Bot    𝔻 where bot  = neg 1.0/0.0
instance Join   𝔻 where (⊔)  = (⩏)
instance Top    𝔻 where top  = 1.0/0.0
instance Meet   𝔻 where (⊓)  = (⩎)
instance Null   𝔻 where null = zero
instance Append 𝔻 where (⧺)  = (+)

instance Additive       𝔻
instance Multiplicative 𝔻
instance JoinLattice    𝔻
instance MeetLattice    𝔻
instance Monoid         𝔻

instance ToNatO 𝔻 where
  natO d =
    let i = truncate d
    in case d ≡ dbl i of
      True → natO i
      False → None
instance ToNatO64 𝔻 where
  natO64 d =
    let i = truncate d
    in case d ≡ dbl i of
      True → natO64 i
      False → None
instance ToNatO32 𝔻 where
  natO32 d =
    let i = truncate d
    in case d ≡ dbl i of
      True → natO32 i
      False → None
instance ToNatO16 𝔻 where
  natO16 d =
    let i = truncate d
    in case d ≡ dbl i of
      True → natO16 i
      False → None
instance ToNatO8 𝔻 where
  natO8 d =
    let i = truncate d
    in case d ≡ dbl i of
      True → natO8 i
      False → None
instance ToIntO 𝔻 where
  intO d =
    let i = truncate d
    in case d ≡ dbl i of
      True → Some i
      False → None
instance ToIntO64 𝔻 where
  intO64 d =
    let i = truncate d
    in case d ≡ dbl i of
      True → intO64 i
      False → None
instance ToIntO32 𝔻 where
  intO32 d =
    let i = truncate d
    in case d ≡ dbl i of
      True → intO32 i
      False → None
instance ToIntO16 𝔻 where
  intO16 d =
    let i = truncate d
    in case d ≡ dbl i of
      True → intO16 i
      False → None
instance ToIntO8 𝔻 where
  intO8 d =
    let i = truncate d
    in case d ≡ dbl i of
      True → intO8 i
      False → None

instance ToRational 𝔻 where rat = HS.realToFrac
instance ToRationalᴾO 𝔻 where
  ratᴾO d
    | d < 0.0 = None
    | otherwise = Some $ HS.realToFrac d

instance ToDouble 𝔻 where dbl = id
instance ToDoubleᴾO 𝔻 where
  dblᴾO d
    | d < 0.0 = None
    | otherwise = Some $ 𝔻ᴾ d

instance ToNumber 𝔻 where num = Double
instance ToNumberᴾO 𝔻 where
  numᴾO d = case dblᴾO d of
    None → None
    Some dᴾ → Some $ Doubleᴾ dᴾ

-- 𝔻ᴾ --

minus𝔻ᴾ ∷ 𝔻ᴾ → 𝔻ᴾ → 𝔻ᴾ
minus𝔻ᴾ p₁ p₂
  | p₁ < p₂ = error "𝔻ᴾ: subtraction: LHS is smaller than RHS"
  | otherwise = p₁ HS.- p₂

instance Zero   𝔻ᴾ where zero = 𝕕ᴾ 0.0
instance Plus   𝔻ᴾ where (+)  = (HS.+)
instance Minus  𝔻ᴾ where (-)  = minus𝔻ᴾ
instance One    𝔻ᴾ where one  = 𝕕ᴾ 1.0
instance Times  𝔻ᴾ where (×)  = (HS.*)
instance Divide 𝔻ᴾ where (/)  = (HS./)
instance Pon    𝔻ᴾ where (^^) = (HS.^)
instance Pow    𝔻ᴾ where (^)  = (HS.**)
instance Root   𝔻ᴾ where root = HS.sqrt
instance Log    𝔻ᴾ where log  = HS.log
instance Efn    𝔻ᴾ where efn  = HS.exp
instance Sin    𝔻ᴾ where sin  = HS.sin
instance Cos    𝔻ᴾ where cos  = HS.cos

instance POrd   𝔻ᴾ where (⊑) = (≤)

instance Bot    𝔻ᴾ where bot  = 𝕕ᴾ 0.0
instance Join   𝔻ᴾ where (⊔)  = (⩏)
instance Top    𝔻ᴾ where top  = 𝕕ᴾ 1.0 / 𝕕ᴾ 0.0
instance Meet   𝔻ᴾ where (⊓)  = (⩎)
instance Null   𝔻ᴾ where null = zero
instance Append 𝔻ᴾ where (⧺)  = (+)

instance Additive       𝔻ᴾ
instance Multiplicative 𝔻ᴾ
instance JoinLattice    𝔻ᴾ
instance MeetLattice    𝔻ᴾ
instance Monoid         𝔻ᴾ

instance ToNatO 𝔻ᴾ where
  natO d =
    let n = truncateᴾ d
    in case d ≡ dblᴾ n of
      True → Some n
      False → None
instance ToNatO64 𝔻ᴾ where
  natO64 d =
    let n = truncateᴾ d
    in case d ≡ dblᴾ n of
      True → natO64 n
      False → None
instance ToNatO32 𝔻ᴾ where
  natO32 d =
    let n = truncateᴾ d
    in case d ≡ dblᴾ n of
      True → natO32 n
      False → None
instance ToNatO16 𝔻ᴾ where
  natO16 d =
    let n = truncateᴾ d
    in case d ≡ dblᴾ n of
      True → natO16 n
      False → None
instance ToNatO8 𝔻ᴾ where
  natO8 d =
    let n = truncateᴾ d
    in case d ≡ dblᴾ n of
      True → natO8 n
      False → None
instance ToIntO 𝔻ᴾ where
  intO d =
    let n = truncateᴾ d
    in case d ≡ dblᴾ n of
      True → Some $ int n
      False → None
instance ToIntO64 𝔻ᴾ where
  intO64 d =
    let n = truncateᴾ d
    in case d ≡ dblᴾ n of
      True → intO64 n
      False → None
instance ToIntO32 𝔻ᴾ where
  intO32 d =
    let n = truncateᴾ d
    in case d ≡ dblᴾ n of
      True → intO32 n
      False → None
instance ToIntO16 𝔻ᴾ where
  intO16 d =
    let n = truncateᴾ d
    in case d ≡ dblᴾ n of
      True → intO16 n
      False → None
instance ToIntO8 𝔻ᴾ where
  intO8 d =
    let n = truncateᴾ d
    in case d ≡ dblᴾ n of
      True → intO8 n
      False → None

instance ToRational 𝔻ᴾ where rat = HS.toRational
instance ToRationalᴾO 𝔻ᴾ where
  ratᴾO d
    | d < 𝕕ᴾ 0.0 = None
    | otherwise = Some $ HS.fromRational $ HS.toRational d

instance ToDouble  𝔻ᴾ where dbl  = un𝔻ᴾ
instance ToDoubleᴾ 𝔻ᴾ where dblᴾ = id
instance ToNumber  𝔻ᴾ where num  = Double ∘ dbl
instance ToNumberᴾ 𝔻ᴾ where numᴾ = Doubleᴾ

-- ℝ and ℝ⁺ --

numberBOp ∷ (ℤ → ℤ → ℝ) → (ℚ → ℚ → ℝ) → (𝔻 → 𝔻 → ℝ) → ℝ → ℝ → ℝ
numberBOp  oZ _oQ _oD (Integer  i₁) (Integer  i₂) = oZ i₁ i₂
numberBOp _oZ  oQ _oD (Rational q₁) (Rational q₂) = oQ q₁ q₂
numberBOp _oZ _oQ  oD (Double   d₁) (Double   d₂) = oD d₁ d₂
numberBOp _oZ  oQ _oD (Integer  i₁) (Rational q₂) = oQ (rat i₁) q₂
numberBOp _oZ  oQ _oD (Rational q₁) (Integer  i₂) = oQ q₁ (rat i₂)
numberBOp _oZ _oQ  oD (Integer  i₁) (Double   d₂) = oD (dbl i₁) d₂
numberBOp _oZ _oQ  oD (Double   d₁) (Integer  i₂) = oD d₁ (dbl i₂)
numberBOp _oZ _oQ  oD (Rational q₁) (Double   d₂) = oD (dbl q₁) d₂
numberBOp _oZ _oQ  oD (Double   d₁) (Rational q₂) = oD d₁ (dbl q₂)

numberBOpᴾ ∷ (ℕ → ℕ → ℝᴾ) → (ℚᴾ → ℚᴾ → ℝᴾ) → (𝔻ᴾ → 𝔻ᴾ → ℝᴾ) → ℝᴾ → ℝᴾ → ℝᴾ
numberBOpᴾ  oZ _oQ _oD (Natural   i₁) (Natural   i₂) = oZ i₁ i₂
numberBOpᴾ _oZ  oQ _oD (Rationalᴾ q₁) (Rationalᴾ q₂) = oQ q₁ q₂
numberBOpᴾ _oZ _oQ  oD (Doubleᴾ   d₁) (Doubleᴾ   d₂) = oD d₁ d₂
numberBOpᴾ _oZ  oQ _oD (Natural   i₁) (Rationalᴾ q₂) = oQ (ratᴾ i₁) q₂
numberBOpᴾ _oZ  oQ _oD (Rationalᴾ q₁) (Natural   i₂) = oQ q₁ (ratᴾ i₂)
numberBOpᴾ _oZ _oQ  oD (Natural   i₁) (Doubleᴾ   d₂) = oD (dblᴾ i₁) d₂
numberBOpᴾ _oZ _oQ  oD (Doubleᴾ   d₁) (Natural   i₂) = oD d₁ (dblᴾ i₂)
numberBOpᴾ _oZ _oQ  oD (Rationalᴾ q₁) (Doubleᴾ   d₂) = oD (dblᴾ q₁) d₂
numberBOpᴾ _oZ _oQ  oD (Doubleᴾ   d₁) (Rationalᴾ q₂) = oD d₁ (dblᴾ q₂)

instance Zero ℝ where
  zero = Integer zero
instance Plus ℝ where
  (+) = numberBOp (Integer ∘∘ (+)) (Rational ∘∘ (+)) $ Double ∘∘ (+)
instance Minus ℝ where
  (-) = numberBOp (Integer ∘∘ (-)) (Rational ∘∘ (-)) $ Double ∘∘ (-)
instance One ℝ where
  one = Integer one
instance Times ℝ where
  (×) = numberBOp (Integer ∘∘ (×)) (Rational ∘∘ (×)) $ Double ∘∘ (×)
instance Divide ℝ where
  (/) = numberBOp (\ i₁ i₂ → Rational $ rat i₁ / rat i₂)
                  (Rational ∘∘ (/))
                $ Double ∘∘ (/)
instance Pon ℝ where
  Integer  m ^^ n = Integer  $ m ^^ n
  Rational q ^^ n = Rational $ q ^^ n
  Double   d ^^ n = Double   $ d ^^ n
instance Pow ℝ where
  (^) = numberBOp (\ i₁ i₂ → Double $ dbl i₁ ^ dbl i₂)
                  (\ q₁ q₂ → Double $ dbl q₁ ^ dbl q₂)
                $ Double ∘∘ (^)
instance Root ℝ where
  root (Integer  i) = Double $ root $ dbl i
  root (Rational q) = Double $ root $ dbl q
  root (Double   d) = Double $ root d
instance Log ℝ where
  log (Integer  i) = Double $ log $ dbl i
  log (Rational q) = Double $ log $ dbl q
  log (Double   d) = Double $ log d
instance Efn ℝ where
  efn (Integer  i) = Double $ efn $ dbl i
  efn (Rational q) = Double $ efn $ dbl q
  efn (Double   d) = Double $ efn d
instance Sin ℝ where
  sin (Integer  i) = Double $ sin $ dbl i
  sin (Rational q) = Double $ sin $ dbl q
  sin (Double   d) = Double $ sin d
instance Cos ℝ where
  cos (Integer  i) = Double $ cos $ dbl i
  cos (Rational q) = Double $ cos $ dbl q
  cos (Double   d) = Double $ cos d
instance Abs ℝ where
  abs (Integer  i) = Integer  $ abs i
  abs (Rational q) = Rational $ abs q
  abs (Double   d) = Double   $ abs d

instance POrd   ℝ where (⊑) = (≤)

instance Bot    ℝ where bot  = zero
instance Join   ℝ where (⊔)  = numberBOp (Integer ∘∘ (⊔)) (Rational ∘∘ (⊔)) (Double ∘∘ (⊔))
instance Meet   ℝ where (⊓)  = numberBOp (Integer ∘∘ (⊓)) (Rational ∘∘ (⊓)) (Double ∘∘ (⊓))
instance Null   ℝ where null = zero
instance Append ℝ where (⧺)  = (+)

instance Additive       ℝ
instance Multiplicative ℝ
instance JoinLattice    ℝ
instance Monoid         ℝ

instance ToNatO ℝ where
  natO (Integer  i) = natO i
  natO (Rational q) = natO q
  natO (Double   d) = natO d
instance ToNatO64 ℝ where
  natO64 (Integer  i) = natO64 i
  natO64 (Rational q) = natO64 q
  natO64 (Double   d) = natO64 d
instance ToNatO32 ℝ where
  natO32 (Integer  i) = natO32 i
  natO32 (Rational q) = natO32 q
  natO32 (Double   d) = natO32 d
instance ToNatO16 ℝ where
  natO16 (Integer  i) = natO16 i
  natO16 (Rational q) = natO16 q
  natO16 (Double   d) = natO16 d
instance ToNatO8 ℝ where
  natO8 (Integer  i) = natO8 i
  natO8 (Rational q) = natO8 q
  natO8 (Double   d) = natO8 d
instance ToIntO ℝ where
  intO (Integer  i) = Some i
  intO (Rational q) = intO q
  intO (Double   d) = intO d
instance ToIntO64 ℝ where
  intO64 (Integer  i) = intO64 i
  intO64 (Rational q) = intO64 q
  intO64 (Double   d) = intO64 d
instance ToIntO32 ℝ where
  intO32 (Integer  i) = intO32 i
  intO32 (Rational q) = intO32 q
  intO32 (Double   d) = intO32 d
instance ToIntO16 ℝ where
  intO16 (Integer  i) = intO16 i
  intO16 (Rational q) = intO16 q
  intO16 (Double   d) = intO16 d
instance ToIntO8 ℝ where
  intO8 (Integer  i) = intO8 i
  intO8 (Rational q) = intO8 q
  intO8 (Double   d) = intO8 d
instance ToRational ℝ where
  rat (Integer  i) = rat i
  rat (Rational q) = q
  rat (Double   d) = rat d
instance ToRationalᴾO ℝ where
  ratᴾO (Integer  i) = ratᴾO i
  ratᴾO (Rational q) = ratᴾO q
  ratᴾO (Double   d) = ratᴾO d
instance ToDouble ℝ where
  dbl (Integer  i) = dbl i
  dbl (Rational q) = dbl q
  dbl (Double   d) = d
instance ToDoubleᴾO ℝ where
  dblᴾO (Integer  i) = dblᴾO i
  dblᴾO (Rational q) = dblᴾO q
  dblᴾO (Double   d) = dblᴾO d

instance Zero ℝᴾ where
  zero = Natural zero
instance Plus ℝᴾ where
  (+) = numberBOpᴾ (Natural ∘∘ (+)) (Rationalᴾ ∘∘ (+)) (Doubleᴾ ∘∘ (+))
instance Minus ℝᴾ where
  (-) = numberBOpᴾ (Natural ∘∘ (-)) (Rationalᴾ ∘∘ (-)) (Doubleᴾ ∘∘ (-))
instance One ℝᴾ where
  one = Natural one
instance Times ℝᴾ where
  (×) = numberBOpᴾ (Natural ∘∘ (×)) (Rationalᴾ ∘∘ (×)) (Doubleᴾ ∘∘ (×))
instance Divide ℝᴾ where
  (/) = numberBOpᴾ (\ n₁ n₂ → Rationalᴾ $ ratᴾ n₁ / ratᴾ n₂) (Rationalᴾ ∘∘ (/)) (Doubleᴾ ∘∘ (/))
instance Pon ℝᴾ where
  Natural   m ^^ n = Natural   $ m ^^ n
  Rationalᴾ q ^^ n = Rationalᴾ $ q ^^ n
  Doubleᴾ   d ^^ n = Doubleᴾ   $ d ^^ n
instance Pow ℝᴾ where
  (^) = numberBOpᴾ (Natural ∘∘ (^)) (\ qᴾ₁ qᴾ₂ → Doubleᴾ $ dblᴾ qᴾ₁ ^ dblᴾ qᴾ₂) (Doubleᴾ ∘∘ (^))
instance Root ℝᴾ where
  root (Natural   n) = Doubleᴾ $ root $ dblᴾ n
  root (Rationalᴾ q) = Doubleᴾ $ root $ dblᴾ q
  root (Doubleᴾ   d) = Doubleᴾ $ root d
instance Log ℝᴾ where
  log (Natural   n) = Doubleᴾ $ log $ dblᴾ n
  log (Rationalᴾ q) = Doubleᴾ $ log $ dblᴾ q
  log (Doubleᴾ   d) = Doubleᴾ $ log d

instance Bot    ℝᴾ where bot  = zero
instance Join   ℝᴾ where (⊔)  = numberBOpᴾ (Natural ∘∘ (⊔)) (Rationalᴾ ∘∘ (⊔)) (Doubleᴾ ∘∘ (⊔))
instance Meet   ℝᴾ where (⊓)  = numberBOpᴾ (Natural ∘∘ (⊓)) (Rationalᴾ ∘∘ (⊓)) (Doubleᴾ ∘∘ (⊓))
instance Null   ℝᴾ where null = zero
instance Append ℝᴾ where (⧺)  = (+)

instance POrd   ℝᴾ where (⊑) = (≤)

instance Additive       ℝᴾ
instance Multiplicative ℝᴾ
instance JoinLattice    ℝᴾ
instance Monoid         ℝᴾ

instance ToNatO ℝᴾ where
  natO (Natural   n)   = Some n
  natO (Rationalᴾ q) = natO q
  natO (Doubleᴾ   d)   = natO d
instance ToNatO64 ℝᴾ where
  natO64 (Natural   n) = natO64 n
  natO64 (Rationalᴾ q) = natO64 q
  natO64 (Doubleᴾ   d) = natO64 d
instance ToNatO32 ℝᴾ where
  natO32 (Natural   n) = natO32 n
  natO32 (Rationalᴾ q) = natO32 q
  natO32 (Doubleᴾ   d) = natO32 d
instance ToNatO16 ℝᴾ where
  natO16 (Natural   n) = natO16 n
  natO16 (Rationalᴾ q) = natO16 q
  natO16 (Doubleᴾ   d) = natO16 d
instance ToNatO8 ℝᴾ where
  natO8 (Natural   n) = natO8 n
  natO8 (Rationalᴾ q) = natO8 q
  natO8 (Doubleᴾ   d) = natO8 d
instance ToIntO ℝᴾ where
  intO (Natural   n) = Some $ int n
  intO (Rationalᴾ q) = intO q
  intO (Doubleᴾ   d) = intO d
instance ToIntO64 ℝᴾ where
  intO64 (Natural   n) = intO64 n
  intO64 (Rationalᴾ q) = intO64 q
  intO64 (Doubleᴾ   d) = intO64 d
instance ToIntO32 ℝᴾ where
  intO32 (Natural   n) = intO32 n
  intO32 (Rationalᴾ q) = intO32 q
  intO32 (Doubleᴾ   d) = intO32 d
instance ToIntO16 ℝᴾ where
  intO16 (Natural   n) = intO16 n
  intO16 (Rationalᴾ q) = intO16 q
  intO16 (Doubleᴾ   d) = intO16 d
instance ToIntO8 ℝᴾ where
  intO8 (Natural   n) = intO8 n
  intO8 (Rationalᴾ q) = intO8 q
  intO8 (Doubleᴾ   d) = intO8 d
instance ToRational ℝᴾ where
  rat (Natural   n) = rat n
  rat (Rationalᴾ q) = rat q
  rat (Doubleᴾ   d) = rat d
instance ToRationalᴾO ℝᴾ where
  ratᴾO (Natural   n) = Some $ ratᴾ n
  ratᴾO (Rationalᴾ q) = Some q
  ratᴾO (Doubleᴾ   d) = ratᴾO d
instance ToDouble ℝᴾ where
  dbl (Natural   n) = dbl n
  dbl (Rationalᴾ q) = dbl q
  dbl (Doubleᴾ   d) = dbl d
instance ToDoubleᴾ ℝᴾ where
  dblᴾ (Natural   n) = dblᴾ n
  dblᴾ (Rationalᴾ q) = dblᴾ q
  dblᴾ (Doubleᴾ   d) = d
