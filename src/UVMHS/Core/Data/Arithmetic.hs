module UVMHS.Core.Data.Arithmetic where

import UVMHS.Init
import UVMHS.Core.Classes

import qualified Prelude as HS

-- ℕ --

instance Additive ℕ where {zero = 0;(+) = (HS.+)}
instance Subtractive ℕ where (-) = (HS.-)
instance Multiplicative ℕ where {one = 1;(×) = (HS.*)}
instance TruncateDivisible ℕ where {(⌿) = HS.div;(÷) = HS.mod}
instance Exponential ℕ where (^) = (HS.^)

instance ToNat ℕ where nat = id
instance ToNatO64 ℕ where 
  natO64 n 
    | n > HS.fromIntegral (HS.maxBound @ ℕ64) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToNatO32 ℕ where 
  natO32 n 
    | n > HS.fromIntegral (HS.maxBound @ ℕ32) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToNatO16 ℕ where 
  natO16 n 
    | n > HS.fromIntegral (HS.maxBound @ ℕ16) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToNatO8 ℕ where 
  natO8 n 
    | n > HS.fromIntegral (HS.maxBound @ ℕ8) = None
    | otherwise = Some $ HS.fromIntegral n

instance ToInt ℕ where int = HS.fromIntegral
instance ToIntO64 ℕ where 
  intO64 n 
    | n > HS.fromIntegral (HS.maxBound @ ℤ64) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToIntO32 ℕ where 
  intO32 n 
    | n > HS.fromIntegral (HS.maxBound @ ℤ32) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToIntO16 ℕ where 
  intO16 n 
    | n > HS.fromIntegral (HS.maxBound @ ℤ16) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToIntO8 ℕ where 
  intO8 n 
    | n > HS.fromIntegral (HS.maxBound @ ℤ8) = None
    | otherwise = Some $ HS.fromIntegral n

instance ToDouble ℕ where dbl = HS.fromIntegral

instance Bot ℕ where bot = 0
instance Join ℕ where (⊔) = (⩏)
instance JoinLattice ℕ
instance Meet ℕ where (⊓) = (⩎)

-- ℕ64 --

instance Additive ℕ64 where {zero = HS.fromIntegral 0;(+) = (HS.+)}
instance Subtractive ℕ64 where (-) = (HS.-)
instance Multiplicative ℕ64 where {one = HS.fromIntegral 1;(×) = (HS.*)}
instance TruncateDivisible ℕ64 where {(⌿) = HS.div;(÷) = HS.mod}
instance Exponential ℕ64 where (^) = (HS.^)

instance ToNat ℕ64 where nat = HS.fromIntegral
instance ToNat64 ℕ64 where nat64 = id
instance ToNatO32 ℕ64 where 
  natO32 n 
    | n > HS.fromIntegral (HS.maxBound @ ℕ32) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToNatO16 ℕ64 where 
  natO16 n 
    | n > HS.fromIntegral (HS.maxBound @ ℕ16) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToNatO8 ℕ64 where 
  natO8 n 
    | n > HS.fromIntegral (HS.maxBound @ ℕ8) = None
    | otherwise = Some $ HS.fromIntegral n

instance ToInt ℕ64 where int = HS.fromIntegral
instance ToIntO64 ℕ64 where 
  intO64 n 
    | n > HS.fromIntegral (HS.maxBound @ ℤ64) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToIntO32 ℕ64 where 
  intO32 n 
    | n > HS.fromIntegral (HS.maxBound @ ℤ32) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToIntO16 ℕ64 where 
  intO16 n 
    | n > HS.fromIntegral (HS.maxBound @ ℤ16) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToIntO8 ℕ64 where 
  intO8 n 
    | n > HS.fromIntegral (HS.maxBound @ ℤ8) = None
    | otherwise = Some $ HS.fromIntegral n

instance ToDouble ℕ64 where dbl = HS.fromIntegral

instance Bot ℕ64 where bot = HS.fromIntegral 0
instance Join ℕ64 where (⊔) = (⩏)
instance JoinLattice ℕ64
instance Meet ℕ64 where (⊓) = (⩎)

-- ℕ32 --

instance Additive ℕ32 where {zero = HS.fromIntegral 0;(+) = (HS.+)}
instance Subtractive ℕ32 where (-) = (HS.-)
instance Multiplicative ℕ32 where {one = HS.fromIntegral 1;(×) = (HS.*)}
instance TruncateDivisible ℕ32 where {(⌿) = HS.div;(÷) = HS.mod}
instance Exponential ℕ32 where (^) = (HS.^)

instance ToNat ℕ32 where nat = HS.fromIntegral
instance ToNat64 ℕ32 where nat64 = HS.fromIntegral
instance ToNat32 ℕ32 where nat32 = id
instance ToNatO16 ℕ32 where 
  natO16 n 
    | n > HS.fromIntegral (HS.maxBound @ ℕ16) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToNatO8 ℕ32 where 
  natO8 n 
    | n > HS.fromIntegral (HS.maxBound @ ℕ8) = None
    | otherwise = Some $ HS.fromIntegral n

instance ToInt ℕ32 where int = HS.fromIntegral
instance ToInt64 ℕ32 where int64 = HS.fromIntegral
instance ToIntO32 ℕ32 where 
  intO32 n 
    | n > HS.fromIntegral (HS.maxBound @ ℤ32) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToIntO16 ℕ32 where 
  intO16 n 
    | n > HS.fromIntegral (HS.maxBound @ ℤ16) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToIntO8 ℕ32 where 
  intO8 n 
    | n > HS.fromIntegral (HS.maxBound @ ℤ8) = None
    | otherwise = Some $ HS.fromIntegral n

instance ToDouble ℕ32 where dbl = HS.fromIntegral

instance Bot ℕ32 where bot = HS.fromIntegral 0
instance Join ℕ32 where (⊔) = (⩏)
instance JoinLattice ℕ32
instance Meet ℕ32 where (⊓) = (⩎)

-- ℕ16 --

instance Additive ℕ16 where {zero = HS.fromIntegral 0;(+) = (HS.+)}
instance Subtractive ℕ16 where (-) = (HS.-)
instance Multiplicative ℕ16 where {one = HS.fromIntegral 1;(×) = (HS.*)}
instance TruncateDivisible ℕ16 where {(⌿) = HS.div;(÷) = HS.mod}
instance Exponential ℕ16 where (^) = (HS.^)

instance ToNat ℕ16 where nat = HS.fromIntegral
instance ToNat64 ℕ16 where nat64 = HS.fromIntegral
instance ToNat32 ℕ16 where nat32 = HS.fromIntegral
instance ToNat16 ℕ16 where nat16 = id
instance ToNatO8 ℕ16 where 
  natO8 n 
    | n > HS.fromIntegral (HS.maxBound @ ℕ8) = None
    | otherwise = Some $ HS.fromIntegral n

instance ToInt ℕ16 where int = HS.fromIntegral
instance ToInt64 ℕ16 where int64 = HS.fromIntegral
instance ToInt32 ℕ16 where int32 = HS.fromIntegral
instance ToIntO16 ℕ16 where 
  intO16 n 
    | n > HS.fromIntegral (HS.maxBound @ ℤ16) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToIntO8 ℕ16 where 
  intO8 n 
    | n > HS.fromIntegral (HS.maxBound @ ℤ8) = None
    | otherwise = Some $ HS.fromIntegral n

instance ToDouble ℕ16 where dbl = HS.fromIntegral

instance Bot ℕ16 where bot = HS.fromIntegral 0
instance Join ℕ16 where (⊔) = (⩏)
instance JoinLattice ℕ16
instance Meet ℕ16 where (⊓) = (⩎)

-- ℕ8 --

instance Additive ℕ8 where {zero = HS.fromIntegral 0;(+) = (HS.+)}
instance Subtractive ℕ8 where (-) = (HS.-)
instance Multiplicative ℕ8 where {one = HS.fromIntegral 1;(×) = (HS.*)}
instance TruncateDivisible ℕ8 where {(⌿) = HS.div;(÷) = HS.mod}
instance Exponential ℕ8 where (^) = (HS.^)

instance ToNat ℕ8 where nat = HS.fromIntegral
instance ToNat64 ℕ8 where nat64 = HS.fromIntegral
instance ToNat32 ℕ8 where nat32 = HS.fromIntegral
instance ToNat16 ℕ8 where nat16 = HS.fromIntegral
instance ToNat8  ℕ8 where nat8  = id

instance ToInt ℕ8 where int = HS.fromIntegral
instance ToInt64 ℕ8 where int64 = HS.fromIntegral
instance ToInt32 ℕ8 where int32 = HS.fromIntegral
instance ToInt16 ℕ8 where int16 = HS.fromIntegral
instance ToIntO8 ℕ8 where 
  intO8 n 
    | n > HS.fromIntegral (HS.maxBound @ ℤ8) = None
    | otherwise = Some $ HS.fromIntegral n

instance ToDouble ℕ8 where dbl = HS.fromIntegral

instance Bot ℕ8 where bot = HS.fromIntegral 0
instance Join ℕ8 where (⊔) = (⩏)
instance JoinLattice ℕ8
instance Meet ℕ8 where (⊓) = (⩎)

-- ℤ --

instance Additive ℤ where {zero = HS.fromIntegral 0;(+) = (HS.+)}
instance Subtractive ℤ where (-) = (HS.-)
instance Multiplicative ℤ where {one = HS.fromIntegral 1;(×) = (HS.*)}
instance TruncateDivisible ℤ where {(⌿) = HS.div;(÷) = HS.mod}
instance Exponential ℤ where (^) = (HS.^)

instance ToNatO ℤ where 
  natO i 
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO64 ℤ where 
  natO64 i 
    | i < HS.fromIntegral 0 = None
    | i > HS.fromIntegral (HS.maxBound @ ℕ64) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO32 ℤ where 
  natO32 i 
    | i < HS.fromIntegral 0 = None
    | i > HS.fromIntegral (HS.maxBound @ ℕ32) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO16 ℤ where 
  natO16 i 
    | i < HS.fromIntegral 0 = None
    | i > HS.fromIntegral (HS.maxBound @ ℕ16) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO8 ℤ where 
  natO8 i 
    | i < HS.fromIntegral 0 = None
    | i > HS.fromIntegral (HS.maxBound @ ℕ8) = None
    | otherwise = Some $ HS.fromIntegral i

instance ToInt ℤ where int = id
instance ToIntO64 ℤ where 
  intO64 i 
    | i < HS.fromIntegral (HS.minBound @ ℤ64) = None
    | i > HS.fromIntegral (HS.maxBound @ ℤ64) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToIntO32 ℤ where 
  intO32 i 
    | i < HS.fromIntegral (HS.minBound @ ℤ32) = None
    | i > HS.fromIntegral (HS.maxBound @ ℤ32) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToIntO16 ℤ where 
  intO16 i 
    | i < HS.fromIntegral (HS.minBound @ ℤ16) = None
    | i > HS.fromIntegral (HS.maxBound @ ℤ16) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToIntO8 ℤ where 
  intO8 i 
    | i < HS.fromIntegral (HS.minBound @ ℤ8) = None
    | i > HS.fromIntegral (HS.maxBound @ ℤ8) = None
    | otherwise = Some $ HS.fromIntegral i

instance ToDouble ℤ where dbl = HS.fromIntegral

instance Bot ℤ where bot = HS.fromIntegral 0
instance Join ℤ where (⊔) = (⩏)
instance JoinLattice ℤ
instance Meet ℤ where (⊓) = (⩎)

-- ℤ64 --

instance Additive ℤ64 where {zero = HS.fromIntegral 0;(+) = (HS.+)}
instance Subtractive ℤ64 where (-) = (HS.-)
instance Multiplicative ℤ64 where {one = HS.fromIntegral 1;(×) = (HS.*)}
instance TruncateDivisible ℤ64 where {(⌿) = HS.div;(÷) = HS.mod}
instance Exponential ℤ64 where (^) = (HS.^)

instance ToNatO ℤ64 where 
  natO i
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO64 ℤ64 where 
  natO64 i
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO32 ℤ64 where 
  natO32 i 
    | i < HS.fromIntegral 0 = None
    | i > HS.fromIntegral (HS.maxBound @ ℕ32) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO16 ℤ64 where 
  natO16 i 
    | i < HS.fromIntegral 0 = None
    | i > HS.fromIntegral (HS.maxBound @ ℕ16) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO8 ℤ64 where 
  natO8 i 
    | i < HS.fromIntegral 0 = None
    | i > HS.fromIntegral (HS.maxBound @ ℕ8) = None
    | otherwise = Some $ HS.fromIntegral i

instance ToInt ℤ64 where int = HS.fromIntegral
instance ToInt64 ℤ64 where int64 = id
instance ToIntO32 ℤ64 where 
  intO32 i 
    | i < HS.fromIntegral (HS.minBound @ ℤ32) = None
    | i > HS.fromIntegral (HS.maxBound @ ℤ32) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToIntO16 ℤ64 where 
  intO16 i 
    | i < HS.fromIntegral (HS.minBound @ ℤ16) = None
    | i > HS.fromIntegral (HS.maxBound @ ℤ16) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToIntO8 ℤ64 where 
  intO8 i 
    | i < HS.fromIntegral (HS.minBound @ ℤ8) = None
    | i > HS.fromIntegral (HS.maxBound @ ℤ8) = None
    | otherwise = Some $ HS.fromIntegral i

instance ToDouble ℤ64 where dbl = HS.fromIntegral

instance Bot ℤ64 where bot = HS.fromIntegral 0
instance Join ℤ64 where (⊔) = (⩏)
instance JoinLattice ℤ64
instance Meet ℤ64 where (⊓) = (⩎)

-- ℤ32 --

instance Additive ℤ32 where {zero = HS.fromIntegral 0;(+) = (HS.+)}
instance Subtractive ℤ32 where (-) = (HS.-)
instance Multiplicative ℤ32 where {one = HS.fromIntegral 1;(×) = (HS.*)}
instance TruncateDivisible ℤ32 where {(⌿) = HS.div;(÷) = HS.mod}
instance Exponential ℤ32 where (^) = (HS.^)

instance ToNatO ℤ32 where
  natO i 
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO64 ℤ32 where
  natO64 i 
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO32 ℤ32 where
  natO32 i 
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO16 ℤ32 where 
  natO16 i 
    | i < HS.fromIntegral 0 = None
    | i > HS.fromIntegral (HS.maxBound @ ℕ16) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO8 ℤ32 where 
  natO8 i 
    | i < HS.fromIntegral 0 = None
    | i > HS.fromIntegral (HS.maxBound @ ℕ8) = None
    | otherwise = Some $ HS.fromIntegral i

instance ToInt ℤ32 where int = HS.fromIntegral
instance ToInt64 ℤ32 where int64 = HS.fromIntegral
instance ToInt32 ℤ32 where int32 = id
instance ToIntO16 ℤ32 where 
  intO16 i 
    | i < HS.fromIntegral (HS.minBound @ ℤ16) = None
    | i > HS.fromIntegral (HS.maxBound @ ℤ16) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToIntO8 ℤ32 where 
  intO8 i 
    | i < HS.fromIntegral (HS.minBound @ ℤ8) = None
    | i > HS.fromIntegral (HS.maxBound @ ℤ8) = None
    | otherwise = Some $ HS.fromIntegral i

instance ToDouble ℤ32 where dbl = HS.fromIntegral

instance Bot ℤ32 where bot = HS.fromIntegral 0
instance Join ℤ32 where (⊔) = (⩏)
instance JoinLattice ℤ32
instance Meet ℤ32 where (⊓) = (⩎)

-- ℤ16 --

instance Additive ℤ16 where {zero = HS.fromIntegral 0;(+) = (HS.+)}
instance Subtractive ℤ16 where (-) = (HS.-)
instance Multiplicative ℤ16 where {one = HS.fromIntegral 1;(×) = (HS.*)}
instance TruncateDivisible ℤ16 where {(⌿) = HS.div;(÷) = HS.mod}
instance Exponential ℤ16 where (^) = (HS.^)

instance ToNatO ℤ16 where 
  natO i 
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO64 ℤ16 where 
  natO64 i 
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO32 ℤ16 where 
  natO32 i 
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO16 ℤ16 where 
  natO16 i 
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO8 ℤ16 where 
  natO8 i 
    | i < HS.fromIntegral 0 = None
    | i > HS.fromIntegral (HS.maxBound @ ℕ8) = None
    | otherwise = Some $ HS.fromIntegral i

instance ToInt ℤ16 where int = HS.fromIntegral
instance ToInt64 ℤ16 where int64 = HS.fromIntegral
instance ToInt32 ℤ16 where int32 = HS.fromIntegral
instance ToInt16 ℤ16 where int16 = id
instance ToIntO8 ℤ16 where 
  intO8 i 
    | i < HS.fromIntegral (HS.minBound @ ℤ8) = None
    | i > HS.fromIntegral (HS.maxBound @ ℤ8) = None
    | otherwise = Some $ HS.fromIntegral i

instance ToDouble ℤ16 where dbl = HS.fromIntegral

instance Bot ℤ16 where bot = HS.fromIntegral 0
instance Join ℤ16 where (⊔) = (⩏)
instance JoinLattice ℤ16
instance Meet ℤ16 where (⊓) = (⩎)

-- ℤ8 --

instance Additive ℤ8 where {zero = HS.fromIntegral 0;(+) = (HS.+)}
instance Subtractive ℤ8 where (-) = (HS.-)
instance Multiplicative ℤ8 where {one = HS.fromIntegral 1;(×) = (HS.*)}
instance TruncateDivisible ℤ8 where {(⌿) = HS.div;(÷) = HS.mod}
instance Exponential ℤ8 where (^) = (HS.^)

instance ToNatO ℤ8 where
  natO i 
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO64 ℤ8 where
  natO64 i 
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO32 ℤ8 where
  natO32 i 
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO16 ℤ8 where
  natO16 i 
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO8  ℤ8 where
  natO8 i 
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i

instance ToInt ℤ8 where int = HS.fromIntegral
instance ToInt64 ℤ8 where int64 = HS.fromIntegral
instance ToInt32 ℤ8 where int32 = HS.fromIntegral
instance ToInt16 ℤ8 where int16 = HS.fromIntegral
instance ToInt8 ℤ8 where int8 = id

instance ToDouble ℤ8 where dbl = HS.fromIntegral

instance Bot ℤ8 where bot = HS.fromIntegral 0
instance Join ℤ8 where (⊔) = (⩏)
instance JoinLattice ℤ8
instance Meet ℤ8 where (⊓) = (⩎)

-- 𝔻 --

instance Additive 𝔻 where {zero = 0.0;(+) = (HS.+)}
instance Subtractive 𝔻 where (-) = (HS.-)
instance Multiplicative 𝔻 where {one = 1.0;(×) = (HS.*)}
instance Divisible 𝔻 where (/) = (HS./)
instance Exponential 𝔻 where (^) = (HS.**)

instance ToDouble 𝔻 where dbl = id

instance Bot 𝔻 where bot = neg 1.0/0.0
instance Join 𝔻 where (⊔) = (⩏)
instance JoinLattice 𝔻
instance Top 𝔻 where top = 1.0/0.0
instance Meet 𝔻 where (⊓) = (⩎)
instance MeetLattice 𝔻

sqrt ∷ 𝔻 → 𝔻
sqrt = HS.sqrt

log ∷ 𝔻 → 𝔻
log = HS.log
