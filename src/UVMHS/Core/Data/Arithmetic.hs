module UVMHS.Core.Data.Arithmetic where

import UVMHS.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data.Option ()

import qualified Prelude as HS

-- ℕ --

instance Zero ℕ where zero = 0
instance Plus ℕ where (+) = (HS.+)
instance Minus ℕ where (-) = (HS.-)
instance One ℕ where one = 1
instance Times ℕ where (×) = (HS.*)
instance DivMod ℕ where {(⌿) = HS.div;(÷) = HS.mod}
instance Exponential ℕ where (^) = (HS.^)

instance Bot ℕ where bot = 0
instance Join ℕ where (⊔) = (⩏)
instance Meet ℕ where (⊓) = (⩎)
instance Null ℕ where null = 0
instance Append ℕ where (⧺) = (+)

instance Additive ℕ
instance Multiplicative ℕ
instance JoinLattice ℕ
instance Monoid ℕ

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

instance ToRatio ℕ where rio = HS.fromIntegral
instance ToNNDouble ℕ where nndbl = HS.fromIntegral

instance ToDouble ℕ where dbl = HS.fromIntegral
instance ToRational ℕ where rat = HS.fromIntegral


-- ℕ64 --

instance Zero ℕ64 where zero = HS.fromIntegral 0
instance Plus ℕ64 where (+) = (HS.+)
instance Minus ℕ64 where (-) = (HS.-)
instance One ℕ64 where one = HS.fromIntegral 1
instance Times ℕ64 where (×) = (HS.*)
instance DivMod ℕ64 where {(⌿) = HS.div;(÷) = HS.mod}
instance Exponential ℕ64 where (^) = (HS.^)

instance Bot ℕ64 where bot = zero
instance Join ℕ64 where (⊔) = (⩏)
instance Meet ℕ64 where (⊓) = (⩎)
instance Null ℕ64 where null = zero
instance Append ℕ64 where (⧺) = (+)

instance Additive ℕ64
instance Multiplicative ℕ64
instance JoinLattice ℕ64
instance Monoid ℕ64

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

instance ToRatio ℕ64 where rio = HS.fromIntegral
instance ToNNDouble ℕ64 where nndbl = HS.fromIntegral

instance ToDouble ℕ64 where dbl = HS.fromIntegral
instance ToRational ℕ64 where rat = HS.fromIntegral

-- ℕ32 --

instance Zero ℕ32 where zero = HS.fromIntegral 0
instance Plus ℕ32 where (+) = (HS.+)
instance Minus ℕ32 where (-) = (HS.-)
instance One ℕ32 where one = HS.fromIntegral 1
instance Times ℕ32 where (×) = (HS.*)
instance DivMod ℕ32 where {(⌿) = HS.div;(÷) = HS.mod}
instance Exponential ℕ32 where (^) = (HS.^)

instance Bot ℕ32 where bot = HS.fromIntegral 0
instance Join ℕ32 where (⊔) = (⩏)
instance Meet ℕ32 where (⊓) = (⩎)
instance Null ℕ32 where null = zero
instance Append ℕ32 where (⧺) = (+)

instance Additive ℕ32
instance Multiplicative ℕ32
instance JoinLattice ℕ32
instance Monoid ℕ32

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

instance ToRatio ℕ32 where rio = HS.fromIntegral
instance ToNNDouble ℕ32 where nndbl = HS.fromIntegral

instance ToDouble ℕ32 where dbl = HS.fromIntegral
instance ToRational ℕ32 where rat = HS.fromIntegral

-- ℕ16 --

instance Zero ℕ16 where zero = HS.fromIntegral 0
instance Plus ℕ16 where (+) = (HS.+)
instance Minus ℕ16 where (-) = (HS.-)
instance One ℕ16 where one = HS.fromIntegral 1
instance Times ℕ16 where (×) = (HS.*)
instance DivMod ℕ16 where {(⌿) = HS.div;(÷) = HS.mod}
instance Exponential ℕ16 where (^) = (HS.^)

instance Bot ℕ16 where bot = HS.fromIntegral 0
instance Join ℕ16 where (⊔) = (⩏)
instance Meet ℕ16 where (⊓) = (⩎)
instance Null ℕ16 where null = zero
instance Append ℕ16 where (⧺) = (+)

instance Additive ℕ16
instance Multiplicative ℕ16
instance JoinLattice ℕ16
instance Monoid ℕ16

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

instance ToRatio ℕ16 where rio = HS.fromIntegral
instance ToNNDouble ℕ16 where nndbl = HS.fromIntegral

instance ToDouble ℕ16 where dbl = HS.fromIntegral
instance ToRational ℕ16 where rat = HS.fromIntegral

-- ℕ8 --

instance Zero ℕ8 where zero = HS.fromIntegral 0
instance Plus ℕ8 where (+) = (HS.+)
instance Minus ℕ8 where (-) = (HS.-)
instance One ℕ8 where one = HS.fromIntegral 1
instance Times ℕ8 where (×) = (HS.*)
instance DivMod ℕ8 where {(⌿) = HS.div;(÷) = HS.mod}
instance Exponential ℕ8 where (^) = (HS.^)

instance Bot ℕ8 where bot = HS.fromIntegral 0
instance Join ℕ8 where (⊔) = (⩏)
instance Meet ℕ8 where (⊓) = (⩎)
instance Null ℕ8 where null = zero
instance Append ℕ8 where (⧺) = (+)

instance Additive ℕ8
instance Multiplicative ℕ8
instance JoinLattice ℕ8
instance Monoid ℕ8

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

instance ToRatio ℕ8 where rio = HS.fromIntegral
instance ToNNDouble ℕ8 where nndbl = HS.fromIntegral

instance ToDouble ℕ8 where dbl = HS.fromIntegral
instance ToRational ℕ8 where rat = HS.fromIntegral

-- ℤ --

instance Zero ℤ where zero = HS.fromIntegral 0
instance Plus ℤ where (+) = (HS.+)
instance Minus ℤ where (-) = (HS.-)
instance One ℤ where one = HS.fromIntegral 1
instance Times ℤ where (×) = (HS.*)
instance DivMod ℤ where {(⌿) = HS.div;(÷) = HS.mod}

instance Bot ℤ where bot = HS.fromIntegral 0
instance Join ℤ where (⊔) = (⩏)
instance Meet ℤ where (⊓) = (⩎)
instance Null ℤ where null = zero
instance Append ℤ where (⧺) = (+)

instance Additive ℤ
instance Multiplicative ℤ
instance JoinLattice ℤ
instance Monoid ℤ

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

instance ToRatioO ℤ where 
  rioO i
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNNDoubleO ℤ where
  nndblO i
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i

instance ToDouble ℤ where dbl = HS.fromIntegral
instance ToRational ℤ where rat = HS.fromIntegral

-- ℤ64 --

instance Zero ℤ64 where zero = HS.fromIntegral 0
instance Plus ℤ64 where (+) = (HS.+)
instance Minus ℤ64 where (-) = (HS.-)
instance One ℤ64 where one = HS.fromIntegral 1
instance Times ℤ64 where (×) = (HS.*)
instance DivMod ℤ64 where {(⌿) = HS.div;(÷) = HS.mod}

instance Bot ℤ64 where bot = HS.fromIntegral 0
instance Join ℤ64 where (⊔) = (⩏)
instance Meet ℤ64 where (⊓) = (⩎)
instance Null ℤ64 where null = zero
instance Append ℤ64 where (⧺) = (+)

instance JoinLattice ℤ64
instance Additive ℤ64
instance Multiplicative ℤ64
instance Monoid ℤ64

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

instance ToRatioO ℤ64 where 
  rioO i
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNNDoubleO ℤ64 where
  nndblO i
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i

instance ToDouble ℤ64 where dbl = HS.fromIntegral
instance ToRational ℤ64 where rat = HS.fromIntegral

-- ℤ32 --

instance Zero ℤ32 where zero = HS.fromIntegral 0
instance Plus ℤ32 where (+) = (HS.+)
instance Minus ℤ32 where (-) = (HS.-)
instance One ℤ32 where one = HS.fromIntegral 1
instance Times ℤ32 where (×) = (HS.*)
instance DivMod ℤ32 where {(⌿) = HS.div;(÷) = HS.mod}

instance Bot ℤ32 where bot = HS.fromIntegral 0
instance Join ℤ32 where (⊔) = (⩏)
instance Meet ℤ32 where (⊓) = (⩎)
instance Null ℤ32 where null = zero
instance Append ℤ32 where (⧺) = (+)

instance Additive ℤ32
instance Multiplicative ℤ32
instance JoinLattice ℤ32
instance Monoid ℤ32

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

instance ToRatioO ℤ32 where 
  rioO i
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNNDoubleO ℤ32 where
  nndblO i
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i

instance ToDouble ℤ32 where dbl = HS.fromIntegral
instance ToRational ℤ32 where rat = HS.fromIntegral

-- ℤ16 --

instance Zero ℤ16 where zero = HS.fromIntegral 0
instance Plus ℤ16 where (+) = (HS.+)
instance Minus ℤ16 where (-) = (HS.-)
instance One ℤ16 where one = HS.fromIntegral 1
instance Times ℤ16 where (×) = (HS.*)
instance DivMod ℤ16 where {(⌿) = HS.div;(÷) = HS.mod}

instance Bot ℤ16 where bot = HS.fromIntegral 0
instance Join ℤ16 where (⊔) = (⩏)
instance Meet ℤ16 where (⊓) = (⩎)
instance Null ℤ16 where null = zero
instance Append ℤ16 where (⧺) = (+)

instance Additive ℤ16
instance Multiplicative ℤ16
instance JoinLattice ℤ16
instance Monoid ℤ16

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

instance ToRatioO ℤ16 where 
  rioO i
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNNDoubleO ℤ16 where
  nndblO i
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i

instance ToDouble ℤ16 where dbl = HS.fromIntegral
instance ToRational ℤ16 where rat = HS.fromIntegral

-- ℤ8 --

instance Zero ℤ8 where zero = HS.fromIntegral 0
instance Plus ℤ8 where (+) = (HS.+)
instance Minus ℤ8 where (-) = (HS.-)
instance One ℤ8 where one = HS.fromIntegral 1
instance Times ℤ8 where (×) = (HS.*)
instance DivMod ℤ8 where {(⌿) = HS.div;(÷) = HS.mod}

instance Bot ℤ8 where bot = HS.fromIntegral 0
instance Join ℤ8 where (⊔) = (⩏)
instance Meet ℤ8 where (⊓) = (⩎)
instance Null ℤ8 where null = zero
instance Append ℤ8 where (⧺) = (+)

instance Additive ℤ8
instance Multiplicative ℤ8
instance JoinLattice ℤ8
instance Monoid ℤ8

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

instance ToRatioO ℤ8 where 
  rioO i
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNNDoubleO ℤ8 where
  nndblO i
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i

instance ToDouble ℤ8 where dbl = HS.fromIntegral
instance ToRational ℤ8 where rat = HS.fromIntegral

-- 𝕋 --

instance Zero 𝕋 where zero = HS.fromIntegral 0
instance Plus 𝕋 where (+) = (HS.+)
instance Minus 𝕋 where (-) = (HS.-)
instance One 𝕋 where one = HS.fromIntegral 1
instance Times 𝕋 where (×) = (HS.*)
instance Divide 𝕋 where (/) = (HS./)

instance Bot 𝕋 where bot = HS.fromIntegral 0
instance Join 𝕋 where (⊔) = (⩏)
instance Meet 𝕋 where (⊓) = (⩎)
instance Null 𝕋 where null = zero
instance Append 𝕋 where (⧺) = (+)

instance Additive 𝕋
instance Multiplicative 𝕋
instance JoinLattice 𝕋
instance Monoid 𝕋

instance ToRatio 𝕋 where rio = id
instance ToNNDouble 𝕋 where nndbl = HS.realToFrac
instance ToRational 𝕋 where rat = HS.realToFrac
instance ToDouble 𝕋 where dbl = HS.realToFrac

-- ℙ --

instance Zero ℙ where zero = HS.fromIntegral 0
instance Plus ℙ where (+) = (HS.+)
instance Minus ℙ where 
  p₁ - p₂ 
    | p₁ < p₂ = error "ℙ: subtraction: LHS is smaller than RHS"
    | otherwise = p₁ HS.- p₂
instance One ℙ where one = HS.fromIntegral 1
instance Times ℙ where (×) = (HS.*)
instance Divide ℙ where (/) = (HS./)
instance Exponential ℙ where (^) = (HS.**)
instance Root ℙ where root = HS.sqrt
instance Log ℙ where log = HS.log

instance Bot ℙ where bot = HS.fromIntegral 0
instance Join ℙ where (⊔) = (⩏)
instance Top ℙ where top = HS.realToFrac $ 1.0 / 0.0
instance Meet ℙ where (⊓) = (⩎)
instance Null ℙ where null = zero
instance Append ℙ where (⧺) = (+)

instance Additive ℙ
instance Multiplicative ℙ
instance JoinLattice ℙ
instance MeetLattice ℙ
instance Monoid ℙ

instance ToRatio ℙ where rio = HS.realToFrac
instance ToNNDouble ℙ where nndbl = id

instance ToRational ℙ where rat = HS.realToFrac
instance ToDouble ℙ where dbl = HS.realToFrac

-- ℚ --

instance Zero ℚ where zero = HS.fromIntegral 0
instance Plus ℚ where (+) = (HS.+)
instance Minus ℚ where (-) = (HS.-)
instance One ℚ where one = HS.fromIntegral 1
instance Times ℚ where (×) = (HS.*)
instance Divide ℚ where (/) = (HS./)

instance Bot ℚ where bot = HS.fromIntegral 0
instance Join ℚ where (⊔) = (⩏)
instance Meet ℚ where (⊓) = (⩎)
instance Null ℚ where null = zero
instance Append ℚ where (⧺) = (+)

instance Additive ℚ
instance Multiplicative ℚ
instance JoinLattice ℚ
instance Monoid ℚ

instance ToRatioO ℚ where
  rioO q 
    | q < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.realToFrac q
instance ToNNDoubleO ℚ where
  nndblO q
    | q < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.realToFrac q

instance ToRational ℚ where rat = id
instance ToDouble ℚ where dbl = HS.fromRational

-- 𝔻 --

instance Zero 𝔻 where zero = 0.0
instance Plus 𝔻 where (+) = (HS.+)
instance Minus 𝔻 where (-) = (HS.-)
instance One 𝔻 where one = 1.0
instance Times 𝔻 where (×) = (HS.*)
instance Divide 𝔻 where (/) = (HS./)
instance Exponential 𝔻 where (^) = (HS.**)
instance Root 𝔻 where root = HS.sqrt
instance Log 𝔻 where log = HS.log
instance ExponentialFn 𝔻 where exp = HS.exp
instance Sin 𝔻 where sin = HS.sin
instance Cos 𝔻 where cos = HS.cos

instance Bot 𝔻 where bot = neg 1.0/0.0
instance Join 𝔻 where (⊔) = (⩏)
instance Top 𝔻 where top = 1.0/0.0
instance Meet 𝔻 where (⊓) = (⩎)
instance Null 𝔻 where null = zero
instance Append 𝔻 where (⧺) = (+)

instance Additive 𝔻
instance Multiplicative 𝔻
instance JoinLattice 𝔻
instance MeetLattice 𝔻
instance Monoid 𝔻

instance ToRatioO 𝔻 where
  rioO d 
    | d < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.realToFrac d
instance ToNNDoubleO 𝔻 where
  nndblO d 
    | d < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.realToFrac d

instance ToRational 𝔻 where rat = HS.realToFrac
instance ToDouble 𝔻 where dbl = id

-- NNNumber and Number --

nnnumberBOp ∷ (ℕ → ℕ → NNNumber) → (𝕋 → 𝕋 → NNNumber) → (ℙ → ℙ → NNNumber) → NNNumber → NNNumber → NNNumber
nnnumberBOp  oZ _oQ _oD (Natural i₁)  (Natural i₂)  = oZ i₁ i₂
nnnumberBOp _oZ  oQ _oD (Ratio q₁)    (Ratio q₂)    = oQ q₁ q₂
nnnumberBOp _oZ _oQ  oD (NNDouble d₁) (NNDouble d₂) = oD d₁ d₂
nnnumberBOp _oZ  oQ _oD (Natural i₁)  (Ratio q₂)    = oQ (rio i₁) q₂
nnnumberBOp _oZ  oQ _oD (Ratio q₁)    (Natural i₂)  = oQ q₁ (rio i₂)
nnnumberBOp _oZ _oQ  oD (Natural i₁)  (NNDouble d₂) = oD (nndbl i₁) d₂
nnnumberBOp _oZ _oQ  oD (NNDouble d₁) (Natural i₂)  = oD d₁ (nndbl i₂)
nnnumberBOp _oZ _oQ  oD (Ratio q₁)    (NNDouble d₂) = oD (nndbl q₁) d₂
nnnumberBOp _oZ _oQ  oD (NNDouble d₁) (Ratio q₂)    = oD d₁ (nndbl q₂)

numberBOp ∷ (ℤ → ℤ → Number) → (ℚ → ℚ → Number) → (𝔻 → 𝔻 → Number) → Number → Number → Number
numberBOp  oZ _oQ _oD (Integer i₁)  (Integer i₂)  = oZ i₁ i₂
numberBOp _oZ  oQ _oD (Rational q₁) (Rational q₂) = oQ q₁ q₂
numberBOp _oZ _oQ  oD (Double d₁)   (Double d₂)   = oD d₁ d₂
numberBOp _oZ  oQ _oD (Integer i₁)  (Rational q₂) = oQ (rat i₁) q₂
numberBOp _oZ  oQ _oD (Rational q₁) (Integer i₂)  = oQ q₁ (rat i₂)
numberBOp _oZ _oQ  oD (Integer i₁)  (Double d₂)   = oD (dbl i₁) d₂
numberBOp _oZ _oQ  oD (Double d₁)   (Integer i₂)  = oD d₁ (dbl i₂)
numberBOp _oZ _oQ  oD (Rational q₁) (Double d₂)   = oD (dbl q₁) d₂
numberBOp _oZ _oQ  oD (Double d₁)   (Rational q₂) = oD d₁ (dbl q₂)

number ∷ NNNumber → Number
number (Natural n) = Integer $ int n
number (Ratio t) = Rational $ rat t
number (NNDouble p) = Double $ dbl p

nnnumberO ∷ Number → 𝑂 NNNumber
nnnumberO (Integer i) = Natural ^$ natO i
nnnumberO (Rational q) = Ratio ^$ rioO q
nnnumberO (Double d) = NNDouble ^$ nndblO d

instance Zero NNNumber where zero = Natural zero
instance Plus NNNumber where (+) = nnnumberBOp (Natural ∘∘ (+)) (Ratio ∘∘ (+)) (NNDouble ∘∘ (+))
instance Minus NNNumber where (-) = nnnumberBOp (Natural ∘∘ (-)) (Ratio ∘∘ (-)) (NNDouble ∘∘ (-))
instance One NNNumber where one = Natural one
instance Times NNNumber where (×) = nnnumberBOp (Natural ∘∘ (×)) (Ratio ∘∘ (×)) (NNDouble ∘∘ (×))
instance Divide NNNumber where (/) = nnnumberBOp (\ n₁ n₂ → Ratio $ rio n₁ / rio n₂) (Ratio ∘∘ (/)) (NNDouble ∘∘ (/))
instance Exponential NNNumber where (^) = nnnumberBOp (Natural ∘∘ (^)) (\ t₁ t₂ → NNDouble $ nndbl t₁ ^ nndbl t₂) (NNDouble ∘∘ (^))
instance Root NNNumber where
  root (Natural n) = NNDouble $ root $ nndbl n
  root (Ratio t) = NNDouble $ root $ nndbl t
  root (NNDouble d) = NNDouble $ root d
instance Log NNNumber where 
  log (Natural n) = NNDouble $ log $ nndbl n
  log (Ratio t) = NNDouble $ log $ nndbl t
  log (NNDouble d) = NNDouble $ log d

instance Bot NNNumber where bot = zero
instance Join NNNumber where (⊔) = nnnumberBOp (Natural ∘∘ (⊔)) (Ratio ∘∘ (⊔)) (NNDouble ∘∘ (⊔))
instance Meet NNNumber where (⊓) = nnnumberBOp (Natural ∘∘ (⊓)) (Ratio ∘∘ (⊓)) (NNDouble ∘∘ (⊓))
instance Null NNNumber where null = zero
instance Append NNNumber where (⧺) = (+)

instance Additive NNNumber
instance Multiplicative NNNumber
instance JoinLattice NNNumber
instance Monoid NNNumber

instance Zero Number where zero = Integer zero
instance Plus Number where (+) = numberBOp (Integer ∘∘ (+)) (Rational ∘∘ (+)) (Double ∘∘ (+))
instance Minus Number where (-) = numberBOp (Integer ∘∘ (-)) (Rational ∘∘ (-)) (Double ∘∘ (-))
instance One Number where one = Integer one
instance Times Number where (×) = numberBOp (Integer ∘∘ (×)) (Rational ∘∘ (×)) (Double ∘∘ (×))
instance Divide Number where (/) = numberBOp (\ i₁ i₂ → Rational $ rat i₁ / rat i₂) (Rational ∘∘ (/)) (Double ∘∘ (/))
instance Exponential Number where (^) = numberBOp (\ i₁ i₂ → Double $ dbl i₁ ^ dbl i₂) (\ q₁ q₂ → Double $ dbl q₁ ^ dbl q₂) (Double ∘∘ (^))
instance Log Number where 
  log (Integer i) = Double $ log $ dbl i
  log (Rational q) = Double $ log $ dbl q
  log (Double d) = Double $ log d

instance Bot Number where bot = zero
instance Join Number where (⊔) = numberBOp (Integer ∘∘ (⊔)) (Rational ∘∘ (⊔)) (Double ∘∘ (⊔))
instance Meet Number where (⊓) = numberBOp (Integer ∘∘ (⊓)) (Rational ∘∘ (⊓)) (Double ∘∘ (⊓))
instance Null Number where null = zero
instance Append Number where (⧺) = (+)

instance Additive Number
instance Multiplicative Number
instance JoinLattice Number
instance Monoid Number
