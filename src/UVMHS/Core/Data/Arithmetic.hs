module UVMHS.Core.Data.Arithmetic where

  -- {-# INLINE natO #-}
  -- {-# INLINE natO64 #-}
  -- {-# INLINE natO32 #-}
  -- {-# INLINE natO16 #-}
  -- {-# INLINE natO8 #-}

import UVMHS.Core.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data.Option ()

import qualified Prelude as HS

-- ℕ --

instance Zero   ℕ where {-# INLINE zero #-} ; zero = 0
instance Plus   ℕ where {-# INLINE (+)  #-} ; (+)  = (HS.+)
instance Minus  ℕ where {-# INLINE (-)  #-} ; (-)  = (HS.-)
instance One    ℕ where {-# INLINE one  #-} ; one  = 1
instance Times  ℕ where {-# INLINE (×)  #-} ; (×)  = (HS.*)
instance DivMod ℕ where {-# INLINE (⌿)  #-} ; (⌿)  = HS.div ; (÷) = HS.mod
instance Pon    ℕ where {-# INLINE (^^) #-} ; (^^) = (HS.^)
instance Pow    ℕ where {-# INLINE (^)  #-} ; (^)  = (HS.^)

instance Bot    ℕ where {-# INLINE bot  #-} ; bot  = 0
instance Join   ℕ where {-# INLINE (⊔)  #-} ; (⊔)  = (⩏)
instance Meet   ℕ where {-# INLINE (⊓)  #-} ; (⊓)  = (⩎)
instance Null   ℕ where {-# INLINE null #-} ; null = 0
instance Append ℕ where {-# INLINE (⧺)  #-} ; (⧺)  = (+)

instance Additive       ℕ
instance Multiplicative ℕ
instance JoinLattice    ℕ
instance Monoid         ℕ

instance ToNat ℕ where {-# INLINE nat #-} ; nat = id
instance ToNatO64 ℕ where 
  {-# INLINE natO64 #-}
  natO64 n 
    | n > HS.fromIntegral (HS.maxBound @ ℕ64) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToNatO32 ℕ where 
  {-# INLINE natO32 #-}
  natO32 n 
    | n > HS.fromIntegral (HS.maxBound @ ℕ32) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToNatO16 ℕ where 
  {-# INLINE natO16 #-}
  natO16 n 
    | n > HS.fromIntegral (HS.maxBound @ ℕ16) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToNatO8 ℕ where 
  {-# INLINE natO8 #-}
  natO8 n 
    | n > HS.fromIntegral (HS.maxBound @ ℕ8) = None
    | otherwise = Some $ HS.fromIntegral n

instance ToInt ℕ where {-# INLINE int #-} ; int = HS.fromIntegral
instance ToIntO64 ℕ where 
  {-# INLINE intO64 #-}
  intO64 n 
    | n > HS.fromIntegral (HS.maxBound @ ℤ64) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToIntO32 ℕ where 
  {-# INLINE intO32 #-}
  intO32 n 
    | n > HS.fromIntegral (HS.maxBound @ ℤ32) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToIntO16 ℕ where 
  {-# INLINE intO16 #-}
  intO16 n 
    | n > HS.fromIntegral (HS.maxBound @ ℤ16) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToIntO8 ℕ where 
  {-# INLINE intO8 #-}
  intO8 n 
    | n > HS.fromIntegral (HS.maxBound @ ℤ8) = None
    | otherwise = Some $ HS.fromIntegral n

instance ToRational  ℕ where {-# INLINE rat  #-} ; rat  = HS.fromIntegral
instance ToRationalᴾ ℕ where {-# INLINE ratᴾ #-} ; ratᴾ = HS.fromIntegral
instance ToDouble    ℕ where {-# INLINE dbl  #-} ; dbl  = HS.fromIntegral
instance ToDoubleᴾ   ℕ where {-# INLINE dblᴾ #-} ; dblᴾ = 𝔻ᴾ ∘ HS.fromIntegral
instance ToNumber    ℕ where {-# INLINE num  #-} ; num  = Integer ∘ int
instance ToNumberᴾ   ℕ where {-# INLINE numᴾ #-} ; numᴾ = Natural

-- ℕ64 --

instance Zero   ℕ64 where {-# INLINE zero #-} ; zero = HS.fromIntegral 0
instance Plus   ℕ64 where {-# INLINE (+)  #-} ; (+)  = (HS.+)
instance Minus  ℕ64 where {-# INLINE (-)  #-} ; (-)  = (HS.-)
instance One    ℕ64 where {-# INLINE one  #-} ; one  = HS.fromIntegral 1
instance Times  ℕ64 where {-# INLINE (×)  #-} ; (×)  = (HS.*)
instance DivMod ℕ64 where {-# INLINE (⌿)  #-} ; (⌿)  = HS.div ; (÷) = HS.mod
instance Pon    ℕ64 where {-# INLINE (^^) #-} ; (^^) = (HS.^)
instance Pow    ℕ64 where {-# INLINE (^)  #-} ; (^)  = (HS.^)

instance Bot    ℕ64 where {-# INLINE bot  #-} ; bot  = zero
instance Join   ℕ64 where {-# INLINE (⊔)  #-} ; (⊔)  = (⩏)
instance Meet   ℕ64 where {-# INLINE (⊓)  #-} ; (⊓)  = (⩎)
instance Null   ℕ64 where {-# INLINE null #-} ; null = zero
instance Append ℕ64 where {-# INLINE (⧺)  #-} ; (⧺)  = (+)

instance Additive       ℕ64
instance Multiplicative ℕ64
instance JoinLattice    ℕ64
instance Monoid         ℕ64

instance ToNat   ℕ64 where {-# INLINE nat   #-} ; nat   = HS.fromIntegral
instance ToNat64 ℕ64 where {-# INLINE nat64 #-} ; nat64 = id
instance ToNatO32 ℕ64 where 
  {-# INLINE natO32 #-}
  natO32 n 
    | n > HS.fromIntegral (HS.maxBound @ ℕ32) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToNatO16 ℕ64 where 
  {-# INLINE natO16 #-}
  natO16 n 
    | n > HS.fromIntegral (HS.maxBound @ ℕ16) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToNatO8 ℕ64 where 
  {-# INLINE natO8 #-}
  natO8 n 
    | n > HS.fromIntegral (HS.maxBound @ ℕ8) = None
    | otherwise = Some $ HS.fromIntegral n

instance ToInt ℕ64 where {-# INLINE int #-} ; int = HS.fromIntegral
instance ToIntO64 ℕ64 where 
  {-# INLINE intO64 #-}
  intO64 n 
    | n > HS.fromIntegral (HS.maxBound @ ℤ64) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToIntO32 ℕ64 where 
  {-# INLINE intO32 #-}
  intO32 n 
    | n > HS.fromIntegral (HS.maxBound @ ℤ32) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToIntO16 ℕ64 where 
  {-# INLINE intO16 #-}
  intO16 n 
    | n > HS.fromIntegral (HS.maxBound @ ℤ16) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToIntO8 ℕ64 where 
  {-# INLINE intO8 #-}
  intO8 n 
    | n > HS.fromIntegral (HS.maxBound @ ℤ8) = None
    | otherwise = Some $ HS.fromIntegral n

instance ToRational  ℕ64 where {-# INLINE rat  #-} ; rat  = HS.fromIntegral
instance ToRationalᴾ ℕ64 where {-# INLINE ratᴾ #-} ; ratᴾ = HS.fromIntegral
instance ToDouble    ℕ64 where {-# INLINE dbl  #-} ; dbl  = HS.fromIntegral
instance ToDoubleᴾ   ℕ64 where {-# INLINE dblᴾ #-} ; dblᴾ = 𝔻ᴾ ∘ HS.fromIntegral
instance ToNumber    ℕ64 where {-# INLINE num  #-} ; num  = Integer ∘ int
instance ToNumberᴾ   ℕ64 where {-# INLINE numᴾ #-} ; numᴾ = Natural ∘ nat

-- ℕ32 --

instance Zero   ℕ32 where {-# INLINE zero #-} ; zero = HS.fromIntegral 0
instance Plus   ℕ32 where {-# INLINE (+)  #-} ; (+)  = (HS.+)
instance Minus  ℕ32 where {-# INLINE (-)  #-} ; (-)  = (HS.-)
instance One    ℕ32 where {-# INLINE one  #-} ; one  = HS.fromIntegral 1
instance Times  ℕ32 where {-# INLINE (×)  #-} ; (×)  = (HS.*)
instance DivMod ℕ32 where {-# INLINE (⌿)  #-} ; (⌿)  = HS.div ; (÷) = HS.mod
instance Pon    ℕ32 where {-# INLINE (^^) #-} ; (^^) = (HS.^)
instance Pow    ℕ32 where {-# INLINE (^)  #-} ; (^)  = (HS.^)

instance Bot    ℕ32 where {-# INLINE bot  #-} ; bot  = HS.fromIntegral 0
instance Join   ℕ32 where {-# INLINE (⊔)  #-} ; (⊔)  = (⩏)
instance Meet   ℕ32 where {-# INLINE (⊓)  #-} ; (⊓)  = (⩎)
instance Null   ℕ32 where {-# INLINE null #-} ; null = zero
instance Append ℕ32 where {-# INLINE (⧺)  #-} ; (⧺)  = (+)

instance Additive       ℕ32
instance Multiplicative ℕ32
instance JoinLattice    ℕ32
instance Monoid         ℕ32

instance ToNat    ℕ32 where {-# INLINE nat   #-} ; nat   = HS.fromIntegral
instance ToNat64  ℕ32 where {-# INLINE nat64 #-} ; nat64 = HS.fromIntegral
instance ToNat32  ℕ32 where {-# INLINE nat32 #-} ; nat32 = id
instance ToNatO16 ℕ32 where 
  {-# INLINE natO16 #-}
  natO16 n 
    | n > HS.fromIntegral (HS.maxBound @ ℕ16) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToNatO8 ℕ32 where 
  {-# INLINE natO8 #-}
  natO8 n 
    | n > HS.fromIntegral (HS.maxBound @ ℕ8) = None
    | otherwise = Some $ HS.fromIntegral n

instance ToInt   ℕ32 where {-# INLINE int   #-} ; int   = HS.fromIntegral
instance ToInt64 ℕ32 where {-# INLINE int64 #-} ; int64 = HS.fromIntegral
instance ToIntO32 ℕ32 where 
  {-# INLINE intO32 #-}
  intO32 n 
    | n > HS.fromIntegral (HS.maxBound @ ℤ32) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToIntO16 ℕ32 where 
  {-# INLINE intO16 #-}
  intO16 n 
    | n > HS.fromIntegral (HS.maxBound @ ℤ16) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToIntO8 ℕ32 where 
  {-# INLINE intO8 #-}
  intO8 n 
    | n > HS.fromIntegral (HS.maxBound @ ℤ8) = None
    | otherwise = Some $ HS.fromIntegral n

instance ToRational  ℕ32 where {-# INLINE rat  #-} ; rat  = HS.fromIntegral
instance ToRationalᴾ ℕ32 where {-# INLINE ratᴾ #-} ; ratᴾ = HS.fromIntegral
instance ToDouble    ℕ32 where {-# INLINE dbl  #-} ; dbl  = HS.fromIntegral
instance ToDoubleᴾ   ℕ32 where {-# INLINE dblᴾ #-} ; dblᴾ = 𝔻ᴾ ∘ HS.fromIntegral
instance ToNumber    ℕ32 where {-# INLINE num  #-} ; num  = Integer ∘ int
instance ToNumberᴾ   ℕ32 where {-# INLINE numᴾ #-} ; numᴾ = Natural ∘ nat

-- ℕ16 --

instance Zero   ℕ16 where {-# INLINE zero #-} ; zero = HS.fromIntegral 0
instance Plus   ℕ16 where {-# INLINE (+)  #-} ; (+)  = (HS.+)
instance Minus  ℕ16 where {-# INLINE (-)  #-} ; (-)  = (HS.-)
instance One    ℕ16 where {-# INLINE one  #-} ; one  = HS.fromIntegral 1
instance Times  ℕ16 where {-# INLINE (×)  #-} ; (×)  = (HS.*)
instance DivMod ℕ16 where {-# INLINE (⌿)  #-} ; (⌿)  = HS.div ; (÷) = HS.mod
instance Pon    ℕ16 where {-# INLINE (^^) #-} ; (^^) = (HS.^)
instance Pow    ℕ16 where {-# INLINE (^)  #-} ; (^)  = (HS.^)

instance Bot    ℕ16 where {-# INLINE bot  #-} ; bot  = HS.fromIntegral 0
instance Join   ℕ16 where {-# INLINE (⊔)  #-} ; (⊔)  = (⩏)
instance Meet   ℕ16 where {-# INLINE (⊓)  #-} ; (⊓)  = (⩎)
instance Null   ℕ16 where {-# INLINE null #-} ; null = zero
instance Append ℕ16 where {-# INLINE (⧺)  #-} ; (⧺)  = (+)

instance Additive       ℕ16
instance Multiplicative ℕ16
instance JoinLattice    ℕ16
instance Monoid         ℕ16

instance ToNat   ℕ16 where {-# INLINE nat   #-} ; nat   = HS.fromIntegral
instance ToNat64 ℕ16 where {-# INLINE nat64 #-} ; nat64 = HS.fromIntegral
instance ToNat32 ℕ16 where {-# INLINE nat32 #-} ; nat32 = HS.fromIntegral
instance ToNat16 ℕ16 where {-# INLINE nat16 #-} ; nat16 = id
instance ToNatO8 ℕ16 where 
  {-# INLINE natO8 #-}
  natO8 n 
    | n > HS.fromIntegral (HS.maxBound @ ℕ8) = None
    | otherwise = Some $ HS.fromIntegral n

instance ToInt   ℕ16 where {-# INLINE int   #-} ; int   = HS.fromIntegral
instance ToInt64 ℕ16 where {-# INLINE int64 #-} ; int64 = HS.fromIntegral
instance ToInt32 ℕ16 where {-# INLINE int32 #-} ; int32 = HS.fromIntegral
instance ToIntO16 ℕ16 where 
  {-# INLINE intO16 #-}
  intO16 n 
    | n > HS.fromIntegral (HS.maxBound @ ℤ16) = None
    | otherwise = Some $ HS.fromIntegral n
instance ToIntO8 ℕ16 where 
  {-# INLINE intO8 #-}
  intO8 n 
    | n > HS.fromIntegral (HS.maxBound @ ℤ8) = None
    | otherwise = Some $ HS.fromIntegral n

instance ToRational  ℕ16 where {-# INLINE rat  #-} ; rat  = HS.fromIntegral
instance ToRationalᴾ ℕ16 where {-# INLINE ratᴾ #-} ; ratᴾ = HS.fromIntegral
instance ToDouble    ℕ16 where {-# INLINE dbl  #-} ; dbl  = HS.fromIntegral
instance ToDoubleᴾ   ℕ16 where {-# INLINE dblᴾ #-} ; dblᴾ = 𝔻ᴾ ∘ HS.fromIntegral
instance ToNumber    ℕ16 where {-# INLINE num  #-} ; num  = Integer ∘ int
instance ToNumberᴾ   ℕ16 where {-# INLINE numᴾ #-} ; numᴾ = Natural ∘ nat

-- ℕ8 --

instance Zero   ℕ8 where {-# INLINE zero #-} ; zero = HS.fromIntegral 0
instance Plus   ℕ8 where {-# INLINE (+)  #-} ; (+)  = (HS.+)
instance Minus  ℕ8 where {-# INLINE (-)  #-} ; (-)  = (HS.-)
instance One    ℕ8 where {-# INLINE one  #-} ; one  = HS.fromIntegral 1
instance Times  ℕ8 where {-# INLINE (×)  #-} ; (×)  = (HS.*)
instance DivMod ℕ8 where {-# INLINE (⌿)  #-} ; (⌿)  = HS.div ; (÷) = HS.mod
instance Pon    ℕ8 where {-# INLINE (^^) #-} ; (^^) = (HS.^)
instance Pow    ℕ8 where {-# INLINE (^)  #-} ; (^)  = (HS.^)

instance Bot    ℕ8 where {-# INLINE bot  #-} ; bot  = HS.fromIntegral 0
instance Join   ℕ8 where {-# INLINE (⊔)  #-} ; (⊔)  = (⩏)
instance Meet   ℕ8 where {-# INLINE (⊓)  #-} ; (⊓)  = (⩎)
instance Null   ℕ8 where {-# INLINE null #-} ; null = zero
instance Append ℕ8 where {-# INLINE (⧺)  #-} ; (⧺)  = (+)

instance Additive       ℕ8
instance Multiplicative ℕ8
instance JoinLattice    ℕ8
instance Monoid         ℕ8

instance ToNat   ℕ8 where {-# INLINE nat   #-} ; nat   = HS.fromIntegral
instance ToNat64 ℕ8 where {-# INLINE nat64 #-} ; nat64 = HS.fromIntegral
instance ToNat32 ℕ8 where {-# INLINE nat32 #-} ; nat32 = HS.fromIntegral
instance ToNat16 ℕ8 where {-# INLINE nat16 #-} ; nat16 = HS.fromIntegral
instance ToNat8  ℕ8 where {-# INLINE nat8  #-} ; nat8  = id

instance ToInt   ℕ8 where {-# INLINE int   #-} ; int   = HS.fromIntegral
instance ToInt64 ℕ8 where {-# INLINE int64 #-} ; int64 = HS.fromIntegral
instance ToInt32 ℕ8 where {-# INLINE int32 #-} ; int32 = HS.fromIntegral
instance ToInt16 ℕ8 where {-# INLINE int16 #-} ; int16 = HS.fromIntegral
instance ToIntO8 ℕ8 where 
  {-# INLINE intO8 #-}
  intO8 n 
    | n > HS.fromIntegral (HS.maxBound @ ℤ8) = None
    | otherwise = Some $ HS.fromIntegral n

instance ToRational  ℕ8 where {-# INLINE rat  #-} ; rat  = HS.fromIntegral
instance ToRationalᴾ ℕ8 where {-# INLINE ratᴾ #-} ; ratᴾ = HS.fromIntegral
instance ToDouble    ℕ8 where {-# INLINE dbl  #-} ; dbl  = HS.fromIntegral
instance ToDoubleᴾ   ℕ8 where {-# INLINE dblᴾ #-} ; dblᴾ = 𝔻ᴾ ∘ HS.fromIntegral
instance ToNumber    ℕ8 where {-# INLINE num  #-} ; num  = Integer ∘ int
instance ToNumberᴾ   ℕ8 where {-# INLINE numᴾ #-} ; numᴾ = Natural ∘ nat

-- ℤ --

instance Zero   ℤ where {-# INLINE zero #-} ; zero = HS.fromIntegral 0
instance Plus   ℤ where {-# INLINE (+)  #-} ; (+)  = (HS.+)
instance Minus  ℤ where {-# INLINE (-)  #-} ; (-)  = (HS.-)
instance One    ℤ where {-# INLINE one  #-} ; one  = HS.fromIntegral 1
instance Times  ℤ where {-# INLINE (×)  #-} ; (×)  = (HS.*)
instance DivMod ℤ where {-# INLINE (⌿)  #-} ; (⌿)  = HS.div ; (÷) = HS.mod
instance Pon    ℤ where {-# INLINE (^^) #-} ; (^^) = (HS.^)

instance Bot    ℤ where {-# INLINE bot  #-} ; bot  = HS.fromIntegral 0
instance Join   ℤ where {-# INLINE (⊔)  #-} ; (⊔)  = (⩏)
instance Meet   ℤ where {-# INLINE (⊓)  #-} ; (⊓)  = (⩎)
instance Null   ℤ where {-# INLINE null #-} ; null = zero
instance Append ℤ where {-# INLINE (⧺)  #-} ; (⧺)  = (+)

instance Additive       ℤ
instance Multiplicative ℤ
instance JoinLattice    ℤ
instance Monoid         ℤ

instance ToNatO ℤ where 
  {-# INLINE natO #-}
  natO i 
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO64 ℤ where 
  {-# INLINE natO64 #-}
  natO64 i 
    | i < HS.fromIntegral 0 = None
    | i > HS.fromIntegral (HS.maxBound @ ℕ64) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO32 ℤ where 
  {-# INLINE natO32 #-}
  natO32 i 
    | i < HS.fromIntegral 0 = None
    | i > HS.fromIntegral (HS.maxBound @ ℕ32) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO16 ℤ where 
  {-# INLINE natO16 #-}
  natO16 i 
    | i < HS.fromIntegral 0 = None
    | i > HS.fromIntegral (HS.maxBound @ ℕ16) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO8 ℤ where 
  {-# INLINE natO8 #-}
  natO8 i 
    | i < HS.fromIntegral 0 = None
    | i > HS.fromIntegral (HS.maxBound @ ℕ8) = None
    | otherwise = Some $ HS.fromIntegral i

instance ToInt ℤ where {-# INLINE int #-} ; int = id
instance ToIntO64 ℤ where 
  {-# INLINE intO64 #-}
  intO64 i 
    | i < HS.fromIntegral (HS.minBound @ ℤ64) = None
    | i > HS.fromIntegral (HS.maxBound @ ℤ64) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToIntO32 ℤ where 
  {-# INLINE intO32 #-}
  intO32 i 
    | i < HS.fromIntegral (HS.minBound @ ℤ32) = None
    | i > HS.fromIntegral (HS.maxBound @ ℤ32) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToIntO16 ℤ where 
  {-# INLINE intO16 #-}
  intO16 i 
    | i < HS.fromIntegral (HS.minBound @ ℤ16) = None
    | i > HS.fromIntegral (HS.maxBound @ ℤ16) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToIntO8 ℤ where 
  {-# INLINE intO8 #-}
  intO8 i 
    | i < HS.fromIntegral (HS.minBound @ ℤ8) = None
    | i > HS.fromIntegral (HS.maxBound @ ℤ8) = None
    | otherwise = Some $ HS.fromIntegral i

instance ToRational ℤ where {-# INLINE rat #-} ; rat = HS.fromIntegral
instance ToRationalᴾO ℤ where 
  {-# INLINE ratᴾO #-}
  ratᴾO i
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i

instance ToDouble ℤ where {-# INLINE dbl #-} ; dbl = HS.fromIntegral
instance ToDoubleᴾO ℤ where
  {-# INLINE dblᴾO #-}
  dblᴾO i
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ 𝔻ᴾ $ HS.fromIntegral i

instance ToNumber ℤ where {-# INLINE num #-} ; num = Integer
instance ToNumberᴾO ℤ where
  {-# INLINE numᴾO #-}
  numᴾO i = case natO i of
    None → None
    Some n → Some $ Natural n

-- ℤ64 --

instance Zero   ℤ64 where {-# INLINE zero #-} ; zero = HS.fromIntegral 0
instance Plus   ℤ64 where {-# INLINE (+)  #-} ; (+)  = (HS.+)
instance Minus  ℤ64 where {-# INLINE (-)  #-} ; (-)  = (HS.-)
instance One    ℤ64 where {-# INLINE one  #-} ; one  = HS.fromIntegral 1
instance Times  ℤ64 where {-# INLINE (×)  #-} ; (×)  = (HS.*)
instance DivMod ℤ64 where {-# INLINE (⌿)  #-} ; (⌿)  = HS.div ; (÷) = HS.mod
instance Pon    ℤ64 where {-# INLINE (^^) #-} ; (^^) = (HS.^)

instance Bot    ℤ64 where {-# INLINE bot  #-} ; bot  = HS.fromIntegral 0
instance Join   ℤ64 where {-# INLINE (⊔)  #-} ; (⊔)  = (⩏)
instance Meet   ℤ64 where {-# INLINE (⊓)  #-} ; (⊓)  = (⩎)
instance Null   ℤ64 where {-# INLINE null #-} ; null = zero
instance Append ℤ64 where {-# INLINE (⧺)  #-} ; (⧺)  = (+)

instance JoinLattice    ℤ64
instance Additive       ℤ64
instance Multiplicative ℤ64
instance Monoid         ℤ64

instance ToNatO ℤ64 where 
  {-# INLINE natO #-}
  natO i
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO64 ℤ64 where 
  {-# INLINE natO64 #-}
  natO64 i
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO32 ℤ64 where 
  {-# INLINE natO32 #-}
  natO32 i 
    | i < HS.fromIntegral 0 = None
    | i > HS.fromIntegral (HS.maxBound @ ℕ32) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO16 ℤ64 where 
  {-# INLINE natO16 #-}
  natO16 i 
    | i < HS.fromIntegral 0 = None
    | i > HS.fromIntegral (HS.maxBound @ ℕ16) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO8 ℤ64 where 
  {-# INLINE natO8 #-}
  natO8 i 
    | i < HS.fromIntegral 0 = None
    | i > HS.fromIntegral (HS.maxBound @ ℕ8) = None
    | otherwise = Some $ HS.fromIntegral i

instance ToInt    ℤ64 where {-# INLINE int   #-} ; int   = HS.fromIntegral
instance ToInt64  ℤ64 where {-# INLINE int64 #-} ; int64 = id
instance ToIntO32 ℤ64 where 
  {-# INLINE intO32 #-}
  intO32 i 
    | i < HS.fromIntegral (HS.minBound @ ℤ32) = None
    | i > HS.fromIntegral (HS.maxBound @ ℤ32) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToIntO16 ℤ64 where 
  {-# INLINE intO16 #-}
  intO16 i 
    | i < HS.fromIntegral (HS.minBound @ ℤ16) = None
    | i > HS.fromIntegral (HS.maxBound @ ℤ16) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToIntO8 ℤ64 where 
  {-# INLINE intO8 #-}
  intO8 i 
    | i < HS.fromIntegral (HS.minBound @ ℤ8) = None
    | i > HS.fromIntegral (HS.maxBound @ ℤ8) = None
    | otherwise = Some $ HS.fromIntegral i

instance ToRational ℤ64 where {-# INLINE rat #-} ; rat = HS.fromIntegral
instance ToRationalᴾO ℤ64 where 
  {-# INLINE ratᴾO #-}
  ratᴾO i
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i

instance ToDouble ℤ64 where {-# INLINE dbl #-} ; dbl = HS.fromIntegral
instance ToDoubleᴾO ℤ64 where
  {-# INLINE dblᴾO #-}
  dblᴾO i
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ 𝔻ᴾ $ HS.fromIntegral i

instance ToNumber ℤ64 where {-# INLINE num #-} ; num = Integer ∘ int
instance ToNumberᴾO ℤ64 where
  {-# INLINE numᴾO #-}
  numᴾO i = case natO i of
    None → None
    Some n → Some $ Natural n

-- ℤ32 --

instance Zero   ℤ32 where {-# INLINE zero #-} ; zero = HS.fromIntegral 0
instance Plus   ℤ32 where {-# INLINE (+)  #-} ; (+)  = (HS.+)
instance Minus  ℤ32 where {-# INLINE (-)  #-} ; (-)  = (HS.-)
instance One    ℤ32 where {-# INLINE one  #-} ; one  = HS.fromIntegral 1
instance Times  ℤ32 where {-# INLINE (×)  #-} ; (×)  = (HS.*)
instance DivMod ℤ32 where {-# INLINE (⌿)  #-} ; (⌿)  = HS.div ; (÷) = HS.mod
instance Pon    ℤ32 where {-# INLINE (^^) #-} ; (^^) = (HS.^)

instance Bot    ℤ32 where {-# INLINE bot  #-} ; bot  = HS.fromIntegral 0
instance Join   ℤ32 where {-# INLINE (⊔)  #-} ; (⊔)  = (⩏)
instance Meet   ℤ32 where {-# INLINE (⊓)  #-} ; (⊓)  = (⩎)
instance Null   ℤ32 where {-# INLINE null #-} ; null = zero
instance Append ℤ32 where {-# INLINE (⧺)  #-} ; (⧺)  = (+)

instance Additive       ℤ32
instance Multiplicative ℤ32
instance JoinLattice    ℤ32
instance Monoid         ℤ32

instance ToNatO ℤ32 where
  {-# INLINE natO #-}
  natO i 
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO64 ℤ32 where
  {-# INLINE natO64 #-}
  natO64 i 
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO32 ℤ32 where
  {-# INLINE natO32 #-}
  natO32 i 
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO16 ℤ32 where 
  {-# INLINE natO16 #-}
  natO16 i 
    | i < HS.fromIntegral 0 = None
    | i > HS.fromIntegral (HS.maxBound @ ℕ16) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO8 ℤ32 where 
  {-# INLINE natO8 #-}
  natO8 i 
    | i < HS.fromIntegral 0 = None
    | i > HS.fromIntegral (HS.maxBound @ ℕ8) = None
    | otherwise = Some $ HS.fromIntegral i

instance ToInt   ℤ32 where {-# INLINE int   #-} ; int   = HS.fromIntegral
instance ToInt64 ℤ32 where {-# INLINE int64 #-} ; int64 = HS.fromIntegral
instance ToInt32 ℤ32 where {-# INLINE int32 #-} ; int32 = id
instance ToIntO16 ℤ32 where 
  {-# INLINE intO16 #-}
  intO16 i 
    | i < HS.fromIntegral (HS.minBound @ ℤ16) = None
    | i > HS.fromIntegral (HS.maxBound @ ℤ16) = None
    | otherwise = Some $ HS.fromIntegral i
instance ToIntO8 ℤ32 where 
  {-# INLINE intO8 #-}
  intO8 i 
    | i < HS.fromIntegral (HS.minBound @ ℤ8) = None
    | i > HS.fromIntegral (HS.maxBound @ ℤ8) = None
    | otherwise = Some $ HS.fromIntegral i

instance ToRational ℤ32 where {-# INLINE rat #-} ; rat = HS.fromIntegral
instance ToRationalᴾO ℤ32 where 
  {-# INLINE ratᴾO #-}
  ratᴾO i
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i

instance ToDouble ℤ32 where {-# INLINE dbl #-} ; dbl = HS.fromIntegral
instance ToDoubleᴾO ℤ32 where
  {-# INLINE dblᴾO #-}
  dblᴾO i
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ 𝔻ᴾ $ HS.fromIntegral i

instance ToNumber ℤ32 where {-# INLINE num #-} ; num = Integer ∘ int
instance ToNumberᴾO ℤ32 where
  {-# INLINE numᴾO #-}
  numᴾO i = case natO i of
    None → None
    Some n → Some $ Natural n

-- ℤ16 --

instance Zero   ℤ16 where {-# INLINE zero #-} ; zero = HS.fromIntegral 0
instance Plus   ℤ16 where {-# INLINE (+)  #-} ; (+)  = (HS.+)
instance Minus  ℤ16 where {-# INLINE (-)  #-} ; (-)  = (HS.-)
instance One    ℤ16 where {-# INLINE one  #-} ; one  = HS.fromIntegral 1
instance Times  ℤ16 where {-# INLINE (×)  #-} ; (×)  = (HS.*)
instance DivMod ℤ16 where {-# INLINE (⌿)  #-} ; (⌿)  = HS.div ; (÷) = HS.mod
instance Pon    ℤ16 where {-# INLINE (^^) #-} ; (^^) = (HS.^)

instance Bot    ℤ16 where {-# INLINE bot  #-} ; bot  = HS.fromIntegral 0
instance Join   ℤ16 where {-# INLINE (⊔)  #-} ; (⊔)  = (⩏)
instance Meet   ℤ16 where {-# INLINE (⊓)  #-} ; (⊓)  = (⩎)
instance Null   ℤ16 where {-# INLINE null #-} ; null = zero
instance Append ℤ16 where {-# INLINE (⧺)  #-} ; (⧺)  = (+)

instance Additive       ℤ16
instance Multiplicative ℤ16
instance JoinLattice    ℤ16
instance Monoid         ℤ16

instance ToNatO ℤ16 where 
  {-# INLINE natO #-}
  natO i 
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO64 ℤ16 where 
  {-# INLINE natO64 #-}
  natO64 i 
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO32 ℤ16 where 
  {-# INLINE natO32 #-}
  natO32 i 
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO16 ℤ16 where 
  {-# INLINE natO16 #-}
  natO16 i 
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO8 ℤ16 where 
  {-# INLINE natO8 #-}
  natO8 i 
    | i < HS.fromIntegral 0 = None
    | i > HS.fromIntegral (HS.maxBound @ ℕ8) = None
    | otherwise = Some $ HS.fromIntegral i

instance ToInt   ℤ16 where {-# INLINE int   #-} ; int   = HS.fromIntegral
instance ToInt64 ℤ16 where {-# INLINE int64 #-} ; int64 = HS.fromIntegral
instance ToInt32 ℤ16 where {-# INLINE int32 #-} ; int32 = HS.fromIntegral
instance ToInt16 ℤ16 where {-# INLINE int16 #-} ; int16 = id
instance ToIntO8 ℤ16 where 
  {-# INLINE intO8 #-}
  intO8 i 
    | i < HS.fromIntegral (HS.minBound @ ℤ8) = None
    | i > HS.fromIntegral (HS.maxBound @ ℤ8) = None
    | otherwise = Some $ HS.fromIntegral i

instance ToRational ℤ16 where {-# INLINE rat #-} ; rat = HS.fromIntegral
instance ToRationalᴾO ℤ16 where 
  {-# INLINE ratᴾO #-}
  ratᴾO i
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i

instance ToDouble ℤ16 where {-# INLINE dbl #-} ; dbl = HS.fromIntegral
instance ToDoubleᴾO ℤ16 where
  {-# INLINE dblᴾO #-}
  dblᴾO i
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ 𝔻ᴾ $ HS.fromIntegral i

instance ToNumber ℤ16 where {-# INLINE num #-} ; num = Integer ∘ int
instance ToNumberᴾO ℤ16 where
  {-# INLINE numᴾO #-}
  numᴾO i = case natO i of
    None → None
    Some n → Some $ Natural n

-- ℤ8 --

instance Zero   ℤ8 where {-# INLINE zero #-} ; zero = HS.fromIntegral 0
instance Plus   ℤ8 where {-# INLINE (+)  #-} ; (+)  = (HS.+)
instance Minus  ℤ8 where {-# INLINE (-)  #-} ; (-)  = (HS.-)
instance One    ℤ8 where {-# INLINE one  #-} ; one  = HS.fromIntegral 1
instance Times  ℤ8 where {-# INLINE (×)  #-} ; (×)  = (HS.*)
instance DivMod ℤ8 where {-# INLINE (⌿)  #-} ; (⌿)  = HS.div ; (÷) = HS.mod
instance Pon    ℤ8 where {-# INLINE (^^) #-} ; (^^) = (HS.^)

instance Bot    ℤ8 where {-# INLINE bot  #-} ; bot  = HS.fromIntegral 0
instance Join   ℤ8 where {-# INLINE (⊔)  #-} ; (⊔)  = (⩏)
instance Meet   ℤ8 where {-# INLINE (⊓)  #-} ; (⊓)  = (⩎)
instance Null   ℤ8 where {-# INLINE null #-} ; null = zero
instance Append ℤ8 where {-# INLINE (⧺)  #-} ; (⧺)  = (+)

instance Additive       ℤ8
instance Multiplicative ℤ8
instance JoinLattice    ℤ8
instance Monoid         ℤ8

instance ToNatO ℤ8 where
  {-# INLINE natO #-}
  natO i 
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO64 ℤ8 where
  {-# INLINE natO64 #-}
  natO64 i 
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO32 ℤ8 where
  {-# INLINE natO32 #-}
  natO32 i 
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO16 ℤ8 where
  {-# INLINE natO16 #-}
  natO16 i 
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i
instance ToNatO8  ℤ8 where
  {-# INLINE natO8 #-}
  natO8 i 
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i

instance ToInt   ℤ8 where {-# INLINE int   #-} ; int   = HS.fromIntegral
instance ToInt64 ℤ8 where {-# INLINE int64 #-} ; int64 = HS.fromIntegral
instance ToInt32 ℤ8 where {-# INLINE int32 #-} ; int32 = HS.fromIntegral
instance ToInt16 ℤ8 where {-# INLINE int16 #-} ; int16 = HS.fromIntegral
instance ToInt8  ℤ8 where {-# INLINE int8  #-} ; int8  = id

instance ToRational ℤ8 where {-# INLINE rat #-} ; rat = HS.fromIntegral
instance ToRationalᴾO ℤ8 where 
  {-# INLINE ratᴾO #-}
  ratᴾO i
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromIntegral i

instance ToDouble ℤ8 where {-# INLINE dbl #-} ; dbl = HS.fromIntegral
instance ToDoubleᴾO ℤ8 where
  {-# INLINE dblᴾO #-}
  dblᴾO i
    | i < HS.fromIntegral 0 = None
    | otherwise = Some $ 𝔻ᴾ $ HS.fromIntegral i

instance ToNumber ℤ8 where {-# INLINE num #-} ; num = Integer ∘ int
instance ToNumberᴾO ℤ8 where
  {-# INLINE numᴾO #-}
  numᴾO i = case natO i of
    None → None
    Some n → Some $ Natural n

-- ℚ --

instance Zero   ℚ where {-# INLINE zero #-} ; zero = HS.fromIntegral 0
instance Plus   ℚ where {-# INLINE (+)  #-} ; (+)  = (HS.+)
instance Minus  ℚ where {-# INLINE (-)  #-} ; (-)  = (HS.-)
instance One    ℚ where {-# INLINE one  #-} ; one  = HS.fromIntegral 1
instance Times  ℚ where {-# INLINE (×)  #-} ; (×)  = (HS.*)
instance Divide ℚ where {-# INLINE (/)  #-} ; (/)  = (HS./)
instance Pon    ℚ where {-# INLINE (^^) #-} ; (^^) = (HS.^)

instance Bot    ℚ where {-# INLINE bot  #-} ; bot  = HS.fromIntegral 0
instance Join   ℚ where {-# INLINE (⊔)  #-} ; (⊔)  = (⩏)
instance Meet   ℚ where {-# INLINE (⊓)  #-} ; (⊓)  = (⩎)
instance Null   ℚ where {-# INLINE null #-} ; null = zero
instance Append ℚ where {-# INLINE (⧺)  #-} ; (⧺)  = (+)

instance Additive       ℚ
instance Multiplicative ℚ
instance JoinLattice    ℚ
instance Monoid         ℚ

instance ToNatO ℚ where
  {-# INLINE natO #-}
  natO q
    | denom q ≢ 1 = None
    | otherwise = natO $ numer q
instance ToNatO64 ℚ where
  {-# INLINE natO64 #-}
  natO64 q
    | denom q ≢ 1 = None
    | otherwise = natO64 $ numer q
instance ToNatO32 ℚ where
  {-# INLINE natO32 #-}
  natO32 q
    | denom q ≢ 1 = None
    | otherwise = natO32 $ numer q
instance ToNatO16 ℚ where
  {-# INLINE natO16 #-}
  natO16 q
    | denom q ≢ 1 = None
    | otherwise = natO16 $ numer q
instance ToNatO8 ℚ where
  {-# INLINE natO8 #-}
  natO8 q
    | denom q ≢ 1 = None
    | otherwise = natO8 $ numer q
instance ToIntO ℚ where
  {-# INLINE intO #-}
  intO q
    | denom q ≢ 1 = None
    | otherwise = Some $ numer q
instance ToIntO64 ℚ where
  {-# INLINE intO64 #-}
  intO64 q
    | denom q ≢ 1 = None
    | otherwise = intO64 $ numer q
instance ToIntO32 ℚ where
  {-# INLINE intO32 #-}
  intO32 q
    | denom q ≢ 1 = None
    | otherwise = intO32 $ numer q
instance ToIntO16 ℚ where
  {-# INLINE intO16 #-}
  intO16 q
    | denom q ≢ 1 = None
    | otherwise = intO16 $ numer q
instance ToIntO8 ℚ where
  {-# INLINE intO8 #-}
  intO8 q
    | denom q ≢ 1 = None
    | otherwise = intO8 $ numer q

instance ToRational ℚ where {-# INLINE rat #-} ; rat = id
instance ToRationalᴾO ℚ where 
  {-# INLINE ratᴾO #-}
  ratᴾO q
    | numer q < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromRational q 

instance ToDouble ℚ where {-# INLINE dbl #-} ; dbl = HS.fromRational
instance ToDoubleᴾO ℚ where
  {-# INLINE dblᴾO #-}
  dblᴾO q
    | numer q < HS.fromIntegral 0 = None
    | otherwise = Some $ 𝔻ᴾ $ HS.fromRational q

instance ToNumber ℚ where {-# INLINE num #-} ; num = Rational
instance ToNumberᴾO ℚ where 
  {-# INLINE numᴾO #-}
  numᴾO q = case ratᴾO q of
    None → None
    Some qᴾ → Some $ Rationalᴾ qᴾ

-- ℚᴾ --

instance Zero   ℚᴾ where {-# INLINE zero #-} ; zero = HS.fromIntegral 0
instance Plus   ℚᴾ where {-# INLINE (+)  #-} ; (+)  = (HS.+)
instance Minus  ℚᴾ where {-# INLINE (-)  #-} ; (-)  = (HS.-)
instance One    ℚᴾ where {-# INLINE one  #-} ; one  = HS.fromIntegral 1
instance Times  ℚᴾ where {-# INLINE (×)  #-} ; (×)  = (HS.*)
instance Divide ℚᴾ where {-# INLINE (/)  #-} ; (/)  = (HS./)
instance Pon    ℚᴾ where {-# INLINE (^^) #-} ; (^^) = (HS.^)
                                               
instance Bot    ℚᴾ where {-# INLINE bot  #-} ; bot  = HS.fromIntegral 0
instance Join   ℚᴾ where {-# INLINE (⊔)  #-} ; (⊔)  = (⩏)
instance Meet   ℚᴾ where {-# INLINE (⊓)  #-} ; (⊓)  = (⩎)
instance Null   ℚᴾ where {-# INLINE null #-} ; null = zero
instance Append ℚᴾ where {-# INLINE (⧺)  #-} ; (⧺)  = (+)

instance Additive       ℚᴾ
instance Multiplicative ℚᴾ
instance JoinLattice    ℚᴾ
instance Monoid         ℚᴾ

instance ToNatO ℚᴾ where
  {-# INLINE natO #-}
  natO q
    | denomᴾ q ≢ HS.fromIntegral 1 = None
    | otherwise = Some $ numerᴾ q
instance ToNatO64 ℚᴾ where
  {-# INLINE natO64 #-}
  natO64 q
    | denomᴾ q ≢ HS.fromIntegral 1 = None
    | otherwise = natO64 $ numerᴾ q
instance ToNatO32 ℚᴾ where
  {-# INLINE natO32 #-}
  natO32 q
    | denomᴾ q ≢ HS.fromIntegral 1 = None
    | otherwise = natO32 $ numerᴾ q
instance ToNatO16 ℚᴾ where
  {-# INLINE natO16 #-}
  natO16 q
    | denomᴾ q ≢ HS.fromIntegral 1 = None
    | otherwise = natO16 $ numerᴾ q
instance ToNatO8 ℚᴾ where
  {-# INLINE natO8 #-}
  natO8 q
    | denomᴾ q ≢ HS.fromIntegral 1 = None
    | otherwise = natO8 $ numerᴾ q
instance ToIntO ℚᴾ where
  {-# INLINE intO #-}
  intO q
    | denomᴾ q ≢ HS.fromIntegral 1 = None
    | otherwise = Some $ int $ numerᴾ q
instance ToIntO64 ℚᴾ where
  {-# INLINE intO64 #-}
  intO64 q
    | denomᴾ q ≢ HS.fromIntegral 1 = None
    | otherwise = intO64 $ numerᴾ q
instance ToIntO32 ℚᴾ where
  {-# INLINE intO32 #-}
  intO32 q
    | denomᴾ q ≢ HS.fromIntegral 1 = None
    | otherwise = intO32 $ numerᴾ q
instance ToIntO16 ℚᴾ where
  {-# INLINE intO16 #-}
  intO16 q
    | denomᴾ q ≢ HS.fromIntegral 1 = None
    | otherwise = intO16 $ numerᴾ q
instance ToIntO8 ℚᴾ where
  {-# INLINE intO8 #-}
  intO8 q
    | denomᴾ q ≢ HS.fromIntegral 1 = None
    | otherwise = intO8 $ numerᴾ q

instance ToRational  ℚᴾ where {-# INLINE rat  #-} ; rat  = HS.toRational
instance ToRationalᴾ ℚᴾ where {-# INLINE ratᴾ #-} ; ratᴾ = id
instance ToDouble    ℚᴾ where {-# INLINE dbl  #-} ; dbl  = HS.fromRational ∘ rat
instance ToDoubleᴾ   ℚᴾ where {-# INLINE dblᴾ #-} ; dblᴾ = 𝔻ᴾ ∘ dbl
instance ToNumber    ℚᴾ where {-# INLINE num  #-} ; num  = Rational ∘ rat
instance ToNumberᴾ   ℚᴾ where {-# INLINE numᴾ #-} ; numᴾ = Rationalᴾ

-- 𝔻 --

instance Zero   𝔻 where {-# INLINE zero #-} ; zero = 0.0
instance Plus   𝔻 where {-# INLINE (+)  #-} ; (+)  = (HS.+)
instance Minus  𝔻 where {-# INLINE (-)  #-} ; (-)  = (HS.-)
instance One    𝔻 where {-# INLINE one  #-} ; one  = 1.0
instance Times  𝔻 where {-# INLINE (×)  #-} ; (×)  = (HS.*)
instance Divide 𝔻 where {-# INLINE (/)  #-} ; (/)  = (HS./)
instance Pon    𝔻 where {-# INLINE (^^) #-} ; (^^) = (HS.^)
instance Pow    𝔻 where {-# INLINE (^)  #-} ; (^)  = (HS.**)
instance Root   𝔻 where {-# INLINE root #-} ; root = HS.sqrt
instance Log    𝔻 where {-# INLINE log  #-} ; log  = HS.log
instance Efn    𝔻 where {-# INLINE efn  #-} ; efn  = HS.exp
instance Sin    𝔻 where {-# INLINE sin  #-} ; sin  = HS.sin
instance Cos    𝔻 where {-# INLINE cos  #-} ; cos  = HS.cos

instance Bot    𝔻 where {-# INLINE bot  #-} ; bot  = neg 1.0/0.0
instance Join   𝔻 where {-# INLINE (⊔)  #-} ; (⊔)  = (⩏)
instance Top    𝔻 where {-# INLINE top  #-} ; top  = 1.0/0.0
instance Meet   𝔻 where {-# INLINE (⊓)  #-} ; (⊓)  = (⩎)
instance Null   𝔻 where {-# INLINE null #-} ; null = zero
instance Append 𝔻 where {-# INLINE (⧺)  #-} ; (⧺)  = (+)

instance Additive       𝔻
instance Multiplicative 𝔻
instance JoinLattice    𝔻
instance MeetLattice    𝔻
instance Monoid         𝔻

instance ToNatO 𝔻 where
  {-# INLINE natO #-}
  natO d =
    let i = truncate d
    in case d ≡ dbl i of
      True → natO i
      False → None
instance ToNatO64 𝔻 where
  {-# INLINE natO64 #-}
  natO64 d =
    let i = truncate d
    in case d ≡ dbl i of
      True → natO64 i
      False → None
instance ToNatO32 𝔻 where
  {-# INLINE natO32 #-}
  natO32 d =
    let i = truncate d
    in case d ≡ dbl i of
      True → natO32 i
      False → None
instance ToNatO16 𝔻 where
  {-# INLINE natO16 #-}
  natO16 d =
    let i = truncate d
    in case d ≡ dbl i of
      True → natO16 i
      False → None
instance ToNatO8 𝔻 where
  {-# INLINE natO8 #-}
  natO8 d =
    let i = truncate d
    in case d ≡ dbl i of
      True → natO8 i
      False → None
instance ToIntO 𝔻 where
  {-# INLINE intO #-}
  intO d =
    let i = truncate d
    in case d ≡ dbl i of
      True → Some i
      False → None
instance ToIntO64 𝔻 where
  {-# INLINE intO64 #-}
  intO64 d =
    let i = truncate d
    in case d ≡ dbl i of
      True → intO64 i
      False → None
instance ToIntO32 𝔻 where
  {-# INLINE intO32 #-}
  intO32 d =
    let i = truncate d
    in case d ≡ dbl i of
      True → intO32 i
      False → None
instance ToIntO16 𝔻 where
  {-# INLINE intO16 #-}
  intO16 d =
    let i = truncate d
    in case d ≡ dbl i of
      True → intO16 i
      False → None
instance ToIntO8 𝔻 where
  {-# INLINE intO8 #-}
  intO8 d =
    let i = truncate d
    in case d ≡ dbl i of
      True → intO8 i
      False → None

instance ToRational 𝔻 where {-# INLINE rat #-} ; rat = HS.realToFrac
instance ToRationalᴾO 𝔻 where 
  {-# INLINE ratᴾO #-}
  ratᴾO d
    | d < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.realToFrac d

instance ToDouble 𝔻 where {-# INLINE dbl #-} ; dbl = id
instance ToDoubleᴾO 𝔻 where 
  {-# INLINE dblᴾO #-}
  dblᴾO d 
    | d < HS.fromIntegral 0 = None
    | otherwise = Some $ 𝔻ᴾ d

instance ToNumber 𝔻 where {-# INLINE num #-} ; num = Double
instance ToNumberᴾO 𝔻 where
  {-# INLINE numᴾO #-}
  numᴾO d = case dblᴾO d of
    None → None
    Some dᴾ → Some $ Doubleᴾ dᴾ

-- 𝔻ᴾ --

minus𝔻ᴾ ∷ 𝔻ᴾ → 𝔻ᴾ → 𝔻ᴾ
minus𝔻ᴾ p₁ p₂
  | p₁ < p₂ = error "𝔻ᴾ: subtraction: LHS is smaller than RHS"
  | otherwise = p₁ HS.- p₂

instance Zero   𝔻ᴾ where {-# INLINE zero #-} ; zero = HS.fromIntegral 0
instance Plus   𝔻ᴾ where {-# INLINE (+)  #-} ; (+)  = (HS.+)
instance Minus  𝔻ᴾ where {-# INLINE (-)  #-} ; (-)  = minus𝔻ᴾ
instance One    𝔻ᴾ where {-# INLINE one  #-} ; one  = HS.fromIntegral 1
instance Times  𝔻ᴾ where {-# INLINE (×)  #-} ; (×)  = (HS.*)
instance Divide 𝔻ᴾ where {-# INLINE (/)  #-} ; (/)  = (HS./)
instance Pon    𝔻ᴾ where {-# INLINE (^^) #-} ; (^^) = (HS.^)
instance Pow    𝔻ᴾ where {-# INLINE (^)  #-} ; (^)  = (HS.**)
instance Root   𝔻ᴾ where {-# INLINE root #-} ; root = HS.sqrt
instance Log    𝔻ᴾ where {-# INLINE log  #-} ; log  = HS.log
instance Efn    𝔻ᴾ where {-# INLINE efn  #-} ; efn  = HS.exp
instance Sin    𝔻ᴾ where {-# INLINE sin  #-} ; sin  = HS.sin
instance Cos    𝔻ᴾ where {-# INLINE cos  #-} ; cos  = HS.cos
                                               
instance Bot    𝔻ᴾ where {-# INLINE bot  #-} ; bot  = HS.fromIntegral 0
instance Join   𝔻ᴾ where {-# INLINE (⊔)  #-} ; (⊔)  = (⩏)
instance Top    𝔻ᴾ where {-# INLINE top  #-} ; top  = HS.realToFrac $ 1.0 / 0.0
instance Meet   𝔻ᴾ where {-# INLINE (⊓)  #-} ; (⊓)  = (⩎)
instance Null   𝔻ᴾ where {-# INLINE null #-} ; null = zero
instance Append 𝔻ᴾ where {-# INLINE (⧺)  #-} ; (⧺)  = (+)

instance Additive       𝔻ᴾ
instance Multiplicative 𝔻ᴾ
instance JoinLattice    𝔻ᴾ
instance MeetLattice    𝔻ᴾ
instance Monoid         𝔻ᴾ

instance ToNatO 𝔻ᴾ where
  {-# INLINE natO #-}
  natO d =
    let n = truncateᴾ d
    in case d ≡ dblᴾ n of
      True → Some n
      False → None
instance ToNatO64 𝔻ᴾ where
  {-# INLINE natO64 #-}
  natO64 d =
    let n = truncateᴾ d
    in case d ≡ dblᴾ n of
      True → natO64 n
      False → None
instance ToNatO32 𝔻ᴾ where
  {-# INLINE natO32 #-}
  natO32 d =
    let n = truncateᴾ d
    in case d ≡ dblᴾ n of
      True → natO32 n
      False → None
instance ToNatO16 𝔻ᴾ where
  {-# INLINE natO16 #-}
  natO16 d =
    let n = truncateᴾ d
    in case d ≡ dblᴾ n of
      True → natO16 n
      False → None
instance ToNatO8 𝔻ᴾ where
  {-# INLINE natO8 #-}
  natO8 d =
    let n = truncateᴾ d
    in case d ≡ dblᴾ n of
      True → natO8 n
      False → None
instance ToIntO 𝔻ᴾ where
  {-# INLINE intO #-}
  intO d =
    let n = truncateᴾ d
    in case d ≡ dblᴾ n of
      True → Some $ int n
      False → None
instance ToIntO64 𝔻ᴾ where
  {-# INLINE intO64 #-}
  intO64 d =
    let n = truncateᴾ d
    in case d ≡ dblᴾ n of
      True → intO64 n
      False → None
instance ToIntO32 𝔻ᴾ where
  {-# INLINE intO32 #-}
  intO32 d =
    let n = truncateᴾ d
    in case d ≡ dblᴾ n of
      True → intO32 n
      False → None
instance ToIntO16 𝔻ᴾ where
  {-# INLINE intO16 #-}
  intO16 d =
    let n = truncateᴾ d
    in case d ≡ dblᴾ n of
      True → intO16 n
      False → None
instance ToIntO8 𝔻ᴾ where
  {-# INLINE intO8 #-}
  intO8 d =
    let n = truncateᴾ d
    in case d ≡ dblᴾ n of
      True → intO8 n
      False → None

instance ToRational 𝔻ᴾ where {-# INLINE rat #-} ; rat = HS.toRational
instance ToRationalᴾO 𝔻ᴾ where 
  {-# INLINE ratᴾO #-}
  ratᴾO d
    | d < HS.fromIntegral 0 = None
    | otherwise = Some $ HS.fromRational $ HS.toRational d

instance ToDouble  𝔻ᴾ where {-# INLINE dbl  #-} ; dbl  = un𝔻ᴾ
instance ToDoubleᴾ 𝔻ᴾ where {-# INLINE dblᴾ #-} ; dblᴾ = id
instance ToNumber  𝔻ᴾ where {-# INLINE num  #-} ; num  = Double ∘ dbl
instance ToNumberᴾ 𝔻ᴾ where {-# INLINE numᴾ #-} ; numᴾ = Doubleᴾ

-- ℝ and ℝ⁺ --

{-# INLINE numberBOp #-}
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

{-# INLINE numberBOpᴾ #-}
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

instance Zero   ℝ where {-# INLINE zero #-} ; zero = Integer zero
instance Plus   ℝ where {-# INLINE (+)  #-} ; (+)  = numberBOp (Integer ∘∘ (+)) (Rational ∘∘ (+)) $ Double ∘∘ (+)
instance Minus  ℝ where {-# INLINE (-)  #-} ; (-)  = numberBOp (Integer ∘∘ (-)) (Rational ∘∘ (-)) $ Double ∘∘ (-)
instance One    ℝ where {-# INLINE one  #-} ; one  = Integer one
instance Times  ℝ where {-# INLINE (×)  #-} ; (×)  = numberBOp (Integer ∘∘ (×)) (Rational ∘∘ (×)) $ Double ∘∘ (×)
instance Divide ℝ where {-# INLINE (/)  #-} ; (/)  = numberBOp (\ i₁ i₂ → Rational $ rat i₁ / rat i₂) (Rational ∘∘ (/)) $ Double ∘∘ (/)
instance Pon ℝ where 
  {-# INLINE (^^) #-}
  Integer  m ^^ n = Integer  $ m ^^ n
  Rational q ^^ n = Rational $ q ^^ n
  Double   d ^^ n = Double   $ d ^^ n
instance Pow ℝ where 
  {-# INLINE (^) #-} 
  (^) = numberBOp (\ i₁ i₂ → Double $ dbl i₁ ^ dbl i₂) (\ q₁ q₂ → Double $ dbl q₁ ^ dbl q₂) $ Double ∘∘ (^)
instance Root ℝ where
  {-# INLINE root #-}
  root (Integer  i) = Double $ root $ dbl i
  root (Rational q) = Double $ root $ dbl q
  root (Double   d) = Double $ root d
instance Log ℝ where 
  {-# INLINE log #-}
  log (Integer  i) = Double $ log $ dbl i
  log (Rational q) = Double $ log $ dbl q
  log (Double   d) = Double $ log d
instance Efn ℝ where
  {-# INLINE efn #-}
  efn (Integer  i) = Double $ efn $ dbl i
  efn (Rational q) = Double $ efn $ dbl q
  efn (Double   d) = Double $ efn d
instance Sin ℝ where
  {-# INLINE sin #-}
  sin (Integer  i) = Double $ sin $ dbl i
  sin (Rational q) = Double $ sin $ dbl q
  sin (Double   d) = Double $ sin d
instance Cos ℝ where
  {-# INLINE cos #-}
  cos (Integer  i) = Double $ cos $ dbl i
  cos (Rational q) = Double $ cos $ dbl q
  cos (Double   d) = Double $ cos d

instance Bot    ℝ where {-# INLINE bot  #-} ; bot  = zero
instance Join   ℝ where {-# INLINE (⊔)  #-} ; (⊔)  = numberBOp (Integer ∘∘ (⊔)) (Rational ∘∘ (⊔)) (Double ∘∘ (⊔))
instance Meet   ℝ where {-# INLINE (⊓)  #-} ; (⊓)  = numberBOp (Integer ∘∘ (⊓)) (Rational ∘∘ (⊓)) (Double ∘∘ (⊓))
instance Null   ℝ where {-# INLINE null #-} ; null = zero
instance Append ℝ where {-# INLINE (⧺)  #-} ; (⧺)  = (+)

instance Additive       ℝ
instance Multiplicative ℝ
instance JoinLattice    ℝ
instance Monoid         ℝ

instance ToNatO ℝ where
  {-# INLINE natO #-}
  natO (Integer  i) = natO i
  natO (Rational q) = natO q
  natO (Double   d) = natO d
instance ToNatO64 ℝ where
  {-# INLINE natO64 #-}
  natO64 (Integer  i) = natO64 i
  natO64 (Rational q) = natO64 q
  natO64 (Double   d) = natO64 d
instance ToNatO32 ℝ where
  {-# INLINE natO32 #-}
  natO32 (Integer  i) = natO32 i
  natO32 (Rational q) = natO32 q
  natO32 (Double   d) = natO32 d
instance ToNatO16 ℝ where
  {-# INLINE natO16 #-}
  natO16 (Integer  i) = natO16 i
  natO16 (Rational q) = natO16 q
  natO16 (Double   d) = natO16 d
instance ToNatO8 ℝ where
  {-# INLINE natO8 #-}
  natO8 (Integer  i) = natO8 i
  natO8 (Rational q) = natO8 q
  natO8 (Double   d) = natO8 d
instance ToIntO ℝ where
  {-# INLINE intO #-}
  intO (Integer  i) = Some i
  intO (Rational q) = intO q
  intO (Double   d) = intO d
instance ToIntO64 ℝ where
  {-# INLINE intO64 #-}
  intO64 (Integer  i) = intO64 i
  intO64 (Rational q) = intO64 q
  intO64 (Double   d) = intO64 d
instance ToIntO32 ℝ where
  {-# INLINE intO32 #-}
  intO32 (Integer  i) = intO32 i
  intO32 (Rational q) = intO32 q
  intO32 (Double   d) = intO32 d
instance ToIntO16 ℝ where
  {-# INLINE intO16 #-}
  intO16 (Integer  i) = intO16 i
  intO16 (Rational q) = intO16 q
  intO16 (Double   d) = intO16 d
instance ToIntO8 ℝ where
  {-# INLINE intO8 #-}
  intO8 (Integer  i) = intO8 i
  intO8 (Rational q) = intO8 q
  intO8 (Double   d) = intO8 d
instance ToRational ℝ where
  {-# INLINE rat #-}
  rat (Integer  i) = rat i
  rat (Rational q) = q
  rat (Double   d) = rat d
instance ToRationalᴾO ℝ where
  {-# INLINE ratᴾO #-}
  ratᴾO (Integer  i) = ratᴾO i
  ratᴾO (Rational q) = ratᴾO q
  ratᴾO (Double   d) = ratᴾO d
instance ToDouble ℝ where
  {-# INLINE dbl #-}
  dbl (Integer  i) = dbl i
  dbl (Rational q) = dbl q
  dbl (Double   d) = d
instance ToDoubleᴾO ℝ where
  {-# INLINE dblᴾO #-}
  dblᴾO (Integer  i) = dblᴾO i
  dblᴾO (Rational q) = dblᴾO q
  dblᴾO (Double   d) = dblᴾO d

instance Zero   ℝᴾ where {-# INLINE zero #-} ; zero = Natural zero
instance Plus   ℝᴾ where {-# INLINE (+)  #-} ; (+)  = numberBOpᴾ (Natural ∘∘ (+)) (Rationalᴾ ∘∘ (+)) (Doubleᴾ ∘∘ (+))
instance Minus  ℝᴾ where {-# INLINE (-)  #-} ; (-)  = numberBOpᴾ (Natural ∘∘ (-)) (Rationalᴾ ∘∘ (-)) (Doubleᴾ ∘∘ (-))
instance One    ℝᴾ where {-# INLINE one  #-} ; one  = Natural one
instance Times  ℝᴾ where {-# INLINE (×)  #-} ; (×)  = numberBOpᴾ (Natural ∘∘ (×)) (Rationalᴾ ∘∘ (×)) (Doubleᴾ ∘∘ (×))
instance Divide ℝᴾ where {-# INLINE (/)  #-} ; (/)  = numberBOpᴾ (\ n₁ n₂ → Rationalᴾ $ ratᴾ n₁ / ratᴾ n₂) (Rationalᴾ ∘∘ (/)) (Doubleᴾ ∘∘ (/))
instance Pon ℝᴾ where 
  {-# INLINE (^^) #-}
  Natural   m ^^ n = Natural   $ m ^^ n
  Rationalᴾ q ^^ n = Rationalᴾ $ q ^^ n
  Doubleᴾ   d ^^ n = Doubleᴾ   $ d ^^ n
instance Pow ℝᴾ where 
  {-# INLINE (^) #-}
  (^) = numberBOpᴾ (Natural ∘∘ (^)) (\ qᴾ₁ qᴾ₂ → Doubleᴾ $ dblᴾ qᴾ₁ ^ dblᴾ qᴾ₂) (Doubleᴾ ∘∘ (^))
instance Root ℝᴾ where
  {-# INLINE root #-}
  root (Natural   n) = Doubleᴾ $ root $ dblᴾ n
  root (Rationalᴾ q) = Doubleᴾ $ root $ dblᴾ q
  root (Doubleᴾ   d) = Doubleᴾ $ root d
instance Log ℝᴾ where 
  {-# INLINE log #-}
  log (Natural   n) = Doubleᴾ $ log $ dblᴾ n
  log (Rationalᴾ q) = Doubleᴾ $ log $ dblᴾ q
  log (Doubleᴾ   d) = Doubleᴾ $ log d

instance Bot    ℝᴾ where {-# INLINE bot  #-} ; bot  = zero
instance Join   ℝᴾ where {-# INLINE (⊔)  #-} ; (⊔)  = numberBOpᴾ (Natural ∘∘ (⊔)) (Rationalᴾ ∘∘ (⊔)) (Doubleᴾ ∘∘ (⊔))
instance Meet   ℝᴾ where {-# INLINE (⊓)  #-} ; (⊓)  = numberBOpᴾ (Natural ∘∘ (⊓)) (Rationalᴾ ∘∘ (⊓)) (Doubleᴾ ∘∘ (⊓))
instance Null   ℝᴾ where {-# INLINE null #-} ; null = zero
instance Append ℝᴾ where {-# INLINE (⧺)  #-} ; (⧺)  = (+)

instance Additive       ℝᴾ
instance Multiplicative ℝᴾ
instance JoinLattice    ℝᴾ
instance Monoid         ℝᴾ

instance ToNatO ℝᴾ where
  {-# INLINE natO #-}
  natO (Natural   n)   = Some n
  natO (Rationalᴾ q) = natO q
  natO (Doubleᴾ   d)   = natO d
instance ToNatO64 ℝᴾ where
  {-# INLINE natO64 #-}
  natO64 (Natural   n) = natO64 n
  natO64 (Rationalᴾ q) = natO64 q
  natO64 (Doubleᴾ   d) = natO64 d
instance ToNatO32 ℝᴾ where
  {-# INLINE natO32 #-}
  natO32 (Natural   n) = natO32 n
  natO32 (Rationalᴾ q) = natO32 q
  natO32 (Doubleᴾ   d) = natO32 d
instance ToNatO16 ℝᴾ where
  {-# INLINE natO16 #-}
  natO16 (Natural   n) = natO16 n
  natO16 (Rationalᴾ q) = natO16 q
  natO16 (Doubleᴾ   d) = natO16 d
instance ToNatO8 ℝᴾ where
  {-# INLINE natO8 #-}
  natO8 (Natural   n) = natO8 n
  natO8 (Rationalᴾ q) = natO8 q
  natO8 (Doubleᴾ   d) = natO8 d
instance ToIntO ℝᴾ where
  {-# INLINE intO #-}
  intO (Natural   n) = Some $ int n
  intO (Rationalᴾ q) = intO q
  intO (Doubleᴾ   d) = intO d
instance ToIntO64 ℝᴾ where
  {-# INLINE intO64 #-}
  intO64 (Natural   n) = intO64 n
  intO64 (Rationalᴾ q) = intO64 q
  intO64 (Doubleᴾ   d) = intO64 d
instance ToIntO32 ℝᴾ where
  {-# INLINE intO32 #-}
  intO32 (Natural   n) = intO32 n
  intO32 (Rationalᴾ q) = intO32 q
  intO32 (Doubleᴾ   d) = intO32 d
instance ToIntO16 ℝᴾ where
  {-# INLINE intO16 #-}
  intO16 (Natural   n) = intO16 n
  intO16 (Rationalᴾ q) = intO16 q
  intO16 (Doubleᴾ   d) = intO16 d
instance ToIntO8 ℝᴾ where
  {-# INLINE intO8 #-}
  intO8 (Natural   n) = intO8 n
  intO8 (Rationalᴾ q) = intO8 q
  intO8 (Doubleᴾ   d) = intO8 d
instance ToRational ℝᴾ where
  {-# INLINE rat #-}
  rat (Natural   n) = rat n
  rat (Rationalᴾ q) = rat q
  rat (Doubleᴾ   d) = rat d
instance ToRationalᴾO ℝᴾ where
  {-# INLINE ratᴾO #-}
  ratᴾO (Natural   n) = Some $ ratᴾ n
  ratᴾO (Rationalᴾ q) = Some q
  ratᴾO (Doubleᴾ   d) = ratᴾO d
instance ToDouble ℝᴾ where
  {-# INLINE dbl #-}
  dbl (Natural   n) = dbl n
  dbl (Rationalᴾ q) = dbl q
  dbl (Doubleᴾ   d) = dbl d
instance ToDoubleᴾ ℝᴾ where
  {-# INLINE dblᴾ #-}
  dblᴾ (Natural   n) = dblᴾ n
  dblᴾ (Rationalᴾ q) = dblᴾ q
  dblᴾ (Doubleᴾ   d) = d
