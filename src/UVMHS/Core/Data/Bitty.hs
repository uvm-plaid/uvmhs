module UVMHS.Core.Data.Bitty where

import UVMHS.Core.Init
import UVMHS.Core.Classes

import UVMHS.Core.Data.Arithmetic ()

import qualified Data.Bits as HS

shiftL_shim ∷ (HS.Bits a) ⇒ a → ℕ64 → a
shiftL_shim x n = HS.shiftL x $ tohs $ intΩ64 n

shiftR_shim ∷ (HS.Bits a) ⇒ a → ℕ64 → a
shiftR_shim x n = HS.shiftR x $ tohs $ intΩ64 n

bsize_shim ∷ ∀ a. (HS.FiniteBits a) ⇒ P a → ℕ64
bsize_shim _ = natΩ64 $ frhs $ HS.finiteBitSize @ a undefined

instance BitZero   𝔹 where bzero  = HS.zeroBits
instance BitOne    𝔹 where bone   = True
instance BitComp   𝔹 where comp   = HS.complement
instance BitAnd    𝔹 where (⟑)    = (HS..&.)
instance BitOr     𝔹 where (⟇)    = (HS..|.)
instance BitXor    𝔹 where (⊻)    = HS.xor
instance BitShiftL 𝔹 where (⋘)    = shiftL_shim
instance BitShiftR 𝔹 where (⋙)    = shiftR_shim
instance BitSize   𝔹 where bsize  = bsize_shim
instance Bitty     𝔹

instance BitZero   ℕ64 where bzero  = HS.zeroBits
instance BitOne    ℕ64 where bone   = one
instance BitComp   ℕ64 where comp   = HS.complement
instance BitAnd    ℕ64 where (⟑)    = (HS..&.)
instance BitOr     ℕ64 where (⟇)    = (HS..|.)
instance BitXor    ℕ64 where (⊻)    = HS.xor
instance BitShiftL ℕ64 where (⋘)    = shiftL_shim
instance BitShiftR ℕ64 where (⋙)    = shiftR_shim
instance BitSize   ℕ64 where bsize  = bsize_shim
instance Bitty     ℕ64

instance BitZero   ℕ32 where bzero  = HS.zeroBits
instance BitOne    ℕ32 where bone   = one
instance BitComp   ℕ32 where comp   = HS.complement
instance BitAnd    ℕ32 where (⟑)    = (HS..&.)
instance BitOr     ℕ32 where (⟇)    = (HS..|.)
instance BitXor    ℕ32 where (⊻)    = HS.xor
instance BitShiftL ℕ32 where (⋘)    = shiftL_shim
instance BitShiftR ℕ32 where (⋙)    = shiftR_shim
instance BitSize   ℕ32 where bsize  = bsize_shim
instance Bitty     ℕ32

instance BitZero   ℕ16 where bzero  = HS.zeroBits
instance BitOne    ℕ16 where bone   = one
instance BitComp   ℕ16 where comp   = HS.complement
instance BitAnd    ℕ16 where (⟑)    = (HS..&.)
instance BitOr     ℕ16 where (⟇)    = (HS..|.)
instance BitXor    ℕ16 where (⊻)    = HS.xor
instance BitShiftL ℕ16 where (⋘)    = shiftL_shim
instance BitShiftR ℕ16 where (⋙)    = shiftR_shim
instance BitSize   ℕ16 where bsize  = bsize_shim
instance Bitty     ℕ16

instance BitZero   ℕ8 where bzero  = HS.zeroBits
instance BitOne    ℕ8 where bone   = one
instance BitComp   ℕ8 where comp   = HS.complement
instance BitAnd    ℕ8 where (⟑)    = (HS..&.)
instance BitOr     ℕ8 where (⟇)    = (HS..|.)
instance BitXor    ℕ8 where (⊻)    = HS.xor
instance BitShiftL ℕ8 where (⋘)    = shiftL_shim
instance BitShiftR ℕ8 where (⋙)    = shiftR_shim
instance BitSize   ℕ8 where bsize  = bsize_shim
instance Bitty     ℕ8

instance BitZero   ℤ64 where bzero  = HS.zeroBits
instance BitOne    ℤ64 where bone   = one
instance BitComp   ℤ64 where comp   = HS.complement
instance BitAnd    ℤ64 where (⟑)    = (HS..&.)
instance BitOr     ℤ64 where (⟇)    = (HS..|.)
instance BitXor    ℤ64 where (⊻)    = HS.xor
instance BitShiftL ℤ64 where (⋘)    = shiftL_shim
instance BitShiftR ℤ64 where (⋙)    = shiftR_shim
instance BitSize   ℤ64 where bsize  = bsize_shim
instance Bitty     ℤ64

instance BitZero   ℤ32 where bzero  = HS.zeroBits
instance BitOne    ℤ32 where bone   = one
instance BitComp   ℤ32 where comp   = HS.complement
instance BitAnd    ℤ32 where (⟑)    = (HS..&.)
instance BitOr     ℤ32 where (⟇)    = (HS..|.)
instance BitXor    ℤ32 where (⊻)    = HS.xor
instance BitShiftL ℤ32 where (⋘)    = shiftL_shim
instance BitShiftR ℤ32 where (⋙)    = shiftR_shim
instance BitSize   ℤ32 where bsize  = bsize_shim
instance Bitty     ℤ32

instance BitZero   ℤ16 where bzero  = HS.zeroBits
instance BitOne    ℤ16 where bone   = one
instance BitComp   ℤ16 where comp   = HS.complement
instance BitAnd    ℤ16 where (⟑)    = (HS..&.)
instance BitOr     ℤ16 where (⟇)    = (HS..|.)
instance BitXor    ℤ16 where (⊻)    = HS.xor
instance BitShiftL ℤ16 where (⋘)    = shiftL_shim
instance BitShiftR ℤ16 where (⋙)    = shiftR_shim
instance BitSize   ℤ16 where bsize  = bsize_shim
instance Bitty     ℤ16

instance BitZero   ℤ8 where bzero  = HS.zeroBits
instance BitOne    ℤ8 where bone   = one
instance BitComp   ℤ8 where comp   = HS.complement
instance BitAnd    ℤ8 where (⟑)    = (HS..&.)
instance BitOr     ℤ8 where (⟇)    = (HS..|.)
instance BitXor    ℤ8 where (⊻)    = HS.xor
instance BitShiftL ℤ8 where (⋘)    = shiftL_shim
instance BitShiftR ℤ8 where (⋙)    = shiftR_shim
instance BitSize   ℤ8 where bsize  = bsize_shim
instance Bitty     ℤ8
