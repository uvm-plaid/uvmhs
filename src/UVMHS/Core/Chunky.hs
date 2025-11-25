module UVMHS.Core.Chunky where

import UVMHS.Core.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data

import qualified Prelude as HS

import qualified Data.Char as HS

import qualified GHC.Prim as Prim
import qualified GHC.Word as Word
import qualified GHC.Int as Int
import qualified GHC.Float as Float


------------
-- Chunks --
------------

-- Why not `fromIntegral`?
-- - Not guaranteed to be fast
-- - Not guaranteed to be 'bit-representation preserving'
-- See also: https://stackoverflow.com/questions/64044693/how-can-i-bit-convert-between-int-and-word-quickly

firstByte ∷ ℕ64 → ℕ8
firstByte (Word.W64# n) = Word.W8# (Prim.wordToWord8# (Prim.word64ToWord# n))

toBitsHSInt ∷ HS.Int → ℕ64
toBitsHSInt (Int.I# i) = Word.W64# (Prim.int64ToWord64# (Prim.intToInt64# i))

frBitsHSInt ∷ ℕ64 → HS.Int
frBitsHSInt (Word.W64# n) = Int.I# (Prim.int64ToInt# (Prim.word64ToInt64# n))

toBitsℤ64 ∷ ℤ64 → ℕ64
toBitsℤ64 (Int.I64# i) = Word.W64# (Prim.int64ToWord64# i)

frBitsℤ64 ∷ ℕ64 → ℤ64
frBitsℤ64 (Word.W64# n) = Int.I64# (Prim.word64ToInt64# n)

toBits𝔻 ∷ 𝔻 → ℕ64
toBits𝔻 = Float.castDoubleToWord64

frBits𝔻 ∷ ℕ64 → 𝔻
frBits𝔻 = Float.castWord64ToDouble

skipChunk ∷ (Monad m) ⇒ m ℕ8 → ℕ64 → m ()
skipChunk g n₀ = loop (𝕟64 0)
  where
    loop n
      | n ≡ n₀ = return ()
      | otherwise = do
          _ ← g
          loop $ succ n

emptyChunk ∷ ℕ64 → 𝐼 ℕ8
emptyChunk n = replicate (nat n) (𝕟8 0)

joinBytes ∷ (ℕ8,ℕ8,ℕ8,ℕ8,ℕ8,ℕ8,ℕ8,ℕ8) → ℕ64
joinBytes (b₁,b₂,b₃,b₄,b₅,b₆,b₇,b₈) =
    nat64 b₁ ⋘ 𝕟64  0
  ⟇ nat64 b₂ ⋘ 𝕟64  8
  ⟇ nat64 b₃ ⋘ 𝕟64 16
  ⟇ nat64 b₄ ⋘ 𝕟64 24
  ⟇ nat64 b₅ ⋘ 𝕟64 32
  ⟇ nat64 b₆ ⋘ 𝕟64 40
  ⟇ nat64 b₇ ⋘ 𝕟64 48
  ⟇ nat64 b₈ ⋘ 𝕟64 56

splitBytes ∷ ℕ64 → (ℕ8,ℕ8,ℕ8,ℕ8,ℕ8,ℕ8,ℕ8,ℕ8)
splitBytes n =
  ( firstByte $ n ⋙ 𝕟64  0
  , firstByte $ n ⋙ 𝕟64  8
  , firstByte $ n ⋙ 𝕟64 16
  , firstByte $ n ⋙ 𝕟64 24
  , firstByte $ n ⋙ 𝕟64 32
  , firstByte $ n ⋙ 𝕟64 40
  , firstByte $ n ⋙ 𝕟64 48
  , firstByte $ n ⋙ 𝕟64 56
  )

class Chunky a where
  chunkSize ∷ P a → ℕ64
  fromChunk ∷ ∀ m. (Monad m) ⇒ m ℕ8 → m a
  toChunk ∷ a → 𝐼 ℕ8

instance {-# OVERLAPPABLE #-} (Chunky b,a ⇄ b) ⇒ Chunky a where
  chunkSize P = chunkSize @b P
  fromChunk = map isofr ∘ fromChunk
  toChunk = toChunk ∘ isoto

instance Chunky () where
  chunkSize P = 𝕟64 0
  fromChunk _ = return ()
  toChunk ()  = empty𝐼

instance Chunky ℕ8 where
  chunkSize P = 𝕟64 1
  fromChunk   = id
  toChunk     = single

instance Chunky 𝔹 where
  chunkSize P = 𝕟64 1
  fromChunk g = do
    b ← g
    return $ case b ≡ 𝕟8 0 of
      True → False
      False → True
  toChunk b = toChunk $ case b of
    False → 𝕟8 0
    True → 𝕟8 1

instance Chunky ℂ where
  chunkSize P = 𝕟64 4
  fromChunk g = do
    b₁ ← g ; b₂ ← g ; b₃ ← g ; b₄ ← g
    return $ HS.chr $ tohs $ frBitsℤ64 $ joinBytes (b₁,b₂,b₃,b₄,𝕟8 0,𝕟8 0,𝕟8 0,𝕟8 0)
  toChunk c = 𝐼 HS.$ \ (f ∷ ℕ8 → b → (b → b) → b) i 𝓀 →
    let (b₁,b₂,b₃,b₄,_,_,_,_) = splitBytes $ toBitsℤ64 $ frhs $ HS.ord c
    in
      f b₁ i $ \ i' →
      f b₂ i' $ \ i'' →
      f b₃ i'' $ \ i''' →
      f b₄ i''' 𝓀

instance Chunky ℕ64 where
  chunkSize P = 𝕟64 8
  fromChunk g = do
    b₁ ← g ; b₂ ← g ; b₃ ← g ; b₄ ← g
    b₅ ← g ; b₆ ← g ; b₇ ← g ; b₈ ← g
    return $ joinBytes (b₁,b₂,b₃,b₄,b₅,b₆,b₇,b₈)
  toChunk n = 𝐼 HS.$ \ (f ∷ ℕ8 → b → (b → b) → b) i 𝓀 →
    let (b₁,b₂,b₃,b₄,b₅,b₆,b₇,b₈) = splitBytes n
    in
      f b₁ i $ \ i' →
      f b₂ i' $ \ i'' →
      f b₃ i'' $ \ i''' →
      f b₄ i''' $ \ i'''' →
      f b₅ i'''' $ \ i''''' →
      f b₆ i''''' $ \ i'''''' →
      f b₇ i'''''' $ \ i''''''' →
      f b₈ i''''''' 𝓀

instance Chunky ℤ64 where
  chunkSize P = 𝕟64 8
  fromChunk = map (coerce_UNSAFE @ℕ64 @ℤ64) ∘ fromChunk
  toChunk = toChunk ∘ (coerce_UNSAFE @ℤ64 @ℕ64)

instance Chunky 𝔻 where
  chunkSize P = 𝕟64 8
  fromChunk = map (coerce_UNSAFE @ℕ64 @𝔻) ∘ fromChunk
  toChunk = toChunk ∘ (coerce_UNSAFE @𝔻 @ℕ64)

instance (Chunky a,Chunky b) ⇒ Chunky (a ∧ b) where
  chunkSize P = chunkSize @a P + chunkSize @b P
  fromChunk g = do
    x ← fromChunk g
    y ← fromChunk g
    return $ x :* y
  toChunk (x :* y) = toChunk x ⧺ toChunk y

instance (Chunky a,Chunky b) ⇒ Chunky (a ∨ b) where
  chunkSize P = 𝕟64 1 + (chunkSize @a P ⩏ chunkSize @b P)
  fromChunk g = do
    b ← g
    case b ≡ 𝕟8 0 of
      True → do
        x ← fromChunk g
        skipChunk g $ (chunkSize @a P ⩏ chunkSize @b P) - chunkSize @a P
        return $ Inl x
      False → do
        y ← fromChunk g
        skipChunk g $ (chunkSize @a P ⩏ chunkSize @b P) - chunkSize @b P
        return $ Inr y
  toChunk = \case
    Inl x → single (𝕟8 0) ⧺ toChunk x ⧺ emptyChunk ((chunkSize @a P ⩏ chunkSize @b P) - chunkSize @a P)
    Inr y → single (𝕟8 1) ⧺ toChunk y ⧺ emptyChunk ((chunkSize @a P ⩏ chunkSize @b P) - chunkSize @b P)

-- chunkIOBytes ∷ UArr.UArray ℕ64 ℕ8 → State ℕ64 ℕ8
-- chunkIOBytes a = do
--   i ← next
--   return $ a UArr.! i
