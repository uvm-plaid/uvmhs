module UVMHS.Core.Chunky where

import UVMHS.Core.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data

import qualified Prelude as HS

import qualified Data.Char as HS


------------
-- Chunks --
------------

trℕ8 ∷ ℕ64 → ℕ8
trℕ8 = HS.fromIntegral

toBitsℤ64 ∷ ℤ64 → ℕ64
toBitsℤ64 = coerce_UNSAFE

frBitsℤ64 ∷ ℕ64 → ℤ64
frBitsℤ64 = coerce_UNSAFE

toBits𝔻 ∷ 𝔻 → ℕ64
toBits𝔻 = coerce_UNSAFE

frBits𝔻 ∷ ℕ64 → 𝔻
frBits𝔻 = coerce_UNSAFE

skipChunk ∷ (Monad m) ⇒ m ℕ8 → ℕ64 → m ()
skipChunk g n₀ = loop (𝕟64 0)
  where
    loop n
      | n ≡ n₀ = return ()
      | otherwise = do
          _ ← g
          loop $ succ n

emptyChunk ∷ ℕ64 → 𝐼 ℕ8
emptyChunk n = repeat (nat n) (𝕟8 0)

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
  ( trℕ8 $ n ⋙ 𝕟64  0
  , trℕ8 $ n ⋙ 𝕟64  8
  , trℕ8 $ n ⋙ 𝕟64 16
  , trℕ8 $ n ⋙ 𝕟64 24
  , trℕ8 $ n ⋙ 𝕟64 32
  , trℕ8 $ n ⋙ 𝕟64 40
  , trℕ8 $ n ⋙ 𝕟64 48
  , trℕ8 $ n ⋙ 𝕟64 56
  )

class Chunky a where
  chunkSize ∷ P a → ℕ64
  fromChunk ∷ ∀ m. (Monad m) ⇒ m ℕ8 → m a
  toChunk ∷ a → 𝐼 ℕ8

instance {-# OVERLAPPABLE #-} (Chunky b,a ⇄ b) ⇒ Chunky a where
  chunkSize P = chunkSize @ b P
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
  toChunk c = 𝐼 $ \ (f ∷ ℕ8 → b → b) →
    let (b₁,b₂,b₃,b₄,_,_,_,_) = splitBytes $ toBitsℤ64 $ frhs $ HS.ord c
    in f b₄ ∘ f b₃ ∘ f b₂ ∘ f b₁

instance Chunky ℕ64 where
  chunkSize P = 𝕟64 8
  fromChunk g = do
    b₁ ← g ; b₂ ← g ; b₃ ← g ; b₄ ← g
    b₅ ← g ; b₆ ← g ; b₇ ← g ; b₈ ← g
    return $ joinBytes (b₁,b₂,b₃,b₄,b₅,b₆,b₇,b₈)
  toChunk n = 𝐼 $ \ (f ∷ ℕ8 → b → b) →
    let (b₁,b₂,b₃,b₄,b₅,b₆,b₇,b₈) = splitBytes n
    in f b₈ ∘ f b₇ ∘ f b₆ ∘ f b₅ ∘ f b₄ ∘ f b₃ ∘ f b₂ ∘ f b₁

instance Chunky ℤ64 where
  chunkSize P = 𝕟64 8
  fromChunk = map (coerce_UNSAFE @ ℕ64 @ ℤ64) ∘ fromChunk
  toChunk = toChunk ∘ (coerce_UNSAFE @ ℤ64 @ ℕ64)

instance Chunky 𝔻 where
  chunkSize P = 𝕟64 8
  fromChunk = map (coerce_UNSAFE @ ℕ64 @ 𝔻) ∘ fromChunk
  toChunk = toChunk ∘ (coerce_UNSAFE @ 𝔻 @ ℕ64)

instance (Chunky a,Chunky b) ⇒ Chunky (a ∧ b) where
  chunkSize P = chunkSize @ a P + chunkSize @ b P
  fromChunk g = do
    x ← fromChunk g
    y ← fromChunk g
    return $ x :* y
  toChunk (x :* y) = toChunk x ⧺ toChunk y

instance (Chunky a,Chunky b) ⇒ Chunky (a ∨ b) where
  chunkSize P = 𝕟64 1 + (chunkSize @ a P ⩏ chunkSize @ b P)
  fromChunk g = do
    b ← g
    case b ≡ 𝕟8 0 of
      True → do
        x ← fromChunk g
        skipChunk g $ (chunkSize @ a P ⩏ chunkSize @ b P) - chunkSize @ a P
        return $ Inl x
      False → do
        y ← fromChunk g
        skipChunk g $ (chunkSize @ a P ⩏ chunkSize @ b P) - chunkSize @ b P
        return $ Inr y
  toChunk = \case
    Inl x → single (𝕟8 0) ⧺ toChunk x ⧺ emptyChunk ((chunkSize @ a P ⩏ chunkSize @ b P) - chunkSize @ a P)
    Inr y → single (𝕟8 1) ⧺ toChunk y ⧺ emptyChunk ((chunkSize @ a P ⩏ chunkSize @ b P) - chunkSize @ b P)

-- chunkIOBytes ∷ UArr.UArray ℕ64 ℕ8 → State ℕ64 ℕ8
-- chunkIOBytes a = do
--   i ← next
--   return $ a UArr.! i
