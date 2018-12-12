module UVMHS.Core.Vector where

import UVMHS.Init

import UVMHS.Core.Classes
import UVMHS.Core.Data
import UVMHS.Core.Effects
import UVMHS.Core.Monads

import qualified Data.Array.Unboxed as Arr

import qualified Prelude as HS
import qualified Data.Bits as HS
import qualified Data.Char as HS

import qualified Unsafe.Coerce as UNSAFE

skipChunk ∷ (Monad m) ⇒ m ℕ8 → ℕ64 → m ()
skipChunk g n₀ = loop (natΩ64 0)
  where
    loop n
      | n ≡ n₀ = return ()
      | otherwise = do
          _ ← g
          loop $ succ n

emptyChunk ∷ ℕ64 → 𝐼 ℕ8
emptyChunk n = repeat (nat n) (natΩ8 0)

joinBytes ∷ (ℕ8,ℕ8,ℕ8,ℕ8,ℕ8,ℕ8,ℕ8,ℕ8) → ℕ64
joinBytes (b₁,b₂,b₃,b₄,b₅,b₆,b₇,b₈) =
         HS.shiftL (HS.fromIntegral b₁ ∷ ℕ64) (HS.fromIntegral  0 ∷ HS.Int) 
  HS..|. HS.shiftL (HS.fromIntegral b₂ ∷ ℕ64) (HS.fromIntegral  8 ∷ HS.Int) 
  HS..|. HS.shiftL (HS.fromIntegral b₃ ∷ ℕ64) (HS.fromIntegral 16 ∷ HS.Int) 
  HS..|. HS.shiftL (HS.fromIntegral b₄ ∷ ℕ64) (HS.fromIntegral 24 ∷ HS.Int)
  HS..|. HS.shiftL (HS.fromIntegral b₅ ∷ ℕ64) (HS.fromIntegral 32 ∷ HS.Int) 
  HS..|. HS.shiftL (HS.fromIntegral b₆ ∷ ℕ64) (HS.fromIntegral 40 ∷ HS.Int) 
  HS..|. HS.shiftL (HS.fromIntegral b₇ ∷ ℕ64) (HS.fromIntegral 48 ∷ HS.Int) 
  HS..|. HS.shiftL (HS.fromIntegral b₈ ∷ ℕ64) (HS.fromIntegral 56 ∷ HS.Int)

splitBytes ∷ ℕ64 → (ℕ8,ℕ8,ℕ8,ℕ8,ℕ8,ℕ8,ℕ8,ℕ8)
splitBytes n =
  ( HS.fromIntegral (HS.shiftR n (HS.fromIntegral  0 ∷ HS.Int)) ∷ ℕ8
  , HS.fromIntegral (HS.shiftR n (HS.fromIntegral  8 ∷ HS.Int)) ∷ ℕ8
  , HS.fromIntegral (HS.shiftR n (HS.fromIntegral 16 ∷ HS.Int)) ∷ ℕ8
  , HS.fromIntegral (HS.shiftR n (HS.fromIntegral 24 ∷ HS.Int)) ∷ ℕ8
  , HS.fromIntegral (HS.shiftR n (HS.fromIntegral 32 ∷ HS.Int)) ∷ ℕ8
  , HS.fromIntegral (HS.shiftR n (HS.fromIntegral 40 ∷ HS.Int)) ∷ ℕ8
  , HS.fromIntegral (HS.shiftR n (HS.fromIntegral 48 ∷ HS.Int)) ∷ ℕ8
  , HS.fromIntegral (HS.shiftR n (HS.fromIntegral 56 ∷ HS.Int)) ∷ ℕ8 )

class Chunky a where
  chunkSize ∷ P a → ℕ64
  fromChunk ∷ ∀ m. (Monad m) ⇒ m ℕ8 → m a
  toChunk ∷ a → 𝐼 ℕ8

instance {-# OVERLAPPABLE #-} (Chunky b,a ⇄ b) ⇒ Chunky a where
  chunkSize P = chunkSize @ b P
  fromChunk = map isofr ∘ fromChunk
  toChunk = toChunk ∘ isoto

instance Chunky () where
  chunkSize P = natΩ64 0
  fromChunk _g = return ()
  toChunk () = empty𝐼

instance Chunky ℕ8 where
  chunkSize P = natΩ64 1
  fromChunk = id
  toChunk = single

instance Chunky 𝔹 where
  chunkSize P = natΩ64 1
  fromChunk g = do
    b ← g
    return $ case b ≡ natΩ8 0 of
      True → False 
      False → True
  toChunk b = toChunk $ case b of
    False → natΩ8 0
    True → natΩ8 1

instance Chunky ℂ where
  chunkSize P = natΩ64 4
  fromChunk g = do
    b₁ ← g ; b₂ ← g ; b₃ ← g ; b₄ ← g
    return $ HS.chr $ HS.fromIntegral $ joinBytes (b₁,b₂,b₃,b₄,natΩ8 0,natΩ8 0,natΩ8 0,natΩ8 0)
  toChunk c = 𝐼 $ \ (f ∷ ℕ8 → b → b) →
    let (b₁,b₂,b₃,b₄,_,_,_,_) = splitBytes $ HS.fromIntegral $ HS.ord c
    in f b₄ ∘ f b₃ ∘ f b₂ ∘ f b₁

instance Chunky ℕ64 where
  chunkSize P = natΩ64 8
  fromChunk g = do
    b₁ ← g ; b₂ ← g ; b₃ ← g ; b₄ ← g
    b₅ ← g ; b₆ ← g ; b₇ ← g ; b₈ ← g
    return $ joinBytes (b₁,b₂,b₃,b₄,b₅,b₆,b₇,b₈)
  toChunk n = 𝐼 $ \ (f ∷ ℕ8 → b → b) →
    let (b₁,b₂,b₃,b₄,b₅,b₆,b₇,b₈) = splitBytes n
    in f b₈ ∘ f b₇ ∘ f b₆ ∘ f b₅ ∘ f b₄ ∘ f b₃ ∘ f b₂ ∘ f b₁

instance Chunky ℤ64 where
  chunkSize P = natΩ64 8
  fromChunk = map (UNSAFE.unsafeCoerce ∷ ℕ64 → ℤ64) ∘ fromChunk
  toChunk = toChunk ∘ (UNSAFE.unsafeCoerce ∷ ℤ64 → ℕ64)

instance Chunky 𝔻 where
  chunkSize P = natΩ64 8
  fromChunk = map (UNSAFE.unsafeCoerce ∷ ℕ64 → 𝔻) ∘ fromChunk
  toChunk = toChunk ∘ (UNSAFE.unsafeCoerce ∷ 𝔻 → ℕ64)

instance (Chunky a,Chunky b) ⇒ Chunky (a ∧ b) where
  chunkSize P = chunkSize @ a P + chunkSize @ b P
  fromChunk g = do
    x ← fromChunk g
    y ← fromChunk g
    return $ x :* y
  toChunk (x :* y) = toChunk x ⧺ toChunk y

instance (Chunky a,Chunky b) ⇒ Chunky (a ∨ b) where
  chunkSize P = natΩ64 1 + (chunkSize @ a P ⩏ chunkSize @ b P)
  fromChunk g = do
    b ← g
    case b ≡ natΩ8 0 of
      True → do
        x ← fromChunk g
        skipChunk g $ (chunkSize @ a P ⩏ chunkSize @ b P) - chunkSize @ a P
        return $ Inl x
      False → do
        y ← fromChunk g
        skipChunk g $ (chunkSize @ a P ⩏ chunkSize @ b P) - chunkSize @ b P
        return $ Inr y
  toChunk = \case
    Inl x → single (natΩ8 0) ⧺ toChunk x ⧺ emptyChunk ((chunkSize @ a P ⩏ chunkSize @ b P) - chunkSize @ a P)
    Inr y → single (natΩ8 1) ⧺ toChunk y ⧺ emptyChunk ((chunkSize @ a P ⩏ chunkSize @ b P) - chunkSize @ b P)

chunkIOBytes ∷ Arr.UArray ℕ64 ℕ8 → State ℕ64 ℕ8
chunkIOBytes a = do
  i ← next
  return $ a Arr.! i

newtype 𝕍 a = 𝕍 (Arr.UArray ℕ64 ℕ8)

instance (Chunky a) ⇒ ToStream a (𝕍 a) where stream = stream𝕍
instance (Chunky a) ⇒ ToIter a (𝕍 a) where iter = iter ∘ stream
instance (Chunky a,Show a) ⇒ Show (𝕍 a) where show = chars ∘ showWith𝕍 show𝕊
instance (Chunky a) ⇒ Lookup ℕ64 a (𝕍 a) where (⋕?) = idx𝕍𝑂
instance Null (𝕍 a) where null = 𝕍 $ Arr.listArray (natΩ64 1,natΩ64 0) []
instance (Chunky a) ⇒ Append (𝕍 a) where xs ⧺ ys = vec (iter xs ⧺ iter ys)

instance Eq (𝕍 a) where xs == ys = streamBytes𝕍 xs ≡ streamBytes𝕍 ys
instance Ord (𝕍 a) where compare xs ys = streamBytes𝕍 xs ⋚ streamBytes𝕍 ys

idxᐪ𝕍 ∷ ∀ a. (Chunky a) ⇒ 𝕍 a → ℕ64
idxᐪ𝕍 (𝕍 a) =
  let (_,iᵀ) = Arr.bounds a
  in iᵀ ⌿ chunkSize @ a P

rawIdx𝕍 ∷ (Chunky a) ⇒ P a → ℕ64 → ℕ64
rawIdx𝕍 p i = (i - natΩ64 1) × chunkSize p + natΩ64 1

idx𝕍 ∷ ∀ a. (Chunky a) ⇒ 𝕍 a → ℕ64 → a
idx𝕍 (𝕍 a) i = evalState (rawIdx𝕍 @ a P i) $ fromChunk $ chunkIOBytes a

idx𝕍𝑂 ∷ (Chunky a) ⇒ 𝕍 a → ℕ64 → 𝑂 a
idx𝕍𝑂 a i 
  | (i < natΩ64 0) ⩔ (i > idxᐪ𝕍 a) = None
  | otherwise = Some $ idx𝕍 a i

vec ∷ ∀ t a. (ToIter a t,Chunky a) ⇒ t → 𝕍 a
vec xs = 𝕍 $ Arr.listArray (natΩ64 1,natΩ64 (count xs) × chunkSize @ a P) $ lazyList $ mjoin $ map toChunk $ iter xs

stream𝕍 ∷ ∀ a. (Chunky a) ⇒ 𝕍 a → 𝑆 a
stream𝕍 xs = 
  let ιᵀ = idxᐪ𝕍 xs
      g ∷ ℕ64 → 𝑂 (a ∧ ℕ64)
      g ι | ι > ιᵀ = None
          | otherwise = Some (idx𝕍 xs ι :* succ ι)
  in 𝑆 (natΩ64 1) g

-- iter𝕍 ∷ ∀ a. (Chunky a) ⇒ 𝕍 a → 𝐼 a
-- iter𝕍 a = 
--   let ιᐪ = idxᐪ𝕍 a
--   in 𝐼 $ \ (f ∷ a → b → b) (i₀ ∷ b) →
--     let loop ι 
--           | ι > ιᐪ = id
--           | otherwise = loop (succ ι) ∘ f (idx𝕍 a ι)
--     in loop (natΩ64 1) i₀

showWith𝕍 ∷ (Chunky a) ⇒ (a → 𝕊) → 𝕍 a → 𝕊
showWith𝕍 = showCollection "𝕍[" "]" ","

streamBytes𝕍 ∷ 𝕍 a → 𝑆 ℕ8
streamBytes𝕍 (𝕍 a) =
  let (i₁,iₙ) = Arr.bounds a
  in 𝑆 i₁ $ \ i →
    case i > iₙ of
      True → abort
      False → return $ (a Arr.! i) :* succ i

corelib_vector_e1 ∷ 𝕍 (ℕ64 ∨ (ℕ64 ∧ ℕ64))
corelib_vector_e1 = vec $ vec $ mapOn (upTo 10) $ \ x → 
  case even x of
    True → Inl $ natΩ64 x 
    False → Inr $ natΩ64 x :* natΩ64 99

corelib_vector_e2 ∷ 𝕍 ℂ
corelib_vector_e2 = vec ['a','b','c','d','e','f']

corelib_vector_e3 ∷ 𝕍 𝔹
corelib_vector_e3 = vec $ map (elimChoice even $ even ∘ fst) $ iter corelib_vector_e1

