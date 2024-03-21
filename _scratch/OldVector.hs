module UVMHS.Core.Vector where

import UVMHS.Core.Init

import UVMHS.Core.Classes
import UVMHS.Core.Data
import UVMHS.Core.Effects
import UVMHS.Core.Monads

import qualified Data.Array as BArr
import qualified Data.Array.Unboxed as UArr

import qualified Prelude as HS
import qualified Data.Bits as HS
import qualified Data.Char as HS

import qualified Unsafe.Coerce as UNSAFE

-------
-- 𝕍 --
-------

newtype 𝕍 a = 𝕍 (BArr.Array ℕ64 a)

instance ToStream a (𝕍 a) where stream = stream𝕍
instance ToIter a (𝕍 a) where iter = iter ∘ stream
instance (Show a) ⇒ Show (𝕍 a) where show = chars ∘ showCollection "𝕍[" "]" "," show𝕊
instance Lookup ℕ64 a (𝕍 a) where (⋕?) = idx𝕍𝑂
instance Null (𝕍 a) where null = 𝕍 $ BArr.listArray (𝕟64 1,𝕟64 0) []
instance Append (𝕍 a) where xs ⧺ ys = vec (iter xs ⧺ iter ys)

instance (Eq a) ⇒ Eq (𝕍 a) where xs == ys = stream xs ≡ stream ys
instance (Ord a) ⇒ Ord (𝕍 a) where compare xs ys = stream xs ⋚ stream ys

instance Sized (𝕍 a) where size = size𝕍

instance Functor 𝕍 where map = map𝕍

vecN ∷ (ToIter a t) ⇒ ℕ64 → t → 𝕍 a
vecN l xs
  | l ≡ 𝕟64 0 = 𝕍 $ BArr.listArray (𝕟64 1,𝕟64 0) []
  | otherwise = 𝕍 $ BArr.listArray (𝕟64 0,l - 𝕟64 1) $ lazyList $ iter xs

vecS ∷ (ToIter a t,Sized t) ⇒ t → 𝕍 a
vecS xs = vecN (size xs) xs

vec ∷ (ToIter a t) ⇒ t → 𝕍 a
vec xs = vecN (𝕟64 $ count xs) xs

vecF ∷ ℕ64 → (ℕ64 → a) → 𝕍 a
vecF n f = vecN n $ map (f ∘ 𝕟64) $ upTo $ nat n

vecD ∷ ℕ64 ⇰ a → 𝕍 a
vecD d = case dmaxKey d of
  None → error "vecD on empty dictionary"
  Some k → vecF (k + one) $ \ n → d ⋕! n

idxOK𝕍 ∷ 𝕍 a → ℕ64 → 𝔹
idxOK𝕍 (𝕍 a) ι =
  let (ιᴮ,ιᵀ) = BArr.bounds a
  in (ι ≥ ιᴮ) ⩓ (ι ≤ ιᵀ)

idx𝕍 ∷ 𝕍 a → ℕ64 → a
idx𝕍 (𝕍 a) ι = a BArr.! ι

idx𝕍𝑂 ∷ 𝕍 a → ℕ64 → 𝑂 a
idx𝕍𝑂 a ι
  | idxOK𝕍 a ι = Some $ idx𝕍 a ι
  | otherwise = None

stream𝕍 ∷ 𝕍 a → 𝑆 a
stream𝕍 xs = 𝑆 (𝕟64 0) $ \ ι → do
  x ← idx𝕍𝑂 xs ι
  return $ x :* succ ι

size𝕍 ∷ 𝕍 a → ℕ64
size𝕍 (𝕍 a) =
  let (ιᴮ,ιᵀ) = BArr.bounds a
  in if ιᴮ > ιᵀ then zero else ιᵀ + one

map𝕍 ∷ (a → b) → 𝕍 a → 𝕍 b
map𝕍 f xs = vecN (size xs) $ map f $ iter xs

set𝕍 ∷ ℕ64 → a → 𝕍 a → 𝕍 a
set𝕍 i x (𝕍 a) = 𝕍 $ a BArr.// [(i,x)]

------------
-- Chunks --
------------

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
  chunkSize P = 𝕟64 0
  fromChunk _g = return ()
  toChunk () = empty𝐼

instance Chunky ℕ8 where
  chunkSize P = 𝕟64 1
  fromChunk = id
  toChunk = single

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
    return $ HS.chr $ HS.fromIntegral $ joinBytes (b₁,b₂,b₃,b₄,𝕟8 0,𝕟8 0,𝕟8 0,𝕟8 0)
  toChunk c = 𝐼 $ \ (f ∷ ℕ8 → b → b) →
    let (b₁,b₂,b₃,b₄,_,_,_,_) = splitBytes $ HS.fromIntegral $ HS.ord c
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
  fromChunk = map (UNSAFE.unsafeCoerce ∷ ℕ64 → ℤ64) ∘ fromChunk
  toChunk = toChunk ∘ (UNSAFE.unsafeCoerce ∷ ℤ64 → ℕ64)

instance Chunky 𝔻 where
  chunkSize P = 𝕟64 8
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

chunkIOBytes ∷ UArr.UArray ℕ64 ℕ8 → State ℕ64 ℕ8
chunkIOBytes a = do
  i ← next
  return $ a UArr.! i

-------
-- 𝕌 --
-------

newtype 𝕌 a = 𝕌 (UArr.UArray ℕ64 ℕ8)

instance (Chunky a) ⇒ ToStream a (𝕌 a) where stream = stream𝕌
instance (Chunky a) ⇒ ToIter a (𝕌 a) where iter = iter ∘ stream
instance (Chunky a,Show a) ⇒ Show (𝕌 a) where show = chars ∘ showWith𝕌 show𝕊
instance (Chunky a) ⇒ Lookup ℕ64 a (𝕌 a) where (⋕?) = idx𝕌𝑂
instance Null (𝕌 a) where null = 𝕌 $ UArr.listArray (𝕟64 1,𝕟64 0) []
instance (Chunky a) ⇒ Append (𝕌 a) where xs ⧺ ys = uvec (iter xs ⧺ iter ys)

instance (Chunky a,Eq a) ⇒ Eq (𝕌 a) where xs == ys = stream xs ≡ stream ys
instance (Chunky a,Ord a) ⇒ Ord (𝕌 a) where compare xs ys = stream xs ⋚ stream ys

uvecN ∷ ∀ t a. (ToIter a t,Chunky a) ⇒ ℕ64 → t → 𝕌 a
uvecN l xs
  | l ≡ 𝕟64 0 = 𝕌 $ UArr.listArray (𝕟64 1,𝕟64 0) []
  | otherwise = 𝕌 $ UArr.listArray (𝕟64 0,l × chunkSize @ a P - 𝕟64 1) $ lazyList $ iter xs ≫= toChunk

uvec ∷ (ToIter a t,Chunky a) ⇒ t → 𝕌 a
uvec xs = uvecN (𝕟64 $ count xs) xs

idxOK𝕌 ∷ ∀ a. (Chunky a) ⇒ 𝕌 a → ℕ64 → 𝔹
idxOK𝕌 (𝕌 a) ι =
  let (ιᴮ,ιᵀ) = UArr.bounds a
      ιᵀ' = ((ιᵀ + 𝕟64 1) ⌿ chunkSize @ a P) - 𝕟64 1
  in (ι ≥ ιᴮ) ⩓ (ι ≤ ιᵀ')

rawIdx𝕌 ∷ (Chunky a) ⇒ P a → ℕ64 → ℕ64
rawIdx𝕌 p i = i × chunkSize p

idx𝕌 ∷ ∀ a. (Chunky a) ⇒ 𝕌 a → ℕ64 → a
idx𝕌 (𝕌 a) i = evalState (rawIdx𝕌 @ a P i) $ fromChunk $ chunkIOBytes a

idx𝕌𝑂 ∷ (Chunky a) ⇒ 𝕌 a → ℕ64 → 𝑂 a
idx𝕌𝑂 a i
  | idxOK𝕌 a i = Some $ idx𝕌 a i
  | otherwise = None

stream𝕌 ∷ ∀ a. (Chunky a) ⇒ 𝕌 a → 𝑆 a
stream𝕌 xs = 𝑆 (𝕟64 0) $ \ ι → do
  x ← idx𝕌𝑂 xs ι
  return $ x :* succ ι

showWith𝕌 ∷ (Chunky a) ⇒ (a → 𝕊) → 𝕌 a → 𝕊
showWith𝕌 = showCollection "𝕌[" "]" ","

streamBytes𝕌 ∷ 𝕌 a → 𝑆 ℕ8
streamBytes𝕌 (𝕌 a) =
  let (i₁,iₙ) = UArr.bounds a
  in 𝑆 i₁ $ \ i →
    case i > iₙ of
      True → abort
      False → return $ (a UArr.! i) :* succ i

-- examples --

corelib_vector_e1 ∷ 𝕌 (ℕ64 ∨ (ℕ64 ∧ ℕ64))
corelib_vector_e1 = uvec $ mapOn (upTo 10) $ \ x →
  case even x of
    True → Inl $ 𝕟64 x
    False → Inr $ 𝕟64 x :* 𝕟64 99

corelib_vector_e2 ∷ 𝕌 ℂ
corelib_vector_e2 = uvec ['a','b','c','d','e','f']

corelib_vector_e3 ∷ 𝕌 𝔹
corelib_vector_e3 = uvec $ map (elimChoice even $ even ∘ fst) $ iter corelib_vector_e1
