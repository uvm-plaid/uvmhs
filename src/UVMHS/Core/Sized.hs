module UVMHS.Core.Sized where

import UVMHS.Core.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data

import UVMHS.Core.LensDeriving

instance CSized 𝕊 where csize = natΩ64 ∘ length𝕊

---------
-- 𝐼A --
---------

data 𝐼A a = 𝐼A
  { 𝑖aSize ∷ ℕ64
  , 𝑖aIter ∷ 𝐼 a
  } deriving (Show)
makeLenses ''𝐼A

class ToIterA a t | t → a where iterA ∷ t → 𝐼A a
instance ToIterA a (𝐼A a) where iterA = id

instance Null   (𝐼A a) where null                  = 𝐼A zero null
instance Append (𝐼A a) where 𝐼A a₁ xs₁ ⧺ 𝐼A a₂ xs₂ = 𝐼A (a₁ + a₂) $ xs₁ ⧺ xs₂
instance Monoid (𝐼A a)

instance              ToIter a (𝐼A a) where iter     = 𝑖aIter
instance (ASized a) ⇒ Single a (𝐼A a) where single s = 𝐼A (asize s) $ single s
instance              ASized   (𝐼A a) where asize    = 𝑖aSize

instance FunctorM 𝐼C where mapM f (𝐼C n xs) = 𝐼C n ^$ mapM f xs

iterAI ∷ (ToIter a t,ASized a) ⇒ t → 𝐼A a
iterAI xs = 𝐼A (sum $ map asize $ iter xs) $ iter xs

--------
-- 𝐼C --
--------

data 𝐼C a = 𝐼C
  { 𝑖cSize ∷ ℕ64
  , 𝑖cIter ∷ 𝐼 a
  } deriving (Show)
makeLenses ''𝐼C

class ToIterC a t | t → a where iterC ∷ t → 𝐼C a
instance ToIterC a (𝐼C a) where iterC = id

instance Null   (𝐼C a) where null                  = 𝐼C zero null
instance Append (𝐼C a) where 𝐼C c₁ xs₁ ⧺ 𝐼C c₂ xs₂ = 𝐼C (c₁ + c₂) (xs₁ ⧺ xs₂)
instance Monoid (𝐼C a)

instance ToIter a (𝐼C a) where iter   = 𝑖cIter
instance Single a (𝐼C a) where single = 𝐼C one ∘ single
instance CSized   (𝐼C a) where csize  = 𝑖cSize

instance Functor 𝐼C where map f (𝐼C c xs) = 𝐼C c $ map f xs

iterCI ∷ (ToIter a t) ⇒ t → 𝐼C a
iterCI xs = 𝐼C (count xs) $ iter xs

---------
-- 𝐼AC --
---------

data 𝐼AC a = 𝐼AC
  { 𝑖acSize ∷ ℕ64
  , 𝑖acCSize ∷ ℕ64
  , 𝑖acIter ∷ 𝐼 a
  } deriving (Show)
makeLenses ''𝐼AC

class ToIterAC a t | t → a where iterAC ∷ t → 𝐼AC a
instance ToIterAC a (𝐼AC a) where iterAC = id

instance Null   (𝐼AC a) where null                          = 𝐼AC zero zero null
instance Append (𝐼AC a) where 𝐼AC c₁ a₁ xs₁ ⧺ 𝐼AC c₂ a₂ xs₂ = 𝐼AC (c₁ + c₂) (a₁ + a₂) (xs₁ ⧺ xs₂)
instance Monoid (𝐼AC a)

instance              ToIter a (𝐼AC a) where iter     = 𝑖acIter
instance (ASized a) ⇒ Single a (𝐼AC a) where single s = 𝐼AC one (asize s) $ single s
instance              ASized   (𝐼AC a) where asize    = 𝑖acSize
instance              CSized   (𝐼AC a) where csize    = 𝑖acCSize

iterACI ∷ (ToIter a t,ASized a) ⇒ t → 𝐼AC a
iterACI xs = 𝐼AC (count xs) (sum $ map asize $ iter xs) $ iter xs

---------------
-- Instances --
---------------

instance                       ASized 𝕊       where asize           = length64𝕊
instance (ASized a,ASized b) ⇒ ASized (a ∨ b) where asize           = elimChoice asize asize
instance (ASized a,ASized b) ⇒ ASized (a ∧ b) where asize (x :* y)  = asize x + asize y

---------------
-- Utilities --
---------------

buildC ∷ ℕ64 → a → (a → a) → 𝐼C a
buildC n x g = 𝐼C n $ build n x g

uptoC ∷ ℕ64 → 𝐼C ℕ64
uptoC n = buildC n zero succ

stringCS ∷ (ToIter ℂ t,CSized t) ⇒ t → 𝕊
stringCS ss = build𝕊CN (csize ss) ss

stringSS ∷ (ToIter 𝕊 t,ASized t) ⇒ t → 𝕊
stringSS ss = build𝕊SN (asize ss) ss

mapState𝐼C ∷ (ToIterC a t) ⇒ s → (a → s → (s ∧ b)) → t → 𝐼C b
mapState𝐼C s f (iterC → 𝐼C n xs) = 𝐼C n $ mapState𝐼 s f xs

withIndexC ∷ ∀ t a. (ToIterC a t) ⇒ t → 𝐼C (ℕ64 ∧ a)
withIndexC = mapState𝐼C zero $ \ x i → (i + one) :* (i :* x)

zipWithC ∷ (ToIterC a t₁,ToIterC b t₂) ⇒ (a → b → c) → t₁ → t₂ → 𝐼C c
zipWithC f (iterC → 𝐼C n₁ xs) (iterC → 𝐼C n₂ ys) = 𝐼C (n₁ ⊓ n₂) $ zipWith f xs ys

zipC ∷ (ToIterC a t₁,ToIterC b t₂) ⇒ t₁ → t₂ → 𝐼C (a ∧ b)
zipC = zipWithC (:*)

prodWith𝐼C ∷ (a → b → c) → 𝐼C a → 𝐼C b → 𝐼C c
prodWith𝐼C f (𝐼C n₁ xs) (𝐼C n₂ ys) = 𝐼C (n₁ × n₂) $ do
  x ← xs
  y ← ys
  return $ f x y
