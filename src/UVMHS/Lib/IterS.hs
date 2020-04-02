module UVMHS.Lib.IterS where

import UVMHS.Core

--------
-- 𝐼S --
--------

data 𝐼S a = 𝐼S 
  { 𝑖SSize ∷ ℕ64
  , 𝑖SIter ∷ 𝐼 a
  } deriving (Show)

instance Null (𝐼S a) where 
  null = 𝐼S zero null
instance Append (𝐼S a) where
  𝐼S s₁ xs₁ ⧺ 𝐼S s₂ xs₂ = 𝐼S (s₁ + s₂) (xs₁ ⧺ xs₂)
instance Monoid (𝐼S a)

instance ToIter a (𝐼S a) where iter = 𝑖SIter
instance Single a (𝐼S a) where single = 𝐼S one ∘ single
instance Sized (𝐼S a) where size = 𝑖SSize

iterS ∷ (ToIter a t,Sized t) ⇒ t → 𝐼S a
iterS xs = 𝐼S (size xs) $ iter xs

---------
-- 𝐼S𝕊 --
---------

data 𝐼S𝕊 = 𝐼S𝕊
  { 𝕤SSize ∷ ℕ64
  , 𝕤SIter ∷ 𝐼 𝕊
  } deriving (Show)

instance Null 𝐼S𝕊 where 
  null = 𝐼S𝕊 zero null
instance Append 𝐼S𝕊 where
  𝐼S𝕊 s₁ xs₁ ⧺ 𝐼S𝕊 s₂ xs₂ = 𝐼S𝕊 (s₁ + s₂) (xs₁ ⧺ xs₂)
instance Monoid 𝐼S𝕊

instance ToIter 𝕊 𝐼S𝕊 where iter = 𝕤SIter
instance Single 𝕊 𝐼S𝕊 where single s = 𝐼S𝕊 (𝕟64 $ length𝕊 s) $ single s
instance Sized 𝐼S𝕊 where size = 𝕤SSize
