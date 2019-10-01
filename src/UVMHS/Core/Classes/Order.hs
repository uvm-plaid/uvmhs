module UVMHS.Core.Classes.Order where

import UVMHS.Core.Init

import qualified Prelude as HS

infix 3 ≡,≢,⋚,≤,≥,<,>

-- {-# INLINE (≡) #-}
(≡) ∷ (Eq a) ⇒ a → a → 𝔹
(≡) = (HS.==)

-- {-# INLINE (≢) #-}
(≢) ∷ (Eq a) ⇒ a → a → 𝔹
(≢) = (HS./=)

-- {-# INLINE (⋚) #-}
(⋚) ∷ (Ord a) ⇒ a → a → Ordering
(⋚) = compare

-- {-# INLINE (≤) #-}
(≤) ∷ (Ord a) ⇒ a → a → 𝔹
x ≤ y = case x ⋚ y of {LT → True;EQ → True;GT → False}

-- {-# INLINE (≥) #-}
(≥) ∷ (Ord a) ⇒ a → a → 𝔹
x ≥ y = case x ⋚ y of {LT → False;EQ → True;GT → True}

-- {-# INLINE (<) #-}
(<) ∷ (Ord a) ⇒ a → a → 𝔹
(<) = (HS.<)

-- {-# INLINE (>) #-}
(>) ∷ (Ord a) ⇒ a → a → 𝔹
(>) = (HS.>)

(⩎) ∷ (Ord a) ⇒ a → a → a
x ⩎ y 
  | x ≤ y = x
  | otherwise = y

(⩏) ∷ (Ord a) ⇒ a → a → a
x ⩏ y 
  | x ≤ y = y
  | otherwise = x

minBy ∷ (Ord b) ⇒ (a → b) → a → a → a
minBy f x y 
  | f x ≤ f y = x
  | otherwise = y

maxBy ∷ (Ord b) ⇒ (a → b) → a → a → a
maxBy f x y 
  | f x ≥ f y = x
  | otherwise = y
