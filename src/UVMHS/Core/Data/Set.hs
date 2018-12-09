module UVMHS.Core.Data.Set where

import UVMHS.Init

import UVMHS.Core.Classes

import UVMHS.Core.Data.Iter
import UVMHS.Core.Data.LazyList
import UVMHS.Core.Data.Pair
import UVMHS.Core.Data.Stream ()
import UVMHS.Core.Data.String

import qualified Data.Set as Set
import qualified Prelude as HS

instance (Ord a) ⇒ Single a (𝑃 a) where single = single𝑃

instance (Ord a) ⇒ POrd (𝑃 a) where 
  xs ∇ ys 
    | xs ⊆ ys = PLT
    | xs ⊇ ys = PGT
    | otherwise = PUN

instance Null (𝑃 a) where null = pø
instance (Ord a) ⇒ Append (𝑃 a) where (⧺) = (∪)
instance (Ord a) ⇒ Additive (𝑃 a) where {zero = pø;(+) = (∪)}
instance Bot (𝑃 a) where bot = pø
instance (Ord a) ⇒ Join (𝑃 a) where (⊔) = (∪)
instance (Ord a) ⇒ Meet (𝑃 a) where (⊓) = (∩)

instance ToStream a (𝑃 a) where stream = stream𝑃
instance ToIter a (𝑃 a) where iter = iter ∘ stream

instance (Show a) ⇒ Show (𝑃 a) where show = chars ∘ showCollection "{" "}" "," show𝕊

pø ∷ 𝑃 a
pø = 𝑃 Set.empty

single𝑃 ∷ (Ord a) ⇒ a → 𝑃 a
single𝑃 x = 𝑃 $ Set.singleton x

(∈) ∷ (Ord a) ⇒ a → 𝑃 a → 𝔹
x ∈ xs = Set.member x $ un𝑃 xs

(∉) ∷ (Ord a) ⇒ a → 𝑃 a → 𝔹
x ∉ xs = not (x ∈ xs)

(⊆) ∷ (Ord a) ⇒ 𝑃 a → 𝑃 a → 𝔹
xs ⊆ ys = un𝑃 xs `Set.isSubsetOf` un𝑃 ys

(⊇) ∷ (Ord a) ⇒ 𝑃 a → 𝑃 a → 𝔹
(⊇) = flip (⊆)

(∪) ∷ (Ord a) ⇒ 𝑃 a → 𝑃 a → 𝑃 a
xs ∪ ys = 𝑃 $ un𝑃 xs `Set.union` un𝑃 ys

(∩) ∷ (Ord a) ⇒ 𝑃 a → 𝑃 a → 𝑃 a
xs ∩ ys = 𝑃 $ un𝑃 xs `Set.intersection` un𝑃 ys

(∖) ∷ (Ord a) ⇒ 𝑃 a → 𝑃 a → 𝑃 a
xs ∖ ys = 𝑃 $ un𝑃 xs `Set.difference` un𝑃 ys

psize ∷ 𝑃 a → ℕ
psize = HS.fromIntegral ∘ Set.size ∘ un𝑃

pmin ∷ 𝑃 a → 𝑂 (a ∧ 𝑃 a)
pmin = map (mapSnd 𝑃) ∘ frhs ∘ Set.minView ∘ un𝑃

pmax ∷ 𝑃 a → 𝑂 (a ∧ 𝑃 a)
pmax = map (mapSnd 𝑃) ∘ frhs ∘ Set.maxView ∘ un𝑃

pmap ∷ (Ord b) ⇒ (a → b) → 𝑃 a → 𝑃 b
pmap f = 𝑃 ∘ Set.map f ∘ un𝑃

stream𝑃 ∷ 𝑃 a → 𝑆 a
stream𝑃 = stream ∘ Set.toList ∘ un𝑃

pow𝐼 ∷ (Ord a) ⇒ 𝐼 a → 𝑃 a
pow𝐼 = 𝑃 ∘ Set.fromList ∘ lazyList

pow ∷ (Ord a,ToIter a t) ⇒ t → 𝑃 a
pow = pow𝐼 ∘ iter

uniques ∷ (Ord a,ToIter a t) ⇒ t → 𝐼 a
uniques xs = 𝐼 $ \ (f ∷ a → b → b) (i₀ ∷ b) →
  snd $ foldWith xs (bot :꘍ i₀) $ \ (x ∷ a) (seen :꘍ i ∷ 𝑃 a ∧ b) → case x ∈ seen of
    True → seen :꘍ i
    False → (single x ∪ seen) :꘍ f x i
