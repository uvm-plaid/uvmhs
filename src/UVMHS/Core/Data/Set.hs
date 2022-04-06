module UVMHS.Core.Data.Set where

import UVMHS.Core.Init

import UVMHS.Core.Classes

import UVMHS.Core.Data.Iter
import UVMHS.Core.Data.Pair
import UVMHS.Core.Data.Function
import UVMHS.Core.Data.Stream ()
import UVMHS.Core.Data.String

import qualified Data.Set as Set
import qualified Prelude as HS

infix 4 ⊆,⊇,∈,∉
infixl 5 ∪,∖
infixl 6 ∩

instance (Ord a) ⇒ Single a (𝑃 a) where single = single𝑃

instance (Ord a) ⇒ POrd (𝑃 a) where (⊑) = (⊆)

instance Null (𝑃 a) where null = pø
instance (Ord a) ⇒ Append (𝑃 a) where (⧺) = (∪)
instance (Ord a) ⇒ Monoid (𝑃 a)

instance (Ord a,Null a) ⇒ Unit (𝑃 a) where unit = single null
instance (Ord a,Append a) ⇒ Cross (𝑃 a) where
  xs ⨳ ys = pow $ do
    x ← iter xs
    y ← iter ys
    return $ x ⧺ y
instance (Ord a,Monoid a) ⇒ Prodoid (𝑃 a)

instance Zero (𝑃 a) where zero = pø
instance (Ord a) ⇒ Plus (𝑃 a) where (+) = (∪)
instance (Ord a) ⇒ Additive (𝑃 a)

instance (Ord a,Zero a) ⇒ One (𝑃 a) where one = single zero
instance (Ord a,Plus a) ⇒ Times (𝑃 a) where
  xs × ys = pow $ do
    x ← iter xs
    y ← iter ys
    return $ x + y

instance Bot (𝑃 a) where bot = pø
instance (Ord a) ⇒ Join (𝑃 a) where (⊔) = (∪)
instance (Ord a) ⇒ JoinLattice (𝑃 a)

instance (Ord a) ⇒ Meet (𝑃 a) where (⊓) = (∩)
instance (Ord a) ⇒ Difference (𝑃 a) where (⊟) = (∖)

instance ToIter a (𝑃 a) where iter = iter𝑃

instance (Ord a,All a) ⇒ All (𝑃 a) where
  all ∷ 𝐼 (𝑃 a)
  all = foldrOnFrom all (single pø) $ \ x xssᵢ → do
    xs ← xssᵢ
    iter $ [ xs , single x ∪ xs ]

instance (Show a) ⇒ Show (𝑃 a) where show = tohsChars ∘ showCollection "{" "}" "," show𝕊

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

(⊈) ∷ (Ord a) ⇒ 𝑃 a → 𝑃 a → 𝔹
(⊈) = not ∘∘ (⊆)

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

iter𝑃 ∷ 𝑃 a → 𝐼 a
iter𝑃 = iterLL ∘ Set.toList ∘ un𝑃

pow𝐼 ∷ (Ord a) ⇒ 𝐼 a → 𝑃 a
pow𝐼 = 𝑃 ∘ Set.fromList ∘ lazyList

pow ∷ (Ord a,ToIter a t) ⇒ t → 𝑃 a
pow = pow𝐼 ∘ iter

uniques ∷ (Ord a,ToIter a t) ⇒ t → 𝐼 a
uniques xs = filterMap id $ appto (iter xs) $ reiter pø $ \ x seen →
  if x ∈ seen 
  then seen :* None 
  else (single x ∪ seen) :* Some x

unions ∷ (Ord a,ToIter (𝑃 a) t) ⇒ t → 𝑃 a
unions = fold pø (∪)
