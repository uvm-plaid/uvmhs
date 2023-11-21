module UVMHS.Core.VectorSparse where

import UVMHS.Core.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import qualified Prelude as HS

data 𝑉 a = 𝑉 { un𝑉 ∷ IntMap.IntMap a }
  deriving (Eq,Ord)

instance Lookup ℤ64 a (𝑉 a) where (⋕?) = lookup𝑉
instance Single (ℤ64 ∧ a) (𝑉 a) where single = single𝑉

instance (POrd a) ⇒ POrd (𝑉 a) where (⊑) = subDictBy𝑉 (⊑)

instance Null (𝑉 a) where null = wø
instance (Append a) ⇒ Append (𝑉 a) where (⧺) = unionWith𝑉 (⧺)
instance (Append a) ⇒ Monoid (𝑉 a) 

instance (Null a) ⇒ Unit (𝑉 a) where unit = null ↦♮ null
instance (Append a,Cross a) ⇒ Cross (𝑉 a) where
  ixs₁ ⨳ ixs₂ = foldr wø (unionWith𝑉 (⧺)) $ do
    (i₁ :* x₁) ← iter ixs₁
    (i₂ :* x₂) ← iter ixs₂
    return $ (i₁ ⧺ i₂) ↦♮ (x₁ ⨳ x₂)
instance (Prodoid a) ⇒ Prodoid (𝑉 a)

instance Zero (𝑉 a) where zero = wø
instance (Plus a) ⇒ Plus (𝑉 a) where (+) = unionWith𝑉 (+)
instance (Plus a) ⇒ Additive (𝑉 a)

instance (Zero a) ⇒ One (𝑉 a) where one = zero ↦♮ zero
instance (Plus a,Times a) ⇒ Times (𝑉 a) where
  ixs₁ × ixs₂ = fold wø (unionWith𝑉 (+)) $ do
    (i₁ :* x₁) ← iter ixs₁
    (i₂ :* x₂) ← iter ixs₂
    return $ (i₁ + i₂) ↦♮ (x₁ × x₂)
instance (Multiplicative a) ⇒ Multiplicative (𝑉 a)

instance Bot (𝑉 a) where bot = wø
instance (Join a) ⇒ Join (𝑉 a) where (⊔) = unionWith𝑉 (⊔)
instance (Join a) ⇒ JoinLattice (𝑉 a)

instance Top (𝑉 a) where top = wø
instance (Meet a) ⇒ Meet (𝑉 a) where (⊓) = unionWith𝑉 (⊓)
instance (Meet a) ⇒ MeetLattice (𝑉 a)

instance Functor 𝑉 where map = map𝑉

instance ToIter (ℤ64 ∧ a) (𝑉 a) where iter = iter𝑉

instance (Show a) ⇒ Show (𝑉 a) where 
  show = tohsChars ∘ showCollection "{" "}" "," (\ (i :* x) → show𝕊 i ⧺ "⇒" ⧺ show𝕊 x)

lookup𝑉 ∷ 𝑉 a → ℤ64 → 𝑂 a
lookup𝑉 ixs i = frhs $ un𝑉 ixs IntMap.!? tohs i

single𝑉 ∷ ℤ64 ∧ a → 𝑉 a
single𝑉 (i :* x) = 𝑉 $ IntMap.singleton (tohs i) x

subDictBy𝑉 ∷ (a → a → 𝔹) → 𝑉 a → 𝑉 a → 𝔹
subDictBy𝑉 f ixs₁ ixs₂ = IntMap.isSubmapOfBy f (un𝑉 ixs₁) (un𝑉 ixs₂)

wø ∷ 𝑉 a
wø = 𝑉 IntMap.empty

unionWith𝑉 ∷ (a → a → a) → 𝑉 a → 𝑉 a → 𝑉 a
unionWith𝑉 f ixs₁ ixs₂ = 𝑉 $ IntMap.unionWith f (un𝑉 ixs₁) (un𝑉 ixs₂)
  
(↦♮) ∷ ℤ64 → a → 𝑉 a
i ↦♮ x = 𝑉 $ IntMap.singleton (tohs i) x

(⋵♮) ∷ ℤ64 → 𝑉 a → 𝔹
i ⋵♮ ixs = tohs i `IntMap.member` un𝑉 ixs

(⩌♮) ∷ 𝑉 a → 𝑉 a → 𝑉 a
ixs₁ ⩌♮ ixs₂ = 𝑉 $ un𝑉 ixs₁ `IntMap.union` un𝑉 ixs₂

(⩍♮) ∷ 𝑉 a → 𝑉 a → 𝑉 a
ixs₁ ⩍♮ ixs₂ = 𝑉 $ un𝑉 ixs₁ `IntMap.intersection` un𝑉 ixs₂
 
-- (∸♮) ∷ 𝑉 a → 𝑉 a → 𝑉 a
-- ixs₁ ∸♮ ixs₂ = 𝑉 $ un𝑉 ixs₁ `IntMap.difference` un𝑉 ixs₂

delete𝑉 ∷ ℤ64 → 𝑉 a → 𝑉 a
delete𝑉 i ixs = 𝑉 $ IntMap.delete (tohs i) $ un𝑉 ixs

size𝑉 ∷ 𝑉 a → ℕ
size𝑉 = HS.fromIntegral ∘ IntMap.size ∘ un𝑉

(⊎♮) ∷ (Additive a) ⇒ 𝑉 a → 𝑉 a → 𝑉 a
(⊎♮) = unionWith𝑉 (+)

unionsWith𝑉 ∷ (ToIter (𝑉 a) t) ⇒ (a → a → a) → t → 𝑉 a
unionsWith𝑉 = fold wø ∘ unionWith𝑉
 
interWith𝑉 ∷ (a → b → c) → 𝑉 a → 𝑉 b → 𝑉 c
interWith𝑉 f ixs₁ ixs₂ = 𝑉 $ IntMap.intersectionWith f (un𝑉 ixs₁) (un𝑉 ixs₂)

-- -- diffWith ∷ (v → v → v) → 𝑉 a → 𝑉 a → 𝑉 a
-- -- diffWith f ixs₁ ixs₂ = 𝑉 $ IntMap.differenceWith (\ x y → HS.Just (f x y)) (un𝑉 ixs₁) (un𝑉 ixs₂)

minView𝑉 ∷ 𝑉 a → 𝑂 (ℤ64 ∧ a ∧ (𝑉 a))
minView𝑉 = map (mapSnd 𝑉) ∘ frhs ∘ IntMap.minViewWithKey ∘ un𝑉

maxView𝑉 ∷ 𝑉 a → 𝑂 (ℤ64 ∧ a ∧ (𝑉 a))
maxView𝑉 = map (mapSnd 𝑉) ∘ frhs ∘ IntMap.maxViewWithKey ∘ un𝑉

minKey𝑉 ∷ 𝑉 a → 𝑂 ℤ64
minKey𝑉 ixs = fst ∘ fst ^$ minView𝑉 ixs

maxKey𝑉 ∷ 𝑉 a → 𝑂 ℤ64
maxKey𝑉 ixs = fst ∘ fst ^$ maxView𝑉 ixs

view𝑉 ∷ ℤ64 → 𝑉 a → 𝑂 (a ∧ (𝑉 a))
view𝑉 i ixs
  | i ⋵♮ ixs = Some $ (ixs ⋕! i) :* delete𝑉 i ixs
  | otherwise = None

without𝑉 ∷ 𝑃 ℤ64 → 𝑉 a → 𝑉 a
without𝑉 is ixs = 𝑉 $ IntMap.withoutKeys (un𝑉 ixs) $ IntSet.fromList $ lazyList $ map tohs $ iter is

restrict𝑉 ∷ 𝑃 ℤ64 → 𝑉 a → 𝑉 a
restrict𝑉 is ixs = 𝑉 $ IntMap.restrictKeys (un𝑉 ixs) $ IntSet.fromList $ lazyList $ map tohs $ iter is

keys𝑉 ∷ 𝑉 a → 𝐼 ℤ64
keys𝑉 = map frhs ∘ iter ∘ IntMap.keys ∘ un𝑉

values𝑉 ∷ 𝑉 a → 𝐼 a
values𝑉 = map frhs ∘ iter ∘ IntMap.elems ∘ un𝑉

map𝑉 ∷ (a → b) → 𝑉 a → 𝑉 b
map𝑉 f = 𝑉 ∘ IntMap.map f ∘ un𝑉

mapK𝑉 ∷ (ℤ64 → a → b) → 𝑉 a → 𝑉 b
mapK𝑉 f ixs = spvec $ mapOn (iter ixs) $ \ (i :* x) → i ↦♮ f i x

iter𝑉 ∷ 𝑉 a → 𝐼 (ℤ64 ∧ a)
iter𝑉 = map frhs ∘ iterLL ∘ IntMap.toList ∘ un𝑉

spvec𝐼 ∷ 𝐼 (ℤ64 ∧ a) → 𝑉 a
spvec𝐼 = 𝑉 ∘ IntMap.fromList ∘ lazyList ∘ map tohs

spvec ∷ (ToIter (𝑉 a) t) ⇒ t → 𝑉 a
spvec = foldr wø (⩌♮) ∘ iter
 
assoc𝑉 ∷ (ToIter (ℤ64 ∧ a) t) ⇒ t → 𝑉 a
assoc𝑉 = spvec ∘ map single ∘ iter

join𝑉 ∷ (Ord a,Ord b) ⇒ 𝑉 (𝑃 a) → 𝑉 (𝑃 b) → 𝑉 (𝑃 (a ∧ b))
join𝑉 = interWith𝑉 $ \ vs₁ vs₂ → pow𝐼𝑃 $ iter $ zipWith (:*) vs₁ vs₂
