module UVMHS.Core.VectorSparse where

import UVMHS.Core.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import qualified Prelude as HS

data 𝑊 a = 𝑊 { un𝑊 ∷ IntMap.IntMap a }
  deriving (Eq,Ord)

instance Lookup ℤ64 a (𝑊 a) where (⋕?) = lookup𝑊
instance Single (ℤ64 ∧ a) (𝑊 a) where single = single𝑊

instance (POrd a) ⇒ POrd (𝑊 a) where (⊑) = subDictBy𝑊 (⊑)

instance Null (𝑊 a) where null = wø
instance (Append a) ⇒ Append (𝑊 a) where (⧺) = unionWith𝑊 (⧺)
instance (Append a) ⇒ Monoid (𝑊 a) 

instance (Null a) ⇒ Unit (𝑊 a) where unit = null ↦♮ null
instance (Append a,Cross a) ⇒ Cross (𝑊 a) where
  ixs₁ ⨳ ixs₂ = foldr wø (unionWith𝑊 (⧺)) $ do
    (i₁ :* x₁) ← iter ixs₁
    (i₂ :* x₂) ← iter ixs₂
    return $ (i₁ ⧺ i₂) ↦♮ (x₁ ⨳ x₂)
instance (Prodoid a) ⇒ Prodoid (𝑊 a)

instance Zero (𝑊 a) where zero = wø
instance (Plus a) ⇒ Plus (𝑊 a) where (+) = unionWith𝑊 (+)
instance (Plus a) ⇒ Additive (𝑊 a)

instance (Zero a) ⇒ One (𝑊 a) where one = zero ↦♮ zero
instance (Plus a,Times a) ⇒ Times (𝑊 a) where
  ixs₁ × ixs₂ = fold wø (unionWith𝑊 (+)) $ do
    (i₁ :* x₁) ← iter ixs₁
    (i₂ :* x₂) ← iter ixs₂
    return $ (i₁ + i₂) ↦♮ (x₁ × x₂)
instance (Multiplicative a) ⇒ Multiplicative (𝑊 a)

instance Bot (𝑊 a) where bot = wø
instance (Join a) ⇒ Join (𝑊 a) where (⊔) = unionWith𝑊 (⊔)
instance (Join a) ⇒ JoinLattice (𝑊 a)

instance Top (𝑊 a) where top = wø
instance (Meet a) ⇒ Meet (𝑊 a) where (⊓) = unionWith𝑊 (⊓)
instance (Meet a) ⇒ MeetLattice (𝑊 a)

instance Functor 𝑊 where map = map𝑊

instance ToStream (ℤ64 ∧ a) (𝑊 a) where stream = stream𝑊
instance ToIter (ℤ64 ∧ a) (𝑊 a) where iter = iter ∘ stream

instance (Show a) ⇒ Show (𝑊 a) where show = chars ∘ showCollection "{" "}" "," (\ (i :* x) → show𝕊 i ⧺ "⇒" ⧺ show𝕊 x)

lookup𝑊 ∷ 𝑊 a → ℤ64 → 𝑂 a
lookup𝑊 ixs i = frhs $ un𝑊 ixs IntMap.!? tohs i

single𝑊 ∷ ℤ64 ∧ a → 𝑊 a
single𝑊 (i :* x) = 𝑊 $ IntMap.singleton (tohs i) x

subDictBy𝑊 ∷ (a → a → 𝔹) → 𝑊 a → 𝑊 a → 𝔹
subDictBy𝑊 f ixs₁ ixs₂ = IntMap.isSubmapOfBy f (un𝑊 ixs₁) (un𝑊 ixs₂)

wø ∷ 𝑊 a
wø = 𝑊 IntMap.empty

unionWith𝑊 ∷ (a → a → a) → 𝑊 a → 𝑊 a → 𝑊 a
unionWith𝑊 f ixs₁ ixs₂ = 𝑊 $ IntMap.unionWith f (un𝑊 ixs₁) (un𝑊 ixs₂)
  
(↦♮) ∷ ℤ64 → a → 𝑊 a
i ↦♮ x = 𝑊 $ IntMap.singleton (tohs i) x

(⋵♮) ∷ ℤ64 → 𝑊 a → 𝔹
i ⋵♮ ixs = tohs i `IntMap.member` un𝑊 ixs

(⩌♮) ∷ 𝑊 a → 𝑊 a → 𝑊 a
ixs₁ ⩌♮ ixs₂ = 𝑊 $ un𝑊 ixs₁ `IntMap.union` un𝑊 ixs₂

(⩍♮) ∷ 𝑊 a → 𝑊 a → 𝑊 a
ixs₁ ⩍♮ ixs₂ = 𝑊 $ un𝑊 ixs₁ `IntMap.intersection` un𝑊 ixs₂
 
-- (∸♮) ∷ 𝑊 a → 𝑊 a → 𝑊 a
-- ixs₁ ∸♮ ixs₂ = 𝑊 $ un𝑊 ixs₁ `IntMap.difference` un𝑊 ixs₂

delete𝑊 ∷ ℤ64 → 𝑊 a → 𝑊 a
delete𝑊 i ixs = 𝑊 $ IntMap.delete (tohs i) $ un𝑊 ixs

size𝑊 ∷ 𝑊 a → ℕ
size𝑊 = HS.fromIntegral ∘ IntMap.size ∘ un𝑊

(⊎♮) ∷ (Additive a) ⇒ 𝑊 a → 𝑊 a → 𝑊 a
(⊎♮) = unionWith𝑊 (+)

unionsWith𝑊 ∷ (ToIter (𝑊 a) t) ⇒ (a → a → a) → t → 𝑊 a
unionsWith𝑊 = fold wø ∘ unionWith𝑊
 
interWith𝑊 ∷ (a → b → c) → 𝑊 a → 𝑊 b → 𝑊 c
interWith𝑊 f ixs₁ ixs₂ = 𝑊 $ IntMap.intersectionWith f (un𝑊 ixs₁) (un𝑊 ixs₂)

-- -- diffWith ∷ (v → v → v) → 𝑊 a → 𝑊 a → 𝑊 a
-- -- diffWith f ixs₁ ixs₂ = 𝑊 $ IntMap.differenceWith (\ x y → HS.Just (f x y)) (un𝑊 ixs₁) (un𝑊 ixs₂)

minView𝑊 ∷ 𝑊 a → 𝑂 (ℤ64 ∧ a ∧ (𝑊 a))
minView𝑊 = map (mapSnd 𝑊) ∘ frhs ∘ IntMap.minViewWithKey ∘ un𝑊

maxView𝑊 ∷ 𝑊 a → 𝑂 (ℤ64 ∧ a ∧ (𝑊 a))
maxView𝑊 = map (mapSnd 𝑊) ∘ frhs ∘ IntMap.maxViewWithKey ∘ un𝑊

minKey𝑊 ∷ 𝑊 a → 𝑂 ℤ64
minKey𝑊 ixs = fst ∘ fst ^$ minView𝑊 ixs

maxKey𝑊 ∷ 𝑊 a → 𝑂 ℤ64
maxKey𝑊 ixs = fst ∘ fst ^$ maxView𝑊 ixs

view𝑊 ∷ ℤ64 → 𝑊 a → 𝑂 (a ∧ (𝑊 a))
view𝑊 i ixs
  | i ⋵♮ ixs = Some (ixs ⋕! i :* delete𝑊 i ixs)
  | otherwise = None

without𝑊 ∷ 𝑃 ℤ64 → 𝑊 a → 𝑊 a
without𝑊 is ixs = 𝑊 $ IntMap.withoutKeys (un𝑊 ixs) $ IntSet.fromList $ lazyList $ map tohs $ iter is

restrict𝑊 ∷ 𝑃 ℤ64 → 𝑊 a → 𝑊 a
restrict𝑊 is ixs = 𝑊 $ IntMap.restrictKeys (un𝑊 ixs) $ IntSet.fromList $ lazyList $ map tohs $ iter is

keys𝑊 ∷ 𝑊 a → 𝐼 ℤ64
keys𝑊 = map frhs ∘ iter ∘ IntMap.keys ∘ un𝑊

values𝑊 ∷ 𝑊 a → 𝐼 a
values𝑊 = map frhs ∘ iter ∘ IntMap.elems ∘ un𝑊

map𝑊 ∷ (a → b) → 𝑊 a → 𝑊 b
map𝑊 f = 𝑊 ∘ IntMap.map f ∘ un𝑊

mapK𝑊 ∷ (ℤ64 → a → b) → 𝑊 a → 𝑊 b
mapK𝑊 f ixs = spvec $ mapOn (iter ixs) $ \ (i :* x) → i ↦♮ f i x

stream𝑊 ∷ 𝑊 a → 𝑆 (ℤ64 ∧ a)
stream𝑊 = map frhs ∘ stream ∘ IntMap.toList ∘ un𝑊

spvec𝐼 ∷ 𝐼 (ℤ64 ∧ a) → 𝑊 a
spvec𝐼 = 𝑊 ∘ IntMap.fromList ∘ lazyList ∘ map tohs

spvec ∷ (ToIter (𝑊 a) t) ⇒ t → 𝑊 a
spvec = foldr wø (⩌♮) ∘ iter
 
assoc𝑊 ∷ (ToIter (ℤ64 ∧ a) t) ⇒ t → 𝑊 a
assoc𝑊 = spvec ∘ map single ∘ iter

join𝑊 ∷ (Ord a,Ord b) ⇒ 𝑊 (𝑃 a) → 𝑊 (𝑃 b) → 𝑊 (𝑃 (a ∧ b))
join𝑊 = interWith𝑊 $ \ vs₁ vs₂ → pow $ zipWith (:*) vs₁ vs₂
