module UVMHS.Core.VectorSparse where

import UVMHS.Core.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data
import UVMHS.Core.Chunky

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import qualified Prelude as HS

data 𝑉 a = 𝑉 { un𝑉 ∷ IntMap.IntMap a }
  deriving (Eq,Ord)

instance Lookup ℕ64 a (𝑉 a) where (⋕?) = lookup𝑉
instance Single (ℕ64 ∧ a) (𝑉 a) where single = single𝑉

instance (POrd a) ⇒ POrd (𝑉 a) where (⊑) = subDictBy𝑉 (⊑)

instance Null (𝑉 a) where null = wø
instance (Append a) ⇒ Append (𝑉 a) where (⧺) = unionWith𝑉 (⧺)
instance (Append a) ⇒ Monoid (𝑉 a)

instance (Null a) ⇒ Unit (𝑉 a) where unit = null ↦♮ null
instance (Append a,Cross a) ⇒ Cross (𝑉 a) where
  nxs₁ ⨳ nxs₂ = foldr wø (unionWith𝑉 (⧺)) $ do
    (i₁ :* x₁) ← iter nxs₁
    (i₂ :* x₂) ← iter nxs₂
    return $ (i₁ ⧺ i₂) ↦♮ (x₁ ⨳ x₂)
instance (Prodoid a) ⇒ Prodoid (𝑉 a)

instance Zero (𝑉 a) where zero = wø
instance (Plus a) ⇒ Plus (𝑉 a) where (+) = unionWith𝑉 (+)
instance (Plus a) ⇒ Additive (𝑉 a)

instance (Zero a) ⇒ One (𝑉 a) where one = zero ↦♮ zero
instance (Plus a,Times a) ⇒ Times (𝑉 a) where
  nxs₁ × nxs₂ = fold wø (unionWith𝑉 (+)) $ do
    (i₁ :* x₁) ← iter nxs₁
    (i₂ :* x₂) ← iter nxs₂
    return $ (i₁ + i₂) ↦♮ (x₁ × x₂)
instance (Multiplicative a) ⇒ Multiplicative (𝑉 a)

instance Bot (𝑉 a) where bot = wø
instance (Join a) ⇒ Join (𝑉 a) where (⊔) = unionWith𝑉 (⊔)
instance (Join a) ⇒ JoinLattice (𝑉 a)

instance Top (𝑉 a) where top = wø
instance (Meet a) ⇒ Meet (𝑉 a) where (⊓) = unionWith𝑉 (⊓)
instance (Meet a) ⇒ MeetLattice (𝑉 a)

instance Functor 𝑉 where map = map𝑉

instance ToIter (ℕ64 ∧ a) (𝑉 a) where iter = iter𝑉

instance (Show a) ⇒ Show (𝑉 a) where
  show = tohsChars ∘ showCollection "{" "}" "," (\ (i :* x) → show𝕊 i ⧺ "⇒" ⧺ show𝕊 x)

lookup𝑉 ∷ 𝑉 a → ℕ64 → 𝑂 a
lookup𝑉 nxs n = frhs $ un𝑉 nxs IntMap.!? frBitsHSInt n

single𝑉 ∷ ℕ64 ∧ a → 𝑉 a
single𝑉 (n :* x) = 𝑉 $ IntMap.singleton (frBitsHSInt n) x

subDictBy𝑉 ∷ (a → a → 𝔹) → 𝑉 a → 𝑉 a → 𝔹
subDictBy𝑉 f nxs₁ nxs₂ = IntMap.isSubmapOfBy f (un𝑉 nxs₁) (un𝑉 nxs₂)

wø ∷ 𝑉 a
wø = 𝑉 IntMap.empty

unionWith𝑉 ∷ (a → a → a) → 𝑉 a → 𝑉 a → 𝑉 a
unionWith𝑉 f nxs₁ nxs₂ = 𝑉 $ IntMap.unionWith f (un𝑉 nxs₁) (un𝑉 nxs₂)

(↦♮) ∷ ℕ64 → a → 𝑉 a
i ↦♮ x = 𝑉 $ IntMap.singleton (frBitsHSInt i) x

(⋵♮) ∷ ℕ64 → 𝑉 a → 𝔹
i ⋵♮ nxs = frBitsHSInt i `IntMap.member` un𝑉 nxs

(⩌♮) ∷ 𝑉 a → 𝑉 a → 𝑉 a
nxs₁ ⩌♮ nxs₂ = 𝑉 $ un𝑉 nxs₁ `IntMap.union` un𝑉 nxs₂

(⩍♮) ∷ 𝑉 a → 𝑉 a → 𝑉 a
nxs₁ ⩍♮ nxs₂ = 𝑉 $ un𝑉 nxs₁ `IntMap.intersection` un𝑉 nxs₂

-- (∸♮) ∷ 𝑉 a → 𝑉 a → 𝑉 a
-- nxs₁ ∸♮ nxs₂ = 𝑉 $ un𝑉 nxs₁ `IntMap.difference` un𝑉 nxs₂

delete𝑉 ∷ ℕ64 → 𝑉 a → 𝑉 a
delete𝑉 i nxs = 𝑉 $ IntMap.delete (frBitsHSInt i) $ un𝑉 nxs

size𝑉 ∷ 𝑉 a → ℕ
size𝑉 = HS.fromIntegral ∘ IntMap.size ∘ un𝑉

(⊎♮) ∷ (Additive a) ⇒ 𝑉 a → 𝑉 a → 𝑉 a
(⊎♮) = unionWith𝑉 (+)

unionsWith𝑉 ∷ (ToIter (𝑉 a) t) ⇒ (a → a → a) → t → 𝑉 a
unionsWith𝑉 = fold wø ∘ unionWith𝑉

interWith𝑉 ∷ (a → b → c) → 𝑉 a → 𝑉 b → 𝑉 c
interWith𝑉 f nxs₁ nxs₂ = 𝑉 $ IntMap.intersectionWith f (un𝑉 nxs₁) (un𝑉 nxs₂)

-- -- diffWith ∷ (v → v → v) → 𝑉 a → 𝑉 a → 𝑉 a
-- -- diffWith f nxs₁ nxs₂ = 𝑉 $ IntMap.differenceWith (\ x y → HS.Just (f x y)) (un𝑉 nxs₁) (un𝑉 nxs₂)

minView𝑉 ∷ 𝑉 a → 𝑂 (ℕ64 ∧ a ∧ 𝑉 a)
minView𝑉 nxs = case IntMap.minViewWithKey $ un𝑉 nxs of
  HS.Nothing → None
  HS.Just ((i,x),nxs') → Some $ toBitsHSInt i :* x :* 𝑉 nxs'

maxView𝑉 ∷ 𝑉 a → 𝑂 (ℕ64 ∧ a ∧ (𝑉 a))
maxView𝑉 nxs = case IntMap.maxViewWithKey $ un𝑉 nxs of
  HS.Nothing → None
  HS.Just ((i,x),nxs') → Some $ toBitsHSInt i :* x :* 𝑉 nxs'

minKey𝑉 ∷ 𝑉 a → 𝑂 ℕ64
minKey𝑉 nxs = fst ∘ fst ^$ minView𝑉 nxs

maxKey𝑉 ∷ 𝑉 a → 𝑂 ℕ64
maxKey𝑉 nxs = fst ∘ fst ^$ maxView𝑉 nxs

view𝑉 ∷ ℕ64 → 𝑉 a → 𝑂 (a ∧ 𝑉 a)
view𝑉 n nxs
  | n ⋵♮ nxs = Some $ (nxs ⋕! n) :* delete𝑉 n nxs
  | otherwise = None

without𝑉 ∷ 𝑃 ℕ64 → 𝑉 a → 𝑉 a
without𝑉 is nxs = 𝑉 $ IntMap.withoutKeys (un𝑉 nxs) $ IntSet.fromList $ lazyList $ map frBitsHSInt $ iter is

restrict𝑉 ∷ 𝑃 ℕ64 → 𝑉 a → 𝑉 a
restrict𝑉 is nxs = 𝑉 $ IntMap.restrictKeys (un𝑉 nxs) $ IntSet.fromList $ lazyList $ map frBitsHSInt $ iter is

keys𝑉 ∷ 𝑉 a → 𝐼 ℕ64
keys𝑉 = map toBitsHSInt ∘ iter ∘ IntMap.keys ∘ un𝑉

values𝑉 ∷ 𝑉 a → 𝐼 a
values𝑉 = map frhs ∘ iter ∘ IntMap.elems ∘ un𝑉

map𝑉 ∷ (a → b) → 𝑉 a → 𝑉 b
map𝑉 f = 𝑉 ∘ IntMap.map f ∘ un𝑉

mapK𝑉 ∷ (ℕ64 → a → b) → 𝑉 a → 𝑉 b
mapK𝑉 f nxs = spvec $ mapOn (iter nxs) $ \ (i :* x) → i ↦♮ f i x

iter𝑉 ∷ 𝑉 a → 𝐼 (ℕ64 ∧ a)
iter𝑉 = map (\ (i,x) → toBitsHSInt i :* x)∘ iterLL ∘ IntMap.toList ∘ un𝑉

spvec𝐼 ∷ 𝐼 (ℕ64 ∧ a) → 𝑉 a
spvec𝐼 = 𝑉 ∘ IntMap.fromList ∘ lazyList ∘ map (\ (i :* x) → (frBitsHSInt i,x))

spvec ∷ (ToIter (𝑉 a) t) ⇒ t → 𝑉 a
spvec = foldr wø (⩌♮) ∘ iter

assoc𝑉 ∷ (ToIter (ℕ64 ∧ a) t) ⇒ t → 𝑉 a
assoc𝑉 = spvec ∘ map single ∘ iter

join𝑉 ∷ (Ord a,Ord b) ⇒ 𝑉 (𝑃 a) → 𝑉 (𝑃 b) → 𝑉 (𝑃 (a ∧ b))
join𝑉 = interWith𝑉 $ \ vs₁ vs₂ → pow𝐼𝑃 $ iter $ zipWith (:*) vs₁ vs₂
