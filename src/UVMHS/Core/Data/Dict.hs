module UVMHS.Core.Data.Dict where

import UVMHS.Core.Init
import UVMHS.Core.Classes

import UVMHS.Core.Data.Iter
import UVMHS.Core.Data.Pair
import UVMHS.Core.Data.String
import UVMHS.Core.Data.Set

import qualified Data.Map.Strict as Map
import qualified Prelude as HS

infixr 1 ↦

instance (Ord k) ⇒ Lookup k v (k ⇰ v) where (⋕?) = lookup𝐷
instance (Ord k) ⇒ Single (k ∧ v) (k ⇰ v) where single = single𝐷

instance (Ord k,POrd v) ⇒ POrd (k ⇰ v) where (⊑) = subDictBy (⊑)

instance Null (k ⇰ v) where null = dø
instance (Ord k,Append v) ⇒ Append (k ⇰ v) where (⧺) = unionWith (⧺)
instance (Ord k,Append v) ⇒ Monoid (k ⇰ v) 

instance (Ord k,Null k,Null v) ⇒ Unit (k ⇰ v) where unit = null ↦ null
instance (Ord k,Append k,Append v,Cross v) ⇒ Cross (k ⇰ v) where
  kvs₁ ⨳ kvs₂ = foldr dø (unionWith (⧺)) $ do
    (k₁ :* v₁) ← iter kvs₁
    (k₂ :* v₂) ← iter kvs₂
    return $ (k₁ ⧺ k₂) ↦ (v₁ ⨳ v₂)
instance (Ord k,Monoid k,Prodoid v) ⇒ Prodoid (k ⇰ v)

instance Zero (k ⇰ v) where zero = dø
instance (Ord k,Plus v) ⇒ Plus (k ⇰ v) where (+) = unionWith (+)
instance (Ord k,Plus v) ⇒ Additive (k ⇰ v)

instance (Ord k,Zero k,Zero v) ⇒ One (k ⇰ v) where one = zero ↦ zero
instance (Ord k,Plus k,Plus v,Times v) ⇒ Times (k ⇰ v) where
  kvs₁ × kvs₂ = fold dø (unionWith (+)) $ do
    (k₁ :* v₁) ← iter kvs₁
    (k₂ :* v₂) ← iter kvs₂
    return $ (k₁ + k₂) ↦ (v₁ × v₂)
instance (Ord k,Additive k,Multiplicative v) ⇒ Multiplicative (k ⇰ v)

instance Bot (k ⇰ v) where bot = dø
instance (Ord k,Join v) ⇒ Join (k ⇰ v) where (⊔) = unionWith (⊔)
instance (Ord k,Join v) ⇒ JoinLattice (k ⇰ v)

instance Top (k ⇰ v) where top = dø
instance (Ord k,Meet v) ⇒ Meet (k ⇰ v) where (⊓) = unionWith (⊓)
instance (Ord k,Meet v) ⇒ MeetLattice (k ⇰ v)

instance (Ord k,Difference v) ⇒ Difference (k ⇰ v) where (⊟) = diffWith (⊟)


instance Functor ((⇰) k) where map = map𝐷
instance FunctorM ((⇰) k) where mapM = mapM𝐷

instance ToIter (k ∧ v) (k ⇰ v) where iter = iter𝐷

instance (Ord k,All k,All v) ⇒ All (k ⇰ v) where
  all ∷ 𝐼 (k ⇰ v)
  all = foldrOnFrom all (return dø) $ \ k kvssᵢ → do
    kvs ← map (k ↦) all
    kvsᵢ ← kvssᵢ
    return $ kvs ⩌ kvsᵢ

instance (Show k,Show v) ⇒ Show (k ⇰ v) where 
  show = tohsChars ∘ showCollection "{" "}" "," (\ (k :* v) → show𝕊 k ⧺ "⇒" ⧺ show𝕊 v)

lookup𝐷 ∷ (Ord k) ⇒ k ⇰ v → k → 𝑂 v
lookup𝐷 kvs k = frhs $ un𝐷 kvs Map.!? k

single𝐷 ∷ k ∧ v → k ⇰ v
single𝐷 (k :* v) = 𝐷 $ Map.singleton k v

dø ∷ k ⇰ v
dø = 𝐷 Map.empty
  
(↦) ∷ (Ord k) ⇒ k → v → k ⇰ v
k ↦ v = 𝐷 $ Map.singleton k v

(⋵) ∷ (Ord k) ⇒ k → k ⇰ v → 𝔹
k ⋵ kvs = k `Map.member` un𝐷 kvs

(⩌) ∷ (Ord k) ⇒ k ⇰ v → k ⇰ v → k ⇰ v
kvs₁ ⩌ kvs₂ = 𝐷 $ un𝐷 kvs₁ `Map.union` un𝐷 kvs₂

(⩍) ∷ (Ord k) ⇒ k ⇰ v → k ⇰ v → k ⇰ v
kvs₁ ⩍ kvs₂ = 𝐷 $ un𝐷 kvs₁ `Map.intersection` un𝐷 kvs₂

-- (∸) ∷ (Ord k) ⇒ k ⇰ v → k ⇰ v → k ⇰ v
-- kvs₁ ∸ kvs₂ = 𝐷 $ un𝐷 kvs₁ `Map.difference` un𝐷 kvs₂

delete ∷ (Ord k) ⇒ k → k ⇰ v → k ⇰ v
delete k kvs = 𝐷 $ Map.delete k $ un𝐷 kvs

dsize ∷ (Ord k) ⇒ k ⇰ v → ℕ
dsize = HS.fromIntegral ∘ Map.size ∘ un𝐷

subDictBy ∷ (Ord k) ⇒ (v → v → 𝔹) → k ⇰ v → k ⇰ v → 𝔹
subDictBy f kvs₁ kvs₂ = Map.isSubmapOfBy f (un𝐷 kvs₁) (un𝐷 kvs₂)

unionWithG ∷ (Ord k) ⇒ (a → c) → (b → c) → (a → b → c) → k ⇰ a → k ⇰ b → k ⇰ c
unionWithG f₁ f₂ f₃ kvs₁ kvs₂ = assoc $ mapOn (iter $ keys kvs₁ ∪ keys kvs₂) $ \ k → (:*) k $
  case (kvs₁ ⋕? k,kvs₂ ⋕? k) of
    (Some v₁,None) → f₁ v₁
    (None,Some v₂) → f₂ v₂
    (Some v₁,Some v₂) → f₃ v₁ v₂
    _ → error "impossible"

unionWithGM ∷ (Ord k,Monad m) ⇒ (a → m c) → (b → m c) → (a → b → m c) → k ⇰ a → k ⇰ b → m (k ⇰ c)
unionWithGM f₁ f₂ f₃ kvs₁ kvs₂ = assoc ^$ mapMOn (iter $ keys kvs₁ ∪ keys kvs₂) $ \ k → (:*) k ^$
  case (kvs₁ ⋕? k,kvs₂ ⋕? k) of
    (Some v₁,None) → f₁ v₁
    (None,Some v₂) → f₂ v₂
    (Some v₁,Some v₂) → f₃ v₁ v₂
    _ → error "impossible"

unionWithKeyG ∷ (Ord k) ⇒ (k → a → c) → (k → b → c) → (k → a → b → c) → k ⇰ a → k ⇰ b → k ⇰ c
unionWithKeyG f₁ f₂ f₃ kvs₁ kvs₂ = assoc $ mapOn (iter $ keys kvs₁ ∪ keys kvs₂) $ \ k → (:*) k $
  case (kvs₁ ⋕? k,kvs₂ ⋕? k) of
    (Some v₁,None) → f₁ k v₁
    (None,Some v₂) → f₂ k v₂
    (Some v₁,Some v₂) → f₃ k v₁ v₂
    _ → error "impossible"

unionWithKeyGOn ∷ (Ord k) ⇒ k ⇰ a → k ⇰ b → (k → a → c) → (k → b → c) → (k → a → b → c) → k ⇰ c
unionWithKeyGOn kvs₁ kvs₂ f₁ f₂ f₃ = unionWithKeyG f₁ f₂ f₃ kvs₁ kvs₂

unionWithKeyGM ∷ (Ord k,Monad m) ⇒ (k → a → m c) → (k → b → m c) → (k → a → b → m c) → k ⇰ a → k ⇰ b → m (k ⇰ c)
unionWithKeyGM f₁ f₂ f₃ kvs₁ kvs₂ = assoc ^$ mapMOn (iter $ keys kvs₁ ∪ keys kvs₂) $ \ k → (:*) k ^$
  case (kvs₁ ⋕? k,kvs₂ ⋕? k) of
    (Some v₁,None) → f₁ k v₁
    (None,Some v₂) → f₂ k v₂
    (Some v₁,Some v₂) → f₃ k v₁ v₂
    _ → error "impossible"

unionWith ∷ (Ord k) ⇒ (v → v → v) → k ⇰ v → k ⇰ v → k ⇰ v
unionWith f kvs₁ kvs₂ = 𝐷 $ Map.unionWith f (un𝐷 kvs₁) (un𝐷 kvs₂)

unionWithKey ∷ (Ord k) ⇒ (k → v → v → v) → k ⇰ v → k ⇰ v → k ⇰ v
unionWithKey f kvs₁ kvs₂ = 𝐷 $ Map.unionWithKey f (un𝐷 kvs₁) (un𝐷 kvs₂)

unionWithD ∷ (Ord k) ⇒ v → (v → v → v) → k ⇰ v → k ⇰ v → k ⇰ v
unionWithD d f = unionWithG (\ x → f x d) (\ y → f d y) f

(⊎) ∷ (Ord k,Additive v) ⇒ k ⇰ v → k ⇰ v → k ⇰ v
(⊎) = unionWith (+)

unionsWith ∷ (Ord k,ToIter (k ⇰ v) t) ⇒ (v → v → v) → t → k ⇰ v
unionsWith = fold dø ∘ unionWith

unionWithOn ∷ (Ord k) ⇒ k ⇰ v → k ⇰ v → (v → v → v) → k ⇰ v
unionWithOn = rotateL unionWith

unionWithKeyOn ∷ (Ord k) ⇒ k ⇰ v → k ⇰ v → (k → v → v → v) → k ⇰ v
unionWithKeyOn = rotateL unionWithKey

interWith ∷ (Ord k) ⇒ (v₁ → v₂ → v₃) → k ⇰ v₁ → k ⇰ v₂ → k ⇰ v₃
interWith f kvs₁ kvs₂ = 𝐷 $ Map.intersectionWith f (un𝐷 kvs₁) (un𝐷 kvs₂)

interWithOn ∷ (Ord k) ⇒ k ⇰ v₁ → k ⇰ v₂ → (v₁ → v₂ → v₃) → k ⇰ v₃
interWithOn = rotateL interWith

interWithM ∷ (Monad m,Ord k) ⇒ (v₁ → v₂ → m v₃) → k ⇰ v₁ → k ⇰ v₂ → m (k ⇰ v₃)
interWithM f kvs₁ kvs₂ = exchange $ interWith f kvs₁ kvs₂

interWithMOn ∷ (Monad m,Ord k) ⇒ k ⇰ v₁ → k ⇰ v₂ → (v₁ → v₂ → m v₃) → m (k ⇰ v₃)
interWithMOn = rotateL interWithM

diffWith ∷ (Ord k) ⇒ (v → v → v) → k ⇰ v → k ⇰ v → k ⇰ v
diffWith f kvs₁ kvs₂ = 𝐷 $ Map.differenceWith (\ x y → HS.Just $ f x y) (un𝐷 kvs₁) $ un𝐷 kvs₂

dminView ∷ k ⇰ v → 𝑂 (k ∧ v ∧ (k ⇰ v))
dminView = map (mapSnd 𝐷) ∘ frhs ∘ Map.minViewWithKey ∘ un𝐷

dmaxView ∷ k ⇰ v → 𝑂 (k ∧ v ∧ (k ⇰ v))
dmaxView = map (mapSnd 𝐷) ∘ frhs ∘ Map.maxViewWithKey ∘ un𝐷

dminKey ∷ k ⇰ v → 𝑂 k
dminKey kvs = fst ∘ fst ^$ dminView kvs

dmaxKey ∷ k ⇰ v → 𝑂 k
dmaxKey kvs = fst ∘ fst ^$ dmaxView kvs

dview ∷ (Ord k) ⇒ k → k ⇰ v → 𝑂 (v ∧ (k ⇰ v))
dview k kvs
  | k ⋵ kvs = Some $ (kvs ⋕! k) :* delete k kvs
  | otherwise = None

without ∷ (Ord k) ⇒ 𝑃 k → k ⇰ v → k ⇰ v
without ks kvs = 𝐷 $ Map.withoutKeys (un𝐷 kvs) $ un𝑃 ks

restrict ∷ (Ord k) ⇒ 𝑃 k → k ⇰ v → k ⇰ v
restrict ks kvs = 𝐷 $ Map.restrictKeys (un𝐷 kvs) $ un𝑃 ks

mapWithKey ∷ (k → v → v') → k ⇰ v → k ⇰ v'
mapWithKey f = 𝐷 ∘ Map.mapWithKey f ∘ un𝐷

mapWithKeyOn ∷ k ⇰ v → (k → v → v') → k ⇰ v'
mapWithKeyOn = flip mapWithKey

mapMWithKey ∷ (Monad m) ⇒ (k → v → m v') → k ⇰ v → m (k ⇰ v')
mapMWithKey = exchange ∘∘ mapWithKey

mapMWithKeyOn ∷ (Monad m) ⇒ k ⇰ v → (k → v → m v') → m (k ⇰ v')
mapMWithKeyOn = flip mapMWithKey

keys ∷ (Ord k) ⇒ k ⇰ v → 𝑃 k
keys = pow ∘ Map.keys ∘ un𝐷

values ∷ k ⇰ v → 𝐼 v
values = iter ∘ Map.elems ∘ un𝐷

map𝐷 ∷ (v₁ → v₂) → k ⇰ v₁ → k ⇰ v₂
map𝐷 f = 𝐷 ∘ Map.map f ∘ un𝐷

mapM𝐷 ∷ ∀ m k v₁ v₂. (Monad m) ⇒ (v₁ → m v₂) → k ⇰ v₁ → m (k ⇰ v₂)
mapM𝐷 f kvs = with (tohsMonad @m) HS.$
  𝐷 ^$ HS.mapM f $ un𝐷 kvs

mapK𝐷 ∷ (Ord k) ⇒ (k → v₁ → v₂) → k ⇰ v₁ → k ⇰ v₂
mapK𝐷 f kvs = dict $ mapOn (iter kvs) $ \ (k :* v) → k ↦ f k v

iter𝐷 ∷ k ⇰ v → 𝐼 (k ∧ v)
iter𝐷 = map frhs ∘ iterLL ∘ Map.toList ∘ un𝐷

dict𝐼 ∷ (Ord k) ⇒ 𝐼 (k ∧ v) → k ⇰ v
dict𝐼 = 𝐷 ∘ Map.fromList ∘ lazyList ∘ map tohs

dict ∷ (Ord k,ToIter (k ⇰ v) t) ⇒ t → k ⇰ v
dict = foldr dø (⩌) ∘ iter

assoc ∷ (Ord k,ToIter (k ∧ v) t) ⇒ t → k ⇰ v
assoc = dict ∘ map single ∘ iter

djoin ∷ (Ord k,Ord v₁,Ord v₂) ⇒ k ⇰ 𝑃 v₁ → k ⇰ 𝑃 v₂ → k ⇰ 𝑃 (v₁ ∧ v₂)
djoin = interWith $ \ vs₁ vs₂ → pow $ zipWith (:*) vs₁ vs₂

mapOnKeyWith ∷ (Ord k) ⇒ (v → v) → k → k ⇰ v → k ⇰ v
mapOnKeyWith f k = 𝐷 ∘ Map.adjust f k ∘ un𝐷

mapOnKey ∷ (Ord k) ⇒ k → (v → v) → k ⇰ v → k ⇰ v
mapOnKey = flip mapOnKeyWith
