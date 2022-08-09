module UVMHS.Core.VectorStatic
  ( module UVMHS.Core.VectorStatic
  , module Foreign.Storable
  ) where

import UVMHS.Core.Init

import UVMHS.Core.Classes
import UVMHS.Core.Data

import UVMHS.Core.Static

import UVMHS.Core.Vector

import Foreign.Storable (Storable(..))

import qualified Prelude as HS
import qualified Data.Vector as VB
import qualified Data.Vector.Storable  as VU

infixl 6 ⋅,✖

newtype 𝕍SV n a = 𝕍SV { un𝕍SV ∷ 𝕀64 n → a }

instance Functor (𝕍SV n) where map f xs = 𝕍SV $ f ∘ un𝕍SV xs
instance Access (𝕀64 n) a (𝕍SV n a) where (⋕) = un𝕍SV

--------
-- 𝕍S --
--------

newtype 𝕍S n a = 𝕍S_UNSAFE { un𝕍S ∷ VB.Vector a }
  deriving (Eq,Ord)

instance ToIter a (𝕍S n a)             where iter   = iter𝕍S
instance (Show a) ⇒ Show (𝕍S n a)      where show   = tohsChars ∘ show𝕍S
instance Access (𝕀64 n) a (𝕍S n a)     where (⋕)    = flip idx𝕍S
instance Lookup ℕ64 a (𝕍S n a)         where (⋕?)   = flip idxChecked𝕍S
instance (𝒩 n,Null a) ⇒ Null (𝕍S n a)  where null   = null𝕍S 𝕟64s
instance (𝒩 n) ⇒ Functor (𝕍S n)        where map    = map𝕍S

instance (𝒩 n,Zero a)  ⇒ Zero  (𝕍S n a) where zero     = const𝕍S 𝕟64s zero
instance (𝒩 n,One a)   ⇒ One   (𝕍S n a) where one      = const𝕍S 𝕟64s one
instance (𝒩 n,Plus a)  ⇒ Plus  (𝕍S n a) where xs + ys  = svecF 𝕟64s $ \ i → xs ⋕ i + ys ⋕ i
instance (𝒩 n,Times a) ⇒ Times (𝕍S n a) where xs × ys  = svecF 𝕟64s $ \ i → xs ⋕ i × ys ⋕ i

-- instance DotProduct U 𝕍S where

svec ∷ ∀ n a. (𝒩 n) ⇒ 𝐼S n a → 𝕍S n a
svec xs = 𝕍S_UNSAFE $ VB.fromListN (tohs $ intΩ64 $ unℕ64S $ 𝕟64s @n) $ lazyList $ un𝐼S xs

svecF ∷ ∀ n a. (𝒩 n) ⇒ ℕ64S n → (𝕀64 n → a) → 𝕍S n a
svecF n f = svec $ map f $ upto𝕀64 n

idx𝕍S ∷ 𝕀64 n → 𝕍S n a → a
idx𝕍S i xs = VB.unsafeIndex (un𝕍S xs) $ tohs $ intΩ64 $ un𝕀64 i

idxChecked𝕍S ∷ ℕ64 → 𝕍S n a → 𝑂 a
idxChecked𝕍S i xs = frhs $ un𝕍S xs VB.!? tohs (intΩ64 i)

iter𝕍SS ∷ 𝕍S n a → 𝐼S n a
iter𝕍SS xs = 𝐼S_UNSAFE $ iter𝕍S xs

iter𝕍S ∷ 𝕍S n a → 𝐼 a
iter𝕍S xs = iterLL $ VB.toList $ un𝕍S xs

show𝕍S ∷ (Show a) ⇒ 𝕍S n a → 𝕊
show𝕍S = showCollection "𝕍S[" "]" "," show𝕊 ∘ iter

null𝕍S ∷ (𝒩 n,Null a) ⇒ ℕ64S n → 𝕍S n a
null𝕍S n = svecF n $ const null

map𝕍S ∷ (𝒩 n) ⇒ (a → b) → 𝕍S n a → 𝕍S n b
map𝕍S f = svec ∘ map f ∘ iter𝕍SS

const𝕍S ∷ (𝒩 n) ⇒ ℕ64S n → a → 𝕍S n a
const𝕍S n x = svecF n $ const x

svirt ∷ (𝒩 n) ⇒ 𝕍S n a → 𝕍SV n a
svirt xs = 𝕍SV $ \ i → xs ⋕ i

svirt2 ∷ (𝒩 m,𝒩 n) ⇒ 𝕍S m (𝕍S n a) → 𝕍SV m (𝕍SV n a)
svirt2 = map svirt ∘ svirt

sconc ∷ (𝒩 n) ⇒ ℕ64S n → 𝕍SV n a → 𝕍S n a
sconc n xs = svecF n $ un𝕍SV xs

sconc2 ∷ (𝒩 m,𝒩 n) ⇒ ℕ64S m → ℕ64S n → 𝕍SV m (𝕍SV n a) → 𝕍S m (𝕍S n a)
sconc2 m n = sconc m ∘ map (sconc n)

𝐭 ∷ (𝒩 m,𝒩 n) ⇒ 𝕍S m (𝕍S n a) → 𝕍S n (𝕍S m a)
𝐭 xs = svecF 𝕟64s $ \ j → svecF 𝕟64s $ \ i → xs ⋕ i ⋕ j

(⋅) ∷ (𝒩 n,Additive a,Times a) ⇒ 𝕍S n a → 𝕍S n a → a
xs ⋅ ys = sum $ map (\ i → xs ⋕ i × ys ⋕ i) $ upto𝕀64 $ 𝕟64s

(✖) ∷ (𝒩 m,𝒩 n,𝒩 o,Additive a,Times a) ⇒ 𝕍S m (𝕍S o a) → 𝕍S n (𝕍S o a) → 𝕍S m (𝕍S n a)
xs ✖ ys = svecF 𝕟64s $ \ i → svecF 𝕟64s $ \ j → xs ⋕ i ⋅ ys ⋕ j

d𝕍 ∷ 𝕍 a → (∀ n. (𝒩64 n) ⇒ 𝕍S n a → b) → b
d𝕍 xs f = 𝕟64d (natΩ64 $ frhs $ VB.length $ un𝕍 xs) HS.$ \ (_ ∷ ℕ64S n) → f @n $ 𝕍S_UNSAFE $ un𝕍 xs

--------
-- 𝕌S --
--------

newtype 𝕌S n a = 𝕌S_UNSAFE { un𝕌S ∷ VU.Vector a }
  deriving (Eq,Ord)

instance (Storable a) ⇒ ToIter a (𝕌S n a)         where iter   = iter𝕌S
instance (Storable a,Show a) ⇒ Show (𝕌S n a)      where show   = tohsChars ∘ show𝕌S
instance (Storable a) ⇒ Access (𝕀64 n) a (𝕌S n a) where (⋕)    = flip idx𝕌S
instance (Storable a) ⇒ Lookup ℕ64 a (𝕌S n a)     where (⋕?)   = flip idxChecked𝕌S
instance (𝒩 n,Storable a,Null a) ⇒ Null (𝕌S n a)  where null   = null𝕌S 𝕟64s

suvec ∷ ∀ n a. (𝒩 n,Storable a) ⇒ 𝐼S n a → 𝕌S n a
suvec xs = 𝕌S_UNSAFE $ VU.fromListN (tohs $ intΩ64 $ unℕ64S $ 𝕟64s @n) $ lazyList $ un𝐼S xs

suvecF ∷ ∀ n a. (𝒩 n,Storable a) ⇒ ℕ64S n → (𝕀64 n → a) → 𝕌S n a
suvecF n f = suvec $ map f $ upto𝕀64 n

idx𝕌S ∷ (Storable a) ⇒ 𝕀64 n → 𝕌S n a → a
idx𝕌S i xs = VU.unsafeIndex (un𝕌S xs) $ tohs $ intΩ64 $ un𝕀64 i

idxChecked𝕌S ∷ (Storable a) ⇒ ℕ64 → 𝕌S n a → 𝑂 a
idxChecked𝕌S i xs = frhs $ un𝕌S xs VU.!? tohs (intΩ64 i)

iter𝕌SS ∷ (Storable a) ⇒ 𝕌S n a → 𝐼S n a
iter𝕌SS xs = 𝐼S_UNSAFE $ iter $ iter𝕌S xs

iter𝕌S ∷ (Storable a) ⇒ 𝕌S n a → 𝐼 a
iter𝕌S xs = iterLL $ VU.toList $ un𝕌S xs

show𝕌S ∷ (Storable a,Show a) ⇒ 𝕌S n a → 𝕊
show𝕌S = showCollection "𝕌S[" "]" "," show𝕊 ∘ iter

null𝕌S ∷ (𝒩 n,Storable a,Null a) ⇒ ℕ64S n → 𝕌S n a
null𝕌S n = suvecF n $ const null

map𝕌S ∷ (𝒩 n,Storable a,Storable b) ⇒ (a → b) → 𝕌S n a → 𝕌S n b
map𝕌S f = suvec ∘ map f ∘ iter𝕌SS

d𝕌 ∷ (Storable a) ⇒ 𝕌 a → (∀ n. (𝒩64 n) ⇒ 𝕌S n a → b) → b
d𝕌 xs f = 𝕟64d (natΩ64 $ frhs $ VU.length $ un𝕌 xs) HS.$ \ (_ ∷ ℕ64S n) → f @n $ 𝕌S_UNSAFE $ un𝕌 xs

--------
-- 𝕄S --
--------

data 𝕄S (ns ∷ [𝐍]) a where
  Nil𝕄S ∷ a → 𝕄S '[] a
  Cons𝕄S ∷ 𝕍S n (𝕄S ns a) → 𝕄S (n ': ns) a

zero𝕄S ∷ (AllC 𝒩 ns,Zero a) ⇒ Spine ns → 𝕄S ns a
zero𝕄S = \case
  NilSpine → zero
  ConsSpine sp → Cons𝕄S $ const𝕍S 𝕟64s $ zero𝕄S sp

instance (HasSpine ns,AllC 𝒩 ns,Zero a) ⇒ Zero (𝕄S ns a) where
  zero = zero𝕄S spine

instance (AllC 𝒩 ns,Plus a) ⇒ Plus (𝕄S ns a) where
  Nil𝕄S x + Nil𝕄S y = Nil𝕄S $ x + y
  Cons𝕄S xs + Cons𝕄S ys = Cons𝕄S $ xs + ys

instance (AllC 𝒩 ns,Times a) ⇒ Times (𝕄S ns a) where
  Nil𝕄S x × Nil𝕄S y = Nil𝕄S $ x × y
  Cons𝕄S xs × Cons𝕄S ys = Cons𝕄S $ xs × ys

mapUnder𝕄S ∷ (AllC 𝒩 ns) ⇒ Spine ns → (𝕄S ms₁ a → 𝕄S ms₂ b) → 𝕄S (ns ⧺ ms₁) a → 𝕄S (ns ⧺ ms₂) b
mapUnder𝕄S sp f xs = case sp of
  NilSpine → f xs
  ConsSpine sp' → case xs of
    Cons𝕄S (xs' ∷ 𝕍S n (𝕄S ns a)) → Cons𝕄S $ map (mapUnder𝕄S sp' f) xs'

