module UVMHS.Core.Vector 
  ( module UVMHS.Core.Vector
  , module Foreign.Storable
  ) where

import UVMHS.Core.Init

import UVMHS.Core.Classes
import UVMHS.Core.Data

import UVMHS.Core.Static

import Foreign.Storable (Storable(..))

import qualified Data.Vector           as VB
import qualified Data.Vector.Storable  as VU

infixl 6 ⋅,✖

-- class DotProduct c t | t → c where (⋅) ∷ (𝒩 n,Additive a,Times a,c a) ⇒ t n a → t n a → a
-- class VectorS c t | t → c where vector ∷ (c a) ⇒ 𝐼S n a → t n a
-- 
-- vectorF ∷ (𝒩 n,VectorS c t,c a) ⇒ ℕ64S n → (𝕀64 n → a) → t n a
-- vectorF n f = vector $ map f $ upTo𝕀64 n

newtype 𝕍SV n a = 𝕍SV { un𝕍SV ∷ 𝕀64 n → a }

instance Functor (𝕍SV n) where map f xs = 𝕍SV $ f ∘ un𝕍SV xs
instance Access (𝕀64 n) a (𝕍SV n a) where (⋕) = un𝕍SV

--------
-- 𝕍S --
--------

newtype 𝕍S n a = 𝕍S_UNSAFE { un𝕍S ∷ VB.Vector a }
  deriving (Eq,Ord)

instance ToStream a (𝕍S n a)           where {-# INLINE stream #-} ; stream = stream𝕍S
instance ToIter a (𝕍S n a)             where {-# INLINE iter   #-} ; iter   = iter ∘ stream
instance (Show a) ⇒ Show (𝕍S n a)      where {-# INLINE show   #-} ; show   = chars ∘ show𝕍S
instance Access (𝕀64 n) a (𝕍S n a)     where {-# INLINE (⋕)    #-} ; (⋕)    = flip idx𝕍S
instance Lookup ℕ64 a (𝕍S n a)         where {-# INLINE (⋕?)   #-} ; (⋕?)   = flip idxChecked𝕍S
instance (𝒩 n,Null a) ⇒ Null (𝕍S n a)  where {-# INLINE null   #-} ; null   = null𝕍S 𝕟64s
instance (𝒩 n) ⇒ Functor (𝕍S n)        where {-# INLINE map    #-} ; map    = map𝕍S

instance (𝒩 n,Zero a)  ⇒ Zero  (𝕍S n a) where zero     = const𝕍S 𝕟64s zero
instance (𝒩 n,One a)   ⇒ One   (𝕍S n a) where one      = const𝕍S 𝕟64s one
instance (𝒩 n,Plus a)  ⇒ Plus  (𝕍S n a) where xs + ys  = svecF 𝕟64s $ \ i → xs ⋕ i + ys ⋕ i
instance (𝒩 n,Times a) ⇒ Times (𝕍S n a) where xs × ys  = svecF 𝕟64s $ \ i → xs ⋕ i × ys ⋕ i

-- instance DotProduct U 𝕍S where

{-# INLINE svec #-}
svec ∷ ∀ n a. (𝒩 n) ⇒ 𝐼S n a → 𝕍S n a
svec xs = 𝕍S_UNSAFE $ VB.fromListN (tohs $ intΩ64 $ unℕ64S $ 𝕟64s @ n) $ lazyList $ un𝐼S xs

{-# INLINE svecF #-}
svecF ∷ ∀ n a. (𝒩 n) ⇒ ℕ64S n → (𝕀64 n → a) → 𝕍S n a
svecF n f = svec $ map f $ upTo𝕀64 n

{-# INLINE idx𝕍S #-}
idx𝕍S ∷ 𝕀64 n → 𝕍S n a → a
idx𝕍S i xs = VB.unsafeIndex (un𝕍S xs) $ tohs $ intΩ64 $ un𝕀64 i

{-# INLINE idxChecked𝕍S #-}
idxChecked𝕍S ∷ ℕ64 → 𝕍S n a → 𝑂 a
idxChecked𝕍S i xs = frhs $ un𝕍S xs VB.!? tohs (intΩ64 i)

{-# INLINE iter𝕍S #-}
iter𝕍S ∷ 𝕍S n a → 𝐼S n a
iter𝕍S xs = 𝐼S_UNSAFE $ iter $ stream𝕍S xs

{-# INLINE stream𝕍S #-}
stream𝕍S ∷ 𝕍S n a → 𝑆 a
stream𝕍S xs = stream $ VB.toList $ un𝕍S xs

{-# INLINE show𝕍S #-}
show𝕍S ∷ (Show a) ⇒ 𝕍S n a → 𝕊
show𝕍S = showCollection "𝕍S[" "]" "," show𝕊 ∘ iter

{-# INLINE null𝕍S #-}
null𝕍S ∷ (𝒩 n,Null a) ⇒ ℕ64S n → 𝕍S n a
null𝕍S n = svecF n $ const null

{-# INLINE map𝕍S #-}
map𝕍S ∷ (𝒩 n) ⇒ (a → b) → 𝕍S n a → 𝕍S n b
map𝕍S f = svec ∘ map f ∘ iter𝕍S

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
xs ⋅ ys = sum $ map (\ i → xs ⋕ i × ys ⋕ i) $ upTo𝕀64 $ 𝕟64s

(✖) ∷ (𝒩 m,𝒩 n,𝒩 o,Additive a,Times a) ⇒ 𝕍S m (𝕍S o a) → 𝕍S n (𝕍S o a) → 𝕍S m (𝕍S n a)
xs ✖ ys = svecF 𝕟64s $ \ i → svecF 𝕟64s $ \ j → xs ⋕ i ⋅ ys ⋕ j

-------
-- 𝕍 --
-------

newtype 𝕍 a = 𝕍 { un𝕍 ∷ VB.Vector a }
  deriving (Eq,Ord)

instance ToStream a (𝕍 a)      where {-# INLINE stream #-} ; stream = stream𝕍
instance ToIter a (𝕍 a)        where {-# INLINE iter   #-} ; iter   = iter ∘ stream
instance (Show a) ⇒ Show (𝕍 a) where {-# INLINE show   #-} ; show   = chars ∘ show𝕍
instance Lookup ℕ64 a (𝕍 a)    where {-# INLINE (⋕?)   #-} ; (⋕?)   = flip idx𝕍
instance Functor 𝕍             where {-# INLINE map    #-} ; map    = map𝕍

{-# INLINE vec #-}
vec ∷ 𝐼 a → 𝕍 a
vec xs = 𝕍 $ VB.fromList $ lazyList xs

vecS ∷ 𝐼C a → 𝕍 a
vecS xs = 𝕍 $ VB.fromListN (tohs $ intΩ64 $ size xs) $ lazyList xs

{-# INLINE vecF #-}
vecF ∷ ℕ64 → (ℕ64 → a) → 𝕍 a
vecF n f = vec $ map f $ upTo n

{-# INLINE idx𝕍 #-}
idx𝕍 ∷ ℕ64 → 𝕍 a → 𝑂 a
idx𝕍 i xs = frhs $ un𝕍 xs VB.!? tohs (intΩ64 i)

{-# INLINE stream𝕍 #-}
stream𝕍 ∷ 𝕍 a → 𝑆 a
stream𝕍 xs = stream $ VB.toList $ un𝕍 xs

{-# INLINE show𝕍 #-}
show𝕍 ∷ (Show a) ⇒ 𝕍 a → 𝕊
show𝕍 = showCollection "𝕍[" "]" "," show𝕊 ∘ iter

{-# INLINE null𝕍 #-}
null𝕍 ∷ (Null a) ⇒ ℕ64 → 𝕍 a
null𝕍 n = vecF n $ const null

{-# INLINE map𝕍 #-}
map𝕍 ∷ (a → b) → 𝕍 a → 𝕍 b
map𝕍 f = vec ∘ map f ∘ iter

{-# INLINE d𝕍 #-}
d𝕍 ∷ 𝕍 a → (∀ n. (𝒩64 n) ⇒ 𝕍S n a → b) → b
d𝕍 xs f = 𝕟64d (natΩ64 $ frhs $ VB.length $ un𝕍 xs) $ \ (_ ∷ ℕ64S n) → f @ n $ 𝕍S_UNSAFE $ un𝕍 xs

{-# INLINE vecD  #-}
vecD ∷ ℕ64 ⇰ a → 𝕍 a
vecD d = case dmaxKey d of
  None → error "vecD on empty dictionary"
  Some k → vecF (k + one) $ \ n → d ⋕! n

--------
-- 𝕌S --
--------

newtype 𝕌S n a = 𝕌S_UNSAFE { un𝕌S ∷ VU.Vector a }
  deriving (Eq,Ord)

instance (Storable a) ⇒ ToStream a (𝕌S n a)       where {-# INLINE stream #-} ; stream = stream𝕌S
instance (Storable a) ⇒ ToIter a (𝕌S n a)         where {-# INLINE iter   #-} ; iter   = iter ∘ stream
instance (Storable a,Show a) ⇒ Show (𝕌S n a)      where {-# INLINE show   #-} ; show   = chars ∘ show𝕌S
instance (Storable a) ⇒ Access (𝕀64 n) a (𝕌S n a) where {-# INLINE (⋕)    #-} ; (⋕)    = flip idx𝕌S
instance (Storable a) ⇒ Lookup ℕ64 a (𝕌S n a)     where {-# INLINE (⋕?)   #-} ; (⋕?)   = flip idxChecked𝕌S
instance (𝒩 n,Storable a,Null a) ⇒ Null (𝕌S n a)  where {-# INLINE null   #-} ; null   = null𝕌S 𝕟64s

{-# INLINE suvec #-}
suvec ∷ ∀ n a. (𝒩 n,Storable a) ⇒ 𝐼S n a → 𝕌S n a
suvec xs = 𝕌S_UNSAFE $ VU.fromListN (tohs $ intΩ64 $ unℕ64S $ 𝕟64s @ n) $ lazyList $ un𝐼S xs

{-# INLINE suvecF #-}
suvecF ∷ ∀ n a. (𝒩 n,Storable a) ⇒ ℕ64S n → (𝕀64 n → a) → 𝕌S n a
suvecF n f = suvec $ map f $ upTo𝕀64 n

{-# INLINE idx𝕌S #-}
idx𝕌S ∷ (Storable a) ⇒ 𝕀64 n → 𝕌S n a → a
idx𝕌S i xs = VU.unsafeIndex (un𝕌S xs) $ tohs $ intΩ64 $ un𝕀64 i

{-# INLINE idxChecked𝕌S #-}
idxChecked𝕌S ∷ (Storable a) ⇒ ℕ64 → 𝕌S n a → 𝑂 a
idxChecked𝕌S i xs = frhs $ un𝕌S xs VU.!? tohs (intΩ64 i)

{-# INLINE iter𝕌S #-}
iter𝕌S ∷ (Storable a) ⇒ 𝕌S n a → 𝐼S n a
iter𝕌S xs = 𝐼S_UNSAFE $ iter $ stream𝕌S xs

{-# INLINE stream𝕌S #-}
stream𝕌S ∷ (Storable a) ⇒ 𝕌S n a → 𝑆 a
stream𝕌S xs = stream $ VU.toList $ un𝕌S xs

{-# INLINE show𝕌S #-}
show𝕌S ∷ (Storable a,Show a) ⇒ 𝕌S n a → 𝕊
show𝕌S = showCollection "𝕌S[" "]" "," show𝕊 ∘ iter

{-# INLINE null𝕌S #-}
null𝕌S ∷ (𝒩 n,Storable a,Null a) ⇒ ℕ64S n → 𝕌S n a
null𝕌S n = suvecF n $ const null

{-# INLINE map𝕌S #-}
map𝕌S ∷ (𝒩 n,Storable a,Storable b) ⇒ (a → b) → 𝕌S n a → 𝕌S n b
map𝕌S f = suvec ∘ map f ∘ iter𝕌S

-------
-- 𝕌 --
-------

newtype 𝕌 a = 𝕌 { un𝕌 ∷ VU.Vector a }
  deriving (Eq,Ord)

instance (Storable a) ⇒ ToStream a (𝕌 a)   where {-# INLINE stream #-} ; stream = stream𝕌
instance (Storable a) ⇒ ToIter a (𝕌 a)     where {-# INLINE iter   #-} ; iter   = iter ∘ stream
instance (Storable a,Show a) ⇒ Show (𝕌 a)  where {-# INLINE show   #-} ; show   = chars ∘ show𝕌
instance (Storable a) ⇒ Lookup ℕ64 a (𝕌 a) where {-# INLINE (⋕?)   #-} ; (⋕?)   = flip idx𝕌

{-# INLINE uvec #-}
uvec ∷ (Storable a) ⇒ 𝐼 a → 𝕌 a
uvec xs = 𝕌 $ VU.fromList $ lazyList xs

uvecS ∷ (Storable a) ⇒ 𝐼C a → 𝕌 a
uvecS xs = 𝕌 $ VU.fromListN (tohs $ intΩ64 $ size xs) $ lazyList xs

{-# INLINE uvecF #-}
uvecF ∷ (Storable a) ⇒ ℕ64 → (ℕ64 → a) → 𝕌 a
uvecF n f = uvec $ map f $ upTo n

{-# INLINE idx𝕌 #-}
idx𝕌 ∷ (Storable a) ⇒ ℕ64 → 𝕌 a → 𝑂 a
idx𝕌 i xs = frhs $ un𝕌 xs VU.!? tohs (intΩ64 i)

{-# INLINE stream𝕌 #-}
stream𝕌 ∷ (Storable a) ⇒ 𝕌 a → 𝑆 a
stream𝕌 xs = stream $ VU.toList $ un𝕌 xs

{-# INLINE show𝕌 #-}
show𝕌 ∷ (Storable a,Show a) ⇒ 𝕌 a → 𝕊
show𝕌 = showCollection "𝕌[" "]" "," show𝕊 ∘ iter

{-# INLINE null𝕌 #-}
null𝕌 ∷ (Storable a,Null a) ⇒ ℕ64 → 𝕌 a
null𝕌 n = uvecF n $ const null

{-# INLINE map𝕌 #-}
map𝕌 ∷ (Storable a,Storable b) ⇒ (a → b) → 𝕌 a → 𝕌 b
map𝕌 f = uvec ∘ map f ∘ iter

{-# INLINE d𝕌 #-}
d𝕌 ∷ (Storable a) ⇒ 𝕌 a → (∀ n. (𝒩64 n) ⇒ 𝕌S n a → b) → b
d𝕌 xs f = 𝕟64d (natΩ64 $ frhs $ VU.length $ un𝕌 xs) $ \ (_ ∷ ℕ64S n) → f @ n $ 𝕌S_UNSAFE $ un𝕌 xs

{-# INLINE uvecD  #-}
uvecD ∷ (Storable a) ⇒ ℕ64 ⇰ a → 𝕌 a
uvecD d = case dmaxKey d of
  None → uvec null
  Some k → uvecF (k + one) $ \ n → d ⋕! n

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
