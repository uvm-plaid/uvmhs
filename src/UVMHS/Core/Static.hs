module UVMHS.Core.Static where

import UVMHS.Core.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data

import qualified Prelude as HS
import qualified Data.Proxy as HS
import qualified Data.Type.Equality as HS

import qualified GHC.TypeLits as HS

infix 4 ≍,≺,≼,≻,<,≤

-- type literals: nat and string --

type family (a ∷ k) ≡ (b ∷ k) where
  a ≡ a = 'True
  _ ≡ _ = 'False

type family (x ∷ 𝔹) ⩔ (y ∷ 𝔹) where
  'False ⩔ x = x
  x ⩔ 'False = x
  'True ⩔ 'True = 'True

type family (x ∷ 𝔹) ⩓ (y ∷ 𝔹) where
  'True ⩓ x = x
  x ⩓ 'True = x
  'False ⩓ 'False = 'False

type 𝐍 = HS.Nat
type 𝐒 = HS.Symbol

type (m ∷ 𝐍) + (n ∷ 𝐍) = m HS.+ n
type (m ∷ 𝐍) × (n ∷ 𝐍) = m HS.* n
type (m ∷ 𝐍) ^ (n ∷ 𝐍) = m HS.^ n
type (m ∷ 𝐍) - (n ∷ 𝐍) = m HS.- n
type (m ∷ 𝐍) / (n ∷ 𝐍) = m `HS.Div` n
type (m ∷ 𝐍) % (n ∷ 𝐍) = m `HS.Mod` n
type Log2 (n ∷ 𝐍) = HS.Log2 n

type (m ∷ 𝐍) ⋚ (n ∷ 𝐍) = HS.CmpNat m n

type (m ∷ 𝐍) ≺  (n ∷ 𝐍) = (m ⋚ n) ≡ 'LT ~ 'True
type (m ∷ 𝐍) ≍  (n ∷ 𝐍) = (m ⋚ n) ≡ 'EQ ~ 'True
type (m ∷ 𝐍) ≻  (n ∷ 𝐍) = (m ⋚ n) ≡ 'GT ~ 'True

type (m ∷ 𝐍) ≼ (n ∷ 𝐍) = ((m ⋚ n) ≡ 'LT) ⩔ ((m ⋚ n ≡ 'EQ)) ~ 'True

wnlt_UNSAFE ∷ ∀ m n. P m → P n → W (m ≺ n)
wnlt_UNSAFE _ _ = weq_UNSAFE @ (m ⋚ n ≡ 'LT) @ 'True P P

wneq_UNSAFE ∷ ∀ m n. P m → P n → W (m ≍ n)
wneq_UNSAFE _ _ = weq_UNSAFE @ (m ⋚ n ≡ 'EQ) @ 'True P P

wngt_UNSAFE ∷ ∀ m n. P m → P n → W (m ≻ n)
wngt_UNSAFE _ _ = weq_UNSAFE @ (m ⋚ n ≡ 'GT) @ 'True P P

wnlte_UNSAFE ∷ ∀ m n. P m → P n → W (m ≼ n)
wnlte_UNSAFE _ _ = weq_UNSAFE @ ((m ⋚ n ≡ 'LT) ⩔ (m ⋚ n ≡ 'EQ)) @ 'True P P 

data (m ∷ 𝐍) < (n ∷ 𝐍) where
  W_LT ∷ (m ≺ n) ⇒ m < n

withLT ∷ m < n → ((m ≺ n) ⇒ a) → a
withLT W_LT x = x

nlt_UNSAFE ∷ ∀ m n. P m → P n → m < n
nlt_UNSAFE _ _ = with (wnlt_UNSAFE @ m @ n P P) W_LT

instance Transitive (<) where
  _ ⊚ _ = nlt_UNSAFE P P

data (m ∷ 𝐍) ≤ (n ∷ 𝐍) where
  W_LTE ∷ (m ≼ n) ⇒ m ≤ n

withLTE ∷ m ≤ n → ((m ≼ n) ⇒ a) → a
withLTE W_LTE x = x

nlte_UNSAFE ∷ ∀ m n. P m → P n → m ≤ n
nlte_UNSAFE _ _ = with (wnlte_UNSAFE @ m @ n P P) W_LTE

instance Reflexive (≤) where
  refl = W_LTE
instance Transitive (≤) where
  _ ⊚ _ = nlte_UNSAFE P P
instance Category (≤)

irreflLT ∷ n < n → Void
irreflLT _ = void_UNSAFE

weakenLT ∷ n₁ < n₂ → n₁ ≤ n₂
weakenLT _ = nlte_UNSAFE P P

succLT ∷ n₁ < n₂ → n₁+1 ≤ n₂
succLT _ = nlte_UNSAFE P P

succLTE ∷ n₁ ≤ n₂ → n₁ < n₂+1
succLTE _ = nlt_UNSAFE P P

(⊚♯) ∷ n₁ ≤ n₂ → n₂ < n₃ → n₁ < n₃
_ ⊚♯ _ = nlt_UNSAFE P P

(♯⊚) ∷ n₁ < n₂ → n₂ ≤ n₃ → n₁ < n₃
_ ♯⊚ _ = nlt_UNSAFE P P

-- singleton literals: nat, nat64 and string --

newtype ℕS   (n ∷ 𝐍) = ℕS_UNSAFE   { unℕS   ∷ ℕ   }
newtype ℕ64S (n ∷ 𝐍) = ℕ64S_UNSAFE { unℕ64S ∷ ℕ64 }
newtype 𝕊S   (s ∷ 𝐒) = 𝕊S_UNSAFE   { un𝕊S   ∷ 𝕊   }

class (HS.KnownNat n)    ⇒ 𝒩   (n ∷ 𝐍) where reifyℕ   ∷ P n → ℕ
class (HS.KnownNat n)    ⇒ 𝒩64 (n ∷ 𝐍) where reifyℕ64 ∷ P n → ℕ64
class (HS.KnownSymbol s) ⇒ 𝒮   (s ∷ 𝐒) where reify𝕊   ∷ P s → 𝕊

instance (HS.KnownNat n)    ⇒ 𝒩   (n ∷ 𝐍) where reifyℕ   _ = natΩ   $ HS.natVal    @ n P
instance (HS.KnownNat n)    ⇒ 𝒩64 (n ∷ 𝐍) where reifyℕ64 _ = natΩ64 $ HS.natVal    @ n P
instance (HS.KnownSymbol s) ⇒ 𝒮   (s ∷ 𝐒) where reify𝕊   _ = string $ HS.symbolVal @ s P

compare𝐍 ∷ ∀ (a ∷ 𝐍) (b ∷ 𝐍). (𝒩 a,𝒩 b) ⇒ 𝑂 (a ≟ b)
compare𝐍 = case HS.sameNat (HS.Proxy @ a) (HS.Proxy @ b) of
  HS.Nothing → None
  HS.Just HS.Refl → Some Refl

𝕟s ∷ ∀ n. (𝒩 n) ⇒ ℕS n
𝕟s = ℕS_UNSAFE $ reifyℕ @ n P

𝕟d ∷ ℕ → (∀ n. (𝒩 n) ⇒ ℕS n → a) → a
𝕟d n f = case HS.someNatVal $ int n of
  HS.Nothing → error "impossible"
  HS.Just (HS.SomeNat (HS.Proxy ∷ HS.Proxy n)) → f @ n $ ℕS_UNSAFE n

𝕟64s ∷ ∀ n. (𝒩64 n) ⇒ ℕ64S n
𝕟64s = ℕ64S_UNSAFE $ reifyℕ64 @ n P

𝕟64d ∷ ℕ64 → (∀ n. (𝒩64 n) ⇒ ℕ64S n → a) → a
𝕟64d n f = case HS.someNatVal $ int n of
  HS.Nothing → error "impossible"
  HS.Just (HS.SomeNat (HS.Proxy ∷ HS.Proxy n)) → f @ n $ ℕ64S_UNSAFE n

𝕤s ∷ ∀ s. (HS.KnownSymbol s) ⇒ 𝕊S s
𝕤s = 𝕊S_UNSAFE $ reify𝕊 @ s P

𝕤sd ∷ 𝕊 → (∀ s. (𝒮 s) ⇒ 𝕊S s → a) → a
𝕤sd s f = case HS.someSymbolVal $ tohsChars s of
  HS.SomeSymbol (HS.Proxy ∷ HS.Proxy s) → f $ 𝕊S_UNSAFE @ s s

-- heterogeneous lists --

-- infixr 8 :&&

type family (xs ∷ [a]) ⧺ (ys ∷ [a]) ∷ [a] where
  '[] ⧺ ys = ys
  (x ': xs) ⧺ ys = x ': (xs ⧺ ys)

type family Sum (ns ∷ [𝐍]) where
  Sum '[] = 0
  Sum (n ': ns) = n + Sum ns

type family Prod (ns ∷ [𝐍]) where
  Prod '[] = 1
  Prod (n ': ns) = n × Prod ns

-- data 𝐿S (is ∷ [i]) (c ∷ i → Constraint) (a ∷ i → ★) ∷ ★ where
--   NilS ∷ 𝐿S '[] c a
--   (:&&) ∷ (c x) ⇒ a x → 𝐿S xs c a → 𝐿S (x ': xs) c a
-- 
-- map𝐿S ∷ ∀ i (xs ∷ [i]) (c ∷ i → Constraint) (a ∷ i → ★) (b ∷ i → ★) . (∀ (x ∷ i). a x → b x) → 𝐿S xs c a → 𝐿S xs c b
-- map𝐿S f = \case
--   NilS → NilS
--   x :&& xs → f x :&& map𝐿S f xs
-- 
-- append𝐿S ∷ 𝐿S xs c a → 𝐿S ys c a → 𝐿S (xs ⧺ ys) c a
-- append𝐿S xs ys = case xs of
--   NilS → ys
--   x :&& xs' → x :&& append𝐿S xs' ys
-- 
-- iter𝐿S ∷ ∀ i (xs ∷ [i]) (c ∷ i → Constraint) (a ∷ i → ★) (b ∷ ★). (∀ (x ∷ i). (c x) ⇒ a x → b) → 𝐿S xs c a → 𝐼 b
-- iter𝐿S f = \case
--   NilS → null
--   x :&& xs → single (f x) ⧺ iter𝐿S f xs

type family AllC (c ∷ a → Constraint) (xs ∷ [a]) ∷ Constraint where
  AllC _ '[] = ()
  AllC c (x ': xs) = (c x,AllC c xs)

-- class (AllC c xs) ⇒ AllCC c xs
-- instance (AllC c xs) ⇒ AllCC c xs

-- instance (∀ x. (c x) ⇒ Plus (a x)) ⇒ Plus (𝐿S xs c a) where
--   NilS + NilS = NilS
--   x :&& xs + y :&& ys = (x + y) :&& (xs + ys)
-- 
-- instance (∀ x. (c x) ⇒ Times (a x)) ⇒ Times (𝐿S xs c a) where
--   NilS × NilS = NilS
--   x :&& xs × y :&& ys = (x × y) :&& (xs × ys)

data Spine ∷ [a] → ★ where
  NilSpine ∷ Spine '[]
  ConsSpine ∷ Spine xs → Spine (x ': xs)

class HasSpine xs where spine ∷ Spine xs

instance HasSpine '[] where spine = NilSpine
instance (HasSpine xs) ⇒ HasSpine (x ': xs) where spine = ConsSpine spine

-- zero𝐿S ∷ (AllC c xs,∀ x. (c x) ⇒ Zero (a x)) ⇒ Spine xs → 𝐿S xs c a
-- zero𝐿S = \case
--   NilSpine → NilS
--   ConsSpine sp → zero :&& zero𝐿S sp
-- 
-- instance (HasSpine xs,AllC c xs,∀ x. (c x) ⇒ Zero (a x)) ⇒ Zero (𝐿S xs c a) where
--   zero = zero𝐿S spine
-- 
-- type family PrependMany (xs ∷ [a]) (xxs ∷ [[a]]) ∷ [[a]] where
--   PrependMany _ '[] = '[]
--   PrependMany xs (xs' ': xss) = (xs ⧺ xs') ': PrependMany xs xss

-- indices --

newtype 𝕀64 (n ∷ 𝐍) = 𝕀64_UNSAFE { un𝕀64 ∷ ℕ64 }

𝕚64 ∷ ∀ m n. (m ≺ n) ⇒ ℕ64S m → 𝕀64 n
𝕚64 m = 𝕀64_UNSAFE $ unℕ64S m

𝕚64d ∷ ∀ n. (𝒩64 n) ⇒ ℕ64 → 𝑂 (𝕀64 n)
𝕚64d m = 
  if m < unℕ64S (𝕟64s @ n)
  then Some $ 𝕀64_UNSAFE m
  else None

𝕟64di ∷ ∀ n a. 𝕀64 n → (∀ m. (m ≺ n) ⇒ ℕ64S m → a) → a
𝕟64di i f = 𝕟64d (un𝕀64 i) HS.$ \ (m ∷ ℕ64S m) → with (wnlt_UNSAFE @ m @ n P P) HS.$ f m

upTo𝕀64 ∷ ∀ n. (𝒩64 n) ⇒ ℕ64S n → 𝐼S n (𝕀64 n)
upTo𝕀64 n = 𝐼S_UNSAFE $ map 𝕀64_UNSAFE $ upTo $ unℕ64S n

wk𝕀64 ∷ (m ≼ n) ⇒ 𝕀64 m → 𝕀64 n
wk𝕀64 i = 𝕀64_UNSAFE $ un𝕀64 i

-- static iterators --

newtype 𝐼S (n ∷ 𝐍) a = 𝐼S_UNSAFE
  { un𝐼S ∷ 𝐼 a
  } deriving (Show)

instance Functor (𝐼S n) where map f xs = 𝐼S_UNSAFE $ map f $ un𝐼S xs
instance ToIter a (𝐼S n a) where iter = un𝐼S

-- class ToIterS n a t | t → n,t → a where iterS ∷ t → 𝐼S n a

-- classes --

-- infixl 5 +♮
-- infixl 6 ×♮
-- 
-- class ZeroS  t where zeroS ∷ t 0
-- class OneS   t where oneS  ∷ t 1
-- class PlusS  t where (+♮)  ∷ t m → t n → t (m + n)
-- class TimesS t where (×♮)  ∷ t m → t n → t (m × n)
-- 
-- instance ZeroS  ℕ64S where zeroS  = 𝕟64s @ 0
-- instance OneS   ℕ64S where oneS   = 𝕟64s @ 1
-- instance PlusS  ℕ64S where m +♮ n = ℕ64S_UNSAFE $ unℕ64S m + unℕ64S n
-- instance TimesS ℕ64S where m ×♮ n = ℕ64S_UNSAFE $ unℕ64S m × unℕ64S n

class NullS t where 
  nullS ∷ t 0 a
class SingleS t where
  𝔢 ∷ a → t 1 a
class AppendS t where 
  (⧺♮) ∷ t n₁ a → t n₂ a → t (n₁ + n₂) a
-- class AppendSL t where
--   (⧺♭) ∷ t ns₁ a → t ns₂ a → t (ns₁ ⧺ ns₂) a

instance NullS 𝐼S where nullS = 𝐼S_UNSAFE null
instance SingleS 𝐼S where 𝔢 = 𝐼S_UNSAFE ∘ single
instance AppendS 𝐼S where xs ⧺♮ ys = 𝐼S_UNSAFE $ un𝐼S xs ⧺ un𝐼S ys
-- instance AppendSL (𝐿S c) where (⧺♭) = append𝐿S
