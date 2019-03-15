module UVMHS.Core.Vector2 where

import UVMHS.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data

import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Repr.Vector as Repa
import qualified Data.Array.Repa.Repr.Unboxed as Repa
import qualified Prelude as HS
import qualified Data.Proxy as HS
import qualified Data.Functor.Identity as HS

import qualified GHC.TypeLits as HS

-- type lits --

type Tℕ = HS.Nat
type T𝕊 = HS.Symbol

type (m ∷ Tℕ) +  (n ∷ Tℕ) = m HS.+ n
type (m ∷ Tℕ) ×  (n ∷ Tℕ) = m HS.* n
type (m ∷ Tℕ) ^  (n ∷ Tℕ) = m HS.^ n
type (m ∷ Tℕ) -  (n ∷ Tℕ) = m HS.- n
type (m ∷ Tℕ) /  (n ∷ Tℕ) = m `HS.Div` n
type (m ∷ Tℕ) %  (n ∷ Tℕ) = m `HS.Mod` n
type Log2 (n ∷ Tℕ) = HS.Log2 n

type (m ∷ Tℕ) ⋚? (n ∷ Tℕ) = HS.CmpNat m n

type (m ∷ Tℕ) <  (n ∷ Tℕ) = (m ⋚? n) ~ 'LT
type (m ∷ Tℕ) ≡  (n ∷ Tℕ) = (m ⋚? n) ~ 'EQ
type (m ∷ Tℕ) >  (n ∷ Tℕ) = (m ⋚? n) ~ 'GT

data (m ∷ Tℕ) :<: (n ∷ Tℕ) = TRUSTME_LT

newtype Sℕ (n ∷ Tℕ) = TRUSTME_Sℕ { unSℕ ∷ ℕ }
newtype Sℕ32 (n ∷ Tℕ) = TRUSTME_Sℕ32 { unSℕ32 ∷ ℕ32 }
newtype S𝕊 (s ∷ T𝕊) = TRUSTME_S𝕊 { unS𝕊 ∷ 𝕊 }

class (HS.KnownNat n) ⇒ Rℕ (n ∷ Tℕ) where reifyℕ ∷ P n → ℕ
class (HS.KnownNat n) ⇒ Rℕ32 (n ∷ Tℕ) where reifyℕ32 ∷ P n → ℕ32
class (HS.KnownSymbol s) ⇒ R𝕊 (s ∷ T𝕊) where reify𝕊 ∷ P s → 𝕊

instance (HS.KnownNat n) ⇒ Rℕ (n ∷ Tℕ) where reifyℕ P = natΩ $ HS.natVal @ n P
instance (HS.KnownNat n) ⇒ Rℕ32 (n ∷ Tℕ) where reifyℕ32 P = natΩ32 $ HS.natVal @ n P
instance (HS.KnownSymbol s) ⇒ R𝕊 (s ∷ T𝕊) where reify𝕊 P = string $ HS.symbolVal @ s P

s𝕟 ∷ ∀ n. (Rℕ n) ⇒ Sℕ n
s𝕟 = TRUSTME_Sℕ $ reifyℕ @ n P

d𝕟 ∷ ℕ → (∀ n. (Rℕ n) ⇒ Sℕ n → a) → a
d𝕟 n f = case HS.someNatVal $ int n of
  HS.Nothing → error "impossible"
  HS.Just (HS.SomeNat (HS.Proxy ∷ HS.Proxy n)) → f $ TRUSTME_Sℕ @ n n

s𝕟32 ∷ ∀ n. (Rℕ32 n) ⇒ Sℕ32 n
s𝕟32 = TRUSTME_Sℕ32 $ reifyℕ32 @ n P

d𝕟32 ∷ ℕ32 → (∀ n. (Rℕ32 n) ⇒ Sℕ32 n → a) → a
d𝕟32 n f = case HS.someNatVal $ int n of
  HS.Nothing → error "impossible"
  HS.Just (HS.SomeNat (HS.Proxy ∷ HS.Proxy n)) → f $ TRUSTME_Sℕ32 @ n n

s𝕤 ∷ ∀ s. (HS.KnownSymbol s) ⇒ S𝕊 s
s𝕤 = TRUSTME_S𝕊 $ reify𝕊 @ s P

d𝕤 ∷ 𝕊 → (∀ s. (R𝕊 s) ⇒ S𝕊 s → a) → a
d𝕤 s f = case HS.someSymbolVal $ chars s of
  HS.SomeSymbol (HS.Proxy ∷ HS.Proxy s) → f $ TRUSTME_S𝕊 @ s s

infixr  8 :&&

data S𝐿 (a ∷ sa → ★) ∷ [sa] → ★ where
  SNil ∷ S𝐿 a '[]
  (:&&) ∷ a x → S𝐿 a xs → S𝐿 a (x ': xs)

mapS𝐿 ∷ ∀ (a ∷ sa → ★) (b ∷ sa → ★) (xs ∷ [sa]). (∀ (x ∷ sa). a x → b x) → S𝐿 a xs → S𝐿 b xs
mapS𝐿 _ SNil = SNil
mapS𝐿 f (x :&& xs) = f x :&& mapS𝐿 f xs

newtype Const (a ∷ ★) (b ∷ k) = Const { unConst ∷ a }

type family (xs ∷ [a]) ⧺ (ys ∷ [a]) ∷ [a] where
  '[] ⧺ ys = ys
  (x ': xs) ⧺ ys = x ': (xs ⧺ ys)

-- indices --

data 𝕀32 (n ∷ Tℕ) where
  𝕀32 ∷ Sℕ32 m → m :<: n → 𝕀32 n

-- vectors --

type family ToRepaℕs (ns ∷ [Tℕ]) ∷ ★ where
  ToRepaℕs '[] = Repa.Z
  ToRepaℕs (n ': ns) = ToRepaℕs ns Repa.:. HS.Int

data B𝕍 (ns ∷ [Tℕ]) a where
  B𝕍 ∷ (Repa.Shape (ToRepaℕs ns)) ⇒ Sℕ32s ns → Repa.Array Repa.V (ToRepaℕs ns) a → B𝕍 ns a
data U𝕍 (ns ∷ [Tℕ]) a where
  U𝕍 ∷ (Repa.Shape (ToRepaℕs ns)) ⇒ Sℕ32s ns → Repa.Array Repa.U (ToRepaℕs ns) a → U𝕍 ns a
data V𝕍 (ns ∷ [Tℕ]) a where
  V𝕍 ∷ (Repa.Shape (ToRepaℕs ns)) ⇒ Sℕ32s ns → Repa.Array Repa.D (ToRepaℕs ns) a → V𝕍 ns a

type 𝕀32s (ns ∷ [Tℕ]) = S𝐿 𝕀32 ns
type Sℕ32s (ns ∷ [Tℕ]) = S𝐿 Sℕ32 ns

toShapeSℕ32s ∷ Sℕ32s ns → W (Repa.Shape (ToRepaℕs ns))
toShapeSℕ32s SNil = W
toShapeSℕ32s (_ :&& ns) = with (toShapeSℕ32s ns) W

toRepaSℕ32s ∷ Sℕ32s ns → ToRepaℕs ns
toRepaSℕ32s SNil = Repa.Z
toRepaSℕ32s (n :&& ns) = toRepaSℕ32s ns  Repa.:. HS.fromIntegral (unSℕ32 n)

toRepa𝕀32s ∷ 𝕀32s ns → ToRepaℕs ns
toRepa𝕀32s SNil = Repa.Z
toRepa𝕀32s (𝕀32 n _ :&& ns) = toRepa𝕀32s ns  Repa.:. HS.fromIntegral (unSℕ32 n)

frRepa𝕀32s ∷ Sℕ32s ns → ToRepaℕs ns → 𝕀32s ns
frRepa𝕀32s SNil Repa.Z = SNil
frRepa𝕀32s (_ :&& ss) (ns Repa.:. n) = d𝕟32 (HS.fromIntegral n) $ \ n' → 𝕀32 n' TRUSTME_LT :&& frRepa𝕀32s ss ns

-- boxed --

dimB𝕍 ∷ B𝕍 ns a → Sℕ32s ns
dimB𝕍 (B𝕍 n _) = n

indexB𝕍 ∷ 𝕀32s ns → B𝕍 ns a → a
indexB𝕍 i (B𝕍 _ xs) = xs Repa.! toRepa𝕀32s i 

virtB𝕍 ∷ B𝕍 ns a → V𝕍 ns a
virtB𝕍 (B𝕍 n xs) = V𝕍 n $ Repa.delay xs

-- unboxed --

dimU𝕍 ∷ U𝕍 ns a → Sℕ32s ns
dimU𝕍 (U𝕍 n _) = n

indexU𝕍 ∷ (Repa.Unbox a) ⇒ 𝕀32s ns → U𝕍 ns a → a
indexU𝕍 i (U𝕍 _ xs) = xs Repa.! toRepa𝕀32s i

-- virtual --

dimV𝕍 ∷ V𝕍 ns a → Sℕ32s ns
dimV𝕍 (V𝕍 n _) = n

indexV𝕍 ∷ 𝕀32s ns → V𝕍 ns a → a
indexV𝕍 i (V𝕍 _ xs) = xs Repa.! toRepa𝕀32s i 

makeV𝕍 ∷ Sℕ32s ns → (𝕀32s ns → a) → V𝕍 ns a
makeV𝕍 n f = with (toShapeSℕ32s n) $ 
  V𝕍 n $ Repa.fromFunction (toRepaSℕ32s n) $ \ i → 
    f (frRepa𝕀32s n i)

concV𝕍 ∷ V𝕍 ns a → B𝕍 ns a
concV𝕍 (V𝕍 n xs) = B𝕍 n $ HS.runIdentity $ Repa.computeP xs

iterV𝕍 ∷ V𝕍 ns a → 𝐼 a
iterV𝕍 (V𝕍 _ xs) = iter $ Repa.toList xs

----------
-- SAFE --
----------

zipWithV𝕍 ∷ (a → b → c) → V𝕍 ns a → V𝕍 ns b → V𝕍 ns c
zipWithV𝕍 f xs ys = makeV𝕍 (dimV𝕍 xs) $ \ i → f (indexV𝕍 i xs) (indexV𝕍 i ys)

transposeV𝕍 ∷ V𝕍 (n₁ : n₂ : ns) a → V𝕍 (n₂ : n₁ : ns) a
transposeV𝕍 xs = 
  let (n₁ :&& n₂ :&& ns) = dimV𝕍 xs
  in makeV𝕍 (n₂ :&& n₁ :&& ns) $ \ (i₂ :&& i₁ :&& is) → indexV𝕍 (i₁ :&& i₂ :&& is) xs

rowV𝕍 ∷ 𝕀32 n₁ → V𝕍 (n₁ : ns) a → V𝕍 ns a
rowV𝕍 i xs =
  let (_ :&& ns) = dimV𝕍 xs
  in makeV𝕍 ns $ \ is → indexV𝕍 (i :&& is) xs

colV𝕍 ∷ 𝕀32 n₂ → V𝕍 (n₁ : n₂ : ns) a → V𝕍 (n₁ : ns) a
colV𝕍 i xs =
  let (n₁ :&& _ :&& ns) = dimV𝕍 xs
  in makeV𝕍 (n₁ :&& ns) $ \ (i₁ :&& is) → indexV𝕍 (i₁ :&& i :&& is) xs

productV𝕍 ∷ (Additive a,Times a) ⇒ V𝕍 [n₁,n₂] a → V𝕍 [n₂,n₃] a → V𝕍 [n₁,n₃] a
productV𝕍 xs ys =
  let (n₁ :&& _  :&& SNil) = dimV𝕍 xs
      (_  :&& n₃ :&& SNil) = dimV𝕍 ys
  in 
  makeV𝕍 (n₁ :&& n₃ :&& SNil) $ \ (i₁ :&& i₃ :&& SNil) →
    let v₁ = rowV𝕍 i₁ xs
        v₂ = colV𝕍 i₃ ys
    in sum $ iterV𝕍 $ zipWithV𝕍 (×) v₁ v₂
