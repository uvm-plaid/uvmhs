{-# OPTIONS_GHC -rtsopts -threaded -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -optlo-O3 #-}
module UVMHS.Core.Matrix where

import UVMHS.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data
import UVMHS.Core.Pointed
import UVMHS.Core.IO

import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Repr.Vector as Repa
import qualified Data.Array.Repa.Repr.Unboxed as Repa
import qualified Data.Array.Repa.Eval as Repa
import qualified Prelude as HS
import qualified Data.Proxy as HS
import qualified Data.Functor.Identity as HS
import qualified Data.Type.Equality as HS

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

compareTℕ ∷ ∀ (a ∷ Tℕ) (b ∷ Tℕ). (Rℕ a,Rℕ b) ⇒ 𝑂 (a ≟ b)
compareTℕ = case HS.sameNat (HS.Proxy @ a) (HS.Proxy @ b) of
  HS.Nothing → None
  HS.Just HS.Refl → Some Refl

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

infixr 8 :&&

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

un𝕀32 ∷ 𝕀32 n → ℕ32
un𝕀32 (𝕀32 n _) = unSℕ32 n

s𝕚 ∷ ∀ m n. (Rℕ32 m,m < n) ⇒ P m → 𝕀32 n
s𝕚 P = 𝕀32 (s𝕟32 @ m) TRUSTME_LT

d𝕚 ∷ Sℕ32 m → ℕ32 → 𝑂 (𝕀32 m)
d𝕚 m n = case n ⋚ unSℕ32 m of
  LT → d𝕟32 n $ \ n' → Some $ 𝕀32 n' TRUSTME_LT
  _ → None

-- vectors --

data Bᴍ (m ∷ Tℕ) (n ∷ Tℕ) a where
  Bᴍ ∷ (Rℕ m,Rℕ n) 
     ⇒ { rowsBᴍ ∷ Sℕ32 m
       , colsBᴍ ∷ Sℕ32 n
       , dataBᴍ ∷ Repa.Array Repa.V (Repa.Z Repa.:. HS.Int Repa.:. HS.Int) a
       }
     → Bᴍ m n a
data Uᴍ (m ∷ Tℕ) (n ∷ Tℕ) a where
  Uᴍ ∷ (Rℕ m,Rℕ n,Repa.Unbox a)
     ⇒ { rowsUᴍ ∷ Sℕ32 m
       , colsUᴍ ∷ Sℕ32 n
       , dataUᴍ ∷ Repa.Array Repa.U (Repa.Z Repa.:. HS.Int Repa.:. HS.Int) a
       }
     → Uᴍ m n a
data Vᴍ (m ∷ Tℕ) (n ∷ Tℕ) a where
  Vᴍ ∷ (Rℕ m,Rℕ n)
     ⇒ { rowsVᴍ ∷ Sℕ32 m
       , colsVᴍ ∷ Sℕ32 n
       , dataVᴍ ∷ Repa.Array Repa.D (Repa.Z Repa.:. HS.Int Repa.:. HS.Int) a
       }
     → Vᴍ m n a

infixl 7 𝄪
class Matrix t where 
  xrows ∷ t m n a → Sℕ32 m
  xcols ∷ t m n a → Sℕ32 n
  (𝄪) ∷ t m n a → (𝕀32 m,𝕀32 n) → a
  xvirt ∷ t m n a → Vᴍ m n a

-- boxed --

indexBᴍ ∷ 𝕀32 m → 𝕀32 n → Bᴍ m n a → a
indexBᴍ i j xs = dataBᴍ xs Repa.! (Repa.Z Repa.:. HS.fromIntegral (un𝕀32 i) Repa.:. HS.fromIntegral (un𝕀32 j))

virtBᴍ ∷ Bᴍ m n a → Vᴍ m n a
virtBᴍ (Bᴍ m n xs) = Vᴍ m n $ Repa.delay xs

instance Matrix Bᴍ where 
  xrows = rowsBᴍ
  xcols = colsBᴍ
  xs 𝄪 (i,j) = indexBᴍ i j xs
  xvirt = virtBᴍ

-- unboxed --

indexUᴍ ∷ 𝕀32 m → 𝕀32 n → Uᴍ m n a → a
indexUᴍ i j (Uᴍ _ _ xs) = xs Repa.! (Repa.Z Repa.:. HS.fromIntegral (un𝕀32 i) Repa.:. HS.fromIntegral (un𝕀32 j))

virtUᴍ ∷ Uᴍ m n a → Vᴍ m n a
virtUᴍ (Uᴍ m n xs) = Vᴍ m n $ Repa.delay xs

instance Matrix Uᴍ where 
  xrows = rowsUᴍ
  xcols = colsUᴍ
  xs 𝄪 (i,j) = indexUᴍ i j xs
  xvirt = virtUᴍ

-- virtual --

indexVᴍ ∷ 𝕀32 m → 𝕀32 n → Vᴍ m n a → a
indexVᴍ i j xs = dataVᴍ xs Repa.! (Repa.Z Repa.:. HS.fromIntegral (un𝕀32 i) Repa.:. HS.fromIntegral (un𝕀32 j)) 

instance Matrix Vᴍ where
  xrows = rowsVᴍ
  xcols = colsVᴍ
  xs 𝄪 (i,j) = indexVᴍ i j xs
  xvirt = id

matrix ∷ (Rℕ m,Rℕ n) ⇒ Sℕ32 m → Sℕ32 n → (𝕀32 m → 𝕀32 n → a) → Vᴍ m n a
matrix m n f = 
  Vᴍ m n $ Repa.fromFunction (Repa.Z Repa.:. HS.fromIntegral (unSℕ32 m) Repa.:. HS.fromIntegral (unSℕ32 n)) $ \ (Repa.Z Repa.:. i Repa.:. j) → 
    d𝕟32 (HS.fromIntegral i) $ \ i' → 
      d𝕟32 (HS.fromIntegral j) $ \ j' →
        f (𝕀32 i' TRUSTME_LT) (𝕀32 j' TRUSTME_LT)

xconst ∷ (Rℕ m,Rℕ n) ⇒ Sℕ32 m → Sℕ32 n → a → Vᴍ m n a
xconst m n x = matrix m n $ \ _ _ → x

xbs ∷ Vᴍ m n a → Bᴍ m n a
xbs (Vᴍ m n xs) = Bᴍ m n $ Repa.computeS xs

xbp ∷ Vᴍ m n a → Bᴍ m n a
xbp (Vᴍ m n xs) = Bᴍ m n $ HS.runIdentity $ Repa.computeP xs

xus ∷ (Repa.Unbox a) ⇒ Vᴍ m n a → Uᴍ m n a
xus (Vᴍ m n xs) = Uᴍ m n $ Repa.computeS xs

xup ∷ (Repa.Unbox a) ⇒ Vᴍ m n a → Uᴍ m n a
xup (Vᴍ m n xs) = Uᴍ m n $ HS.runIdentity $ Repa.computeP xs

xiter ∷ Vᴍ m n a → 𝐼 a
xiter xs = iter $ Repa.toList $ dataVᴍ xs

instance ToIter a (Bᴍ m n a) where iter = iter ∘ xvirt
instance ToIter a (Uᴍ m n a) where iter = iter ∘ xvirt
instance ToIter a (Vᴍ m n a) where iter = xiter

-------------
-- DERIVED --
-------------

xtranspose ∷ Vᴍ m n a → Vᴍ n m a
xtranspose xs@(Vᴍ _ _ _) = matrix (xcols xs) (xrows xs) $ \ j i → xs 𝄪 (i,j)

xmap ∷ (a → b) → Vᴍ m n a → Vᴍ m n b
xmap f xs@(Vᴍ _ _ _) = matrix (xrows xs) (xcols xs) $ \ i j → f $ xs 𝄪 (i,j)

instance Functor (Vᴍ m n) where map = xmap

xmap2 ∷ (a → b → c) → Vᴍ m n a → Vᴍ m n b → Vᴍ m n c
xmap2 f xs@(Vᴍ _ _ _) ys@(Vᴍ _ _ _) = matrix (xrows xs) (xcols xs) $ \ i j → f (xs 𝄪 (i,j)) (ys 𝄪 (i,j))

xmeld ∷ (Rℕ n) ⇒ Sℕ32 n → Vᴍ m 1 (Vᴍ 1 n a) → Vᴍ m n a
xmeld n xys@(Vᴍ _ _ _) = matrix (xrows xys) n $ \ i j → indexVᴍ (s𝕚 @ 0 P) j $ indexVᴍ i (s𝕚 @ 0 P) xys

xsplit ∷ Vᴍ m n a → Vᴍ m 1 (Vᴍ 1 n a)
xsplit xys@(Vᴍ _ _ _) = matrix (xrows xys) (s𝕟32 @ 1) $ \ i _ → matrix (s𝕟32 @ 1) (colsVᴍ xys) $ \ _ j → indexVᴍ i j xys

xrow ∷ 𝕀32 m → Vᴍ m n a → Vᴍ 1 n a
xrow i xs@(Vᴍ _ _ _) = matrix (s𝕟32 @ 1) (colsVᴍ xs) $ \ _ j → indexVᴍ i j xs

xcol ∷ 𝕀32 n → Vᴍ m n a → Vᴍ 1 m a
xcol i xs = xrow i $ xtranspose xs

xproduct ∷ (Additive a,Times a) ⇒ Vᴍ m n a → Vᴍ n o a → Vᴍ m o a
xproduct xs@(Vᴍ _ _ _) ys@(Vᴍ _ _ _) =
  matrix (xrows xs) (xcols ys) $ \ i k →
    let r₁ = xrow i xs
        r₂ = xcol k ys
    in sum $ iter $ xmap2 (×) r₁ r₂

xbmapM ∷ (Monad m) ⇒ (a → m b) → Vᴍ n o a → m (Bᴍ n o b)
xbmapM f xs@(Vᴍ _ _ _) = do
  xs' ← mapM (mapM f) $ xiter2 xs
  return $ xb𝐿 (list $ map list xs') $ \ (Bᴍ _ _ xs'') → Bᴍ (xrows xs) (xcols xs) xs''

xumapM ∷ (Monad m,Repa.Unbox a,Repa.Unbox b) ⇒ (a → m b) → Vᴍ n o a → m (Uᴍ n o b)
xumapM f xs@(Vᴍ _ _ _) = do
  xs' ← mapM (mapM f) $ xiter2 xs
  return $ xu𝐿 (list $ map list xs') $ \ (Uᴍ _ _ xs'') → Uᴍ (xrows xs) (xcols xs) xs''

xindirect ∷ Vᴍ m n a → Vᴍ 1 o (𝕀32 m) → Vᴍ o n a
xindirect xs@(Vᴍ _ _ _) is@(Vᴍ _ _ _) = matrix (xcols is) (xcols xs) $ \ o n → xs 𝄪 (is 𝄪 (s𝕚 @ 0 P,o),n)

xiter2 ∷ Vᴍ m n a → 𝐼 (𝐼 a)
xiter2 = map iter ∘ iter ∘ xsplit

xlist2 ∷ Vᴍ m n a → 𝐿 (𝐿 a)
xlist2 = list ∘ map list ∘ xiter2

xb𝐿 ∷ 𝐿 (𝐿 a) → (∀ m n. (Rℕ m,Rℕ n) ⇒ Bᴍ m n a → b) → b
xb𝐿 xs f =
  let uc = joins $ map (natΩ32 ∘ count) xs
      lc = meets $ map (AddTop ∘ natΩ32 ∘ count) xs
  in case AddTop uc ≡ lc of
    True → 
      d𝕟32 uc $ \ n →
      d𝕟32 (natΩ32 $ count xs) $ \ m →
        f $ Bᴍ m n $ Repa.fromList (Repa.Z Repa.:. HS.fromIntegral (unSℕ32 m) Repa.:. HS.fromIntegral (unSℕ32 n)) $ tohs $ concat xs
    False → error "`xb𝐿`: bad input list: input list is either empty (no columns) or has columns of different length"

xu𝐿 ∷ (Repa.Unbox a) ⇒ 𝐿 (𝐿 a) → (∀ m n. (Rℕ m,Rℕ n) ⇒ Uᴍ m n a → b) → b
xu𝐿 xs f =
  let uc = joins $ map (natΩ32 ∘ count) xs
      lc = meets $ map (AddTop ∘ natΩ32 ∘ count) xs
  in case AddTop uc ≡ lc of
    True → 
      d𝕟32 uc $ \ n →
      d𝕟32 (natΩ32 $ count xs) $ \ m →
        f $ Uᴍ m n $ Repa.fromList (Repa.Z Repa.:. HS.fromIntegral (unSℕ32 m) Repa.:. HS.fromIntegral (unSℕ32 n)) $ tohs $ concat xs
    False → error "`xb𝐿`: bad input list: input list is either empty (no columns) or has columns of different length"

testMatrix1 ∷ IO ()
testMatrix1 = do
  let xs = list [list [1,2,3],list [4,5,6],list [7,8,9]]
  shout xs
  xb𝐿 xs $ \ xs' → do
    let ys = xlist2 $ xtranspose $ xvirt xs'
    shout ys
