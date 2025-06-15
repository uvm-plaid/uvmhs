module UVMHS.Core.Data.Dict where

import UVMHS.Core.Init
import UVMHS.Core.Classes

import UVMHS.Core.Data.Iter
import UVMHS.Core.Data.Set
import UVMHS.Core.Data.Pair
import UVMHS.Core.Data.String

import qualified Data.Set as Set

import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map

import qualified Prelude as HS

infixr 1 ↦
infix 4 ⫑,⋿
infixl 5 ⩌,⧅
infixl 6 ⩍

-------------------
-- GENERIC CLASS --
-------------------

class
  ( Set k s
  , FunctorM d
  , OFunctorM d
  , KFunctorM k d
  , OKFunctorM k d
  , BiFunctorM d
  , OBiFunctorM d
  , KBiFunctorM k d
  , OKBiFunctorM k d
  , Functor d
  , OFunctor d
  , KFunctor k d
  , OKFunctor k d
  , BiFunctor d
  , OBiFunctor d
  , KBiFunctor k d
  , OKBiFunctor k d
  , ∀ x.                      CSized (d x)
  , ∀ x. (Eq x)             ⇒ Eq (d x)
  , ∀ x. (Ord x)            ⇒ Ord (d x)
  , ∀ x.                      ToIter (k ∧ x) (d x)
  , ∀ x.                      Single (k ∧ x) (d x)
  , ∀ x.                      Lookup k x (d x)
  , ∀ x.                      Null (d x)
  , ∀ x. (Append x)         ⇒ Append (d x)
  , ∀ x. (Monoid x)         ⇒ Monoid (d x)
  , ∀ x. (POrd x)           ⇒ POrd (d x)
  , ∀ x.                      Bot (d x)
  , ∀ x. (Join x)           ⇒ Join (d x)
  , ∀ x. (JoinLattice x)    ⇒ JoinLattice (d x)
  , ∀ x. (Meet x)           ⇒ Meet (d x)
  , ∀ x. (Difference x)     ⇒ Difference (d x)
  , ∀ x.                      Zero (d x)
  , ∀ x. (Plus x)           ⇒ Plus (d x)
  , ∀ x. (Additive x)       ⇒ Additive (d x)
  , ∀ x. (Zero x,Minus x)   ⇒ Minus (d x)
  ) ⇒ Dict k s d | d→k,d→s
  where
    dø ∷ d a
    dø = null
    (↦) ∷ k → a → d a
    (↦) = single ∘∘ (:*)
    dadd ∷ k → a → d a → d a
    dadd k x = (⩌) $ k ↦ x
    drem ∷ k → d a → d a
    drem k = okmapAt k $ const None
    dupd ∷ k → (a → 𝑂 a) → d a → d a
    dupd k f = okmapAt k $ extend f
    dlteBy ∷ (a → a → 𝔹) → d a → d a → 𝔹 -- NO DEFAULT
    dunionBy ∷ (a → a → a) → d a → d a → d a
    dunionBy f = bimap id id f
    dkunionBy ∷ (k → a → a → a) → d a → d a → d a
    dkunionBy f = kbimap (const id) (const id) f
    dinterBy ∷ (a → b → c) → d a → d b → d c
    dinterBy f = obimap (const None) (const None) $ Some ∘∘ f
    dsdiffBy ∷ (a → b → 𝑂 a) → d a → d b → d a
    dsdiffBy f = obimap Some (const None) f
    (⋿) ∷ ∀ a. k → d a → 𝔹
    k ⋿ d = case d ⋕? k of {None→False;Some (_∷a)→True}
    (⫑) ∷ (Eq a) ⇒ d a → d a → 𝔹
    (⫑) = dlteBy (≡)
    (⩌) ∷ d a → d a → d a
    (⩌) = dunionBy const
    (⩍) ∷ d a → d a → d a
    (⩍) = dinterBy const
    (⧅) ∷ (Eq a) ⇒ d a → d a → d a
    (⧅) = dsdiffBy $ \ x y → if x ≡ y then None else Some x
    dminView ∷ d a → 𝑂 (k ∧ a ∧ d a) -- NO DEFAULT
    dmaxView ∷ d a → 𝑂 (k ∧ a ∧ d a) -- NO DEFAULT
    dkeyView ∷ k → d a → 𝑂 (a ∧ d a) -- NO DEFAULT
    dminElem ∷ d a → 𝑂 (k ∧ a)
    dminElem = map fst ∘ dminView
    dmaxElem ∷ d a → 𝑂 (k ∧ a)
    dmaxElem = map fst ∘ dmaxView
    dkeep ∷ s → d a → d a
    dkeep s d = fold𝐼 dø (\ k → case d ⋕? k of {None → id;Some x → dadd k x}) $ iter s
    dtoss ∷ s → d a → d a
    dtoss s d = fold𝐼 d drem $ iter s
    dict𝐼 ∷ 𝐼 (k ∧ a) → d a
    dict𝐼 = foldr dø (⩌) ∘ map single
    dkeys ∷ ∀ a. d a → s
    dkeys = pow ∘ map fst ∘ iter @(k ∧ a)
    dvals ∷ ∀ a. d a → 𝐼 a
    dvals = map snd ∘ iter @(k ∧ a)

dunionByOn ∷ (Dict k s d) ⇒ d a → d a → (a → a → a) → d a
dunionByOn = rotateL dunionBy

dkunionByOn ∷ (Dict k s d) ⇒ d a → d a → (k → a → a → a) → d a
dkunionByOn = rotateL dkunionBy

dinterByOn ∷ (Dict k s d) ⇒ d a → d b → (a → b → c) → d c
dinterByOn = rotateL dinterBy

dinterByM ∷ (Monad m,Dict k s d) ⇒ (a → b → m c) → d a → d b → m (d c)
dinterByM f = obimapM (const $ return None) (const $ return None) $ map Some ∘∘ f

dict ∷ ∀ d t a k s. (Dict k s d,ToIter (d a) t) ⇒ t → d a
dict = foldr dø (⩌)

assoc ∷ ∀ d t a k s. (Dict k s d,ToIter (k ∧ a) t) ⇒ t → d a
assoc = foldr dø $ uncurry dadd

---------------------------
-- STANDARD DICT DATATYPE --
---------------------------

-- CLASS DEFINITIONS: Dict --

dø𝐷 ∷ ∀ k a. k ⇰ a
dø𝐷 = coerce @(Map.Map k a) Map.empty

(↦♭) ∷ ∀ k a. k → a → k ⇰ a
(↦♭) = coerce @(k → a → Map.Map k a) Map.singleton

dadd𝐷 ∷ ∀ k a. (Ord k) ⇒ k → a → k ⇰ a → k ⇰ a
dadd𝐷 = coerce @(k → a → Map.Map k a → Map.Map k a) Map.insert

drem𝐷 ∷ ∀ k a. (Ord k) ⇒ k → k ⇰ a → k ⇰ a
drem𝐷 = coerce @(k → Map.Map k a → Map.Map k a) Map.delete

dupd𝐷 ∷ ∀ k a. (Ord k) ⇒ k → (a → 𝑂 a) → k ⇰ a → k ⇰ a
dupd𝐷 = coerce @(k → (a → 𝑂 a) → Map.Map k a → Map.Map k a) $ \ k f → flip Map.update k $ tohs ∘ f

dlteBy𝐷 ∷ ∀ k a. (Ord k) ⇒ (a → a → 𝔹) → k ⇰ a → k ⇰ a → 𝔹
dlteBy𝐷 = coerce @((a → a → 𝔹) → Map.Map k a → Map.Map k a → 𝔹) Map.isSubmapOfBy

dunionBy𝐷 ∷ ∀ k a. (Ord k) ⇒ (a → a → a) → k ⇰ a → k ⇰ a → k ⇰ a
dunionBy𝐷 = coerce @((a → a → a) → Map.Map k a → Map.Map k a → Map.Map k a) Map.unionWith

dinterBy𝐷 ∷ ∀ k a b c. (Ord k) ⇒ (a → b → c) → k ⇰ a → k ⇰ b → k ⇰ c
dinterBy𝐷 = coerce @((a → b → c) → Map.Map k a → Map.Map k b → Map.Map k c) $ Map.intersectionWith

dsdiffBy𝐷 ∷ ∀ k b a. (Ord k) ⇒ (a → b → 𝑂 a) → k ⇰ a → k ⇰ b → k ⇰ a
dsdiffBy𝐷 = coerce @((a → b → 𝑂 a) → Map.Map k a → Map.Map k b → Map.Map k a) $ \ f → Map.differenceWith $ tohs ∘∘ f

(⋿♭) ∷ ∀ k a. (Ord k) ⇒ k → k ⇰ a → 𝔹
(⋿♭) = coerce @(k → Map.Map k a → 𝔹) Map.member

(⫑♭) ∷ ∀ k a. (Ord k,Eq a) ⇒ k ⇰ a → k ⇰ a → 𝔹
(⫑♭) = coerce @(Map.Map k a → Map.Map k a → 𝔹) Map.isSubmapOf

(⩌♭) ∷ ∀ k a. (Ord k) ⇒ k ⇰ a → k ⇰ a → k ⇰ a
(⩌♭) = coerce @(Map.Map k a → Map.Map k a → Map.Map k a) Map.union

(⩍♭) ∷ ∀ k a. (Ord k) ⇒ k ⇰ a → k ⇰ a → k ⇰ a
(⩍♭) = coerce @(Map.Map k a → Map.Map k a → Map.Map k a) Map.intersection

(⧅♭) ∷ ∀ k a. (Ord k,Eq a) ⇒ k ⇰ a → k ⇰ a → k ⇰ a
(⧅♭) = coerce @(Map.Map k a → Map.Map k a → Map.Map k a) $ Map.differenceWith $ \ x y →
  if x ≡ y then HS.Nothing else HS.Just x

dminView𝐷 ∷ ∀ k a. k ⇰ a → 𝑂 (k ∧ a ∧ (k ⇰ a))
dminView𝐷 = coerce @(Map.Map k a → 𝑂 (k ∧ a ∧ Map.Map k a)) $
  frhs ∘ Map.minViewWithKey

dmaxView𝐷 ∷ ∀ k a. k ⇰ a → 𝑂 (k ∧ a ∧ (k ⇰ a))
dmaxView𝐷 = coerce @(Map.Map k a → 𝑂 (k ∧ a ∧ Map.Map k a)) $
  frhs ∘ Map.maxViewWithKey

dkeyView𝐷 ∷ ∀ k a. (Ord k) ⇒ k → k ⇰ a → 𝑂 (a ∧ (k ⇰ a))
dkeyView𝐷 = coerce @(k → Map.Map k a → 𝑂 (a ∧ Map.Map k a)) $ \ k d →
  let xM :* d' = frhs $ Map.updateLookupWithKey (const $ const HS.Nothing) k d
  in map (:* d') xM

dminElem𝐷 ∷ ∀ k a. k ⇰ a → 𝑂 (k ∧ a)
dminElem𝐷 = coerce @(Map.Map k a → 𝑂 (k ∧ a)) $ frhs ∘ Map.lookupMin

dmaxElem𝐷 ∷ ∀ k a. k ⇰ a → 𝑂 (k ∧ a)
dmaxElem𝐷 = coerce @(Map.Map k a → 𝑂 (k ∧ a)) $ frhs ∘ Map.lookupMax

dkeep𝐷 ∷ ∀ k a. (Ord k) ⇒ 𝑃 k → k ⇰ a → k ⇰ a
dkeep𝐷 = coerce @(Set.Set k → Map.Map k a → Map.Map k a) $ flip Map.restrictKeys

dtoss𝐷 ∷ ∀ k a. (Ord k) ⇒ 𝑃 k → k ⇰ a → k ⇰ a
dtoss𝐷 = coerce @(Set.Set k → Map.Map k a → Map.Map k a) $ flip Map.withoutKeys

dict𝐼𝐷 ∷ ∀ k a. (Ord k) ⇒ 𝐼 (k ∧ a) → k ⇰ a
dict𝐼𝐷 = coerce @(Map.Map k a) ∘ Map.fromList ∘ lazyList ∘ map tohs

dkeys𝐷 ∷ ∀ k a. (Ord k) ⇒ k ⇰ a → 𝑃 k
dkeys𝐷 = coerce @(Map.Map k a → Set.Set k) Map.keysSet

dvals𝐷 ∷ ∀ k a. k ⇰ a → 𝐼 a
dvals𝐷 = coerce @(Map.Map k a → 𝐼 a) $ iter ∘ Map.elems

-- CLASS DEFINITIONS: FunctorM --

mapM𝐷 ∷ ∀ m k a b. (Monad m) ⇒ (a → m b) → k ⇰ a → m (k ⇰ b)
mapM𝐷 = with (tohsMonad @m) HS.$ with (fcoercibleW_UNSAFE @m) HS.$
  coerce @((a → m b) → Map.Map k a → m (Map.Map k b)) HS.mapM

-- CLASS DEFINITIONS: OFunctorM --

omapM𝐷 ∷ ∀ m k a b. (Monad m) ⇒ (a → m (𝑂 b)) → k ⇰ a → m (k ⇰ b)
omapM𝐷 = with (tohsMonad @m) HS.$ with (fcoercibleW_UNSAFE @m) HS.$
  coerce @((a → m (𝑂 b)) → Map.Map k a → m (Map.Map k b)) $ \ f →
    Map.traverseMaybeWithKey $ const $ map tohs ∘ f

-- CLASS DEFINITIONS: KFunctorM --

kmapM𝐷 ∷ ∀ m k a b. (Monad m) ⇒ (k → a → m b) → k ⇰ a → m (k ⇰ b)
kmapM𝐷 = with (tohsMonad @m) HS.$ with (fcoercibleW_UNSAFE @m) HS.$
  coerce @((k → a → m b) → Map.Map k a → m (Map.Map k b)) Map.traverseWithKey

kmapAtM𝐷 ∷ ∀ m k a. (Monad m,Ord k) ⇒ k → (a → m a) → k ⇰ a → m (k ⇰ a)
kmapAtM𝐷 = with (tohsMonad @m) HS.$ with (fcoercibleW_UNSAFE @m) HS.$
  coerce @(k → (a → m a) → Map.Map k a → m (Map.Map k a)) $ \ k f →
    flip Map.alterF k $ \case
      HS.Nothing → return HS.Nothing
      HS.Just x → HS.Just ^$ f x

-- CLASS DEFINITIONS: OKFunctorM --

okmapM𝐷 ∷ ∀ m k a b. (Monad m) ⇒ (k → a → m (𝑂 b)) → k ⇰ a → m (k ⇰ b)
okmapM𝐷 = with (tohsMonad @m) HS.$ with (fcoercibleW_UNSAFE @m) HS.$
  coerce @((k → a → m (𝑂 b)) → Map.Map k a → m (Map.Map k b)) $ \ f →
    Map.traverseMaybeWithKey $ map tohs ∘∘ f

okmapAtM𝐷 ∷ ∀ m k a. (Monad m,Ord k) ⇒ k → (𝑂 a → m (𝑂 a)) → k ⇰ a → m (k ⇰ a)
okmapAtM𝐷 = with (tohsMonad @m) HS.$ with (fcoercibleW_UNSAFE @m) HS.$
  coerce @(k → (𝑂 a → m (𝑂 a)) → Map.Map k a → m (Map.Map k a)) $ \ k f →
    flip Map.alterF k $ tohs ^∘ f ∘ frhs

-- CLASS DEFINITIONS: BiFunctorM --

bimapM𝐷 ∷ ∀ m k a b c. (Monad m,Ord k) ⇒ (a → m c) → (b → m c) → (a → b → m c) → k ⇰ a → k ⇰ b → m (k ⇰ c)
bimapM𝐷 = with (tohsMonad @m) HS.$ with (fcoercibleW_UNSAFE @m) HS.$
  coerce @((a → m c) → (b → m c) → (a → b → m c) → Map.Map k a → Map.Map k b → m (Map.Map k c)) $ \ f₁ f₂ f₃ →
    Map.mergeA (Map.traverseMissing $ const f₁)
               (Map.traverseMissing $ const f₂) $
               Map.zipWithAMatched $ const f₃

-- CLASS DEFINITIONS: OBiFunctorM --

obimapM𝐷 ∷ ∀ m k a b c. (Monad m,Ord k) ⇒ (a → m (𝑂 c)) → (b → m (𝑂 c)) → (a → b → m (𝑂 c)) → k ⇰ a → k ⇰ b → m (k ⇰ c)
obimapM𝐷 = with (tohsMonad @m) HS.$ with (fcoercibleW_UNSAFE @m) HS.$
  coerce @((a → m (𝑂 c)) → (b → m (𝑂 c)) → (a → b → m (𝑂 c)) → Map.Map k a → Map.Map k b → m (Map.Map k c)) $ \ f₁ f₂ f₃ →
    Map.mergeA (Map.traverseMaybeMissing $ const $ map tohs ∘ f₁)
               (Map.traverseMaybeMissing $ const $ map tohs ∘ f₂) $
               Map.zipWithMaybeAMatched $ const $ map tohs ∘∘ f₃

-- CLASS DEFINITIONS: KBiFunctorM --

kbimapM𝐷 ∷ ∀ m k a b c. (Monad m,Ord k) ⇒ (k → a → m c) → (k → b → m c) → (k → a → b → m c) → k ⇰ a → k ⇰ b → m (k ⇰ c)
kbimapM𝐷 = with (tohsMonad @m) HS.$ with (fcoercibleW_UNSAFE @m) HS.$
  coerce @((k → a → m c) → (k → b → m c) → (k → a → b → m c) → Map.Map k a → Map.Map k b → m (Map.Map k c)) $ \ f₁ f₂ f₃ →
    Map.mergeA (Map.traverseMissing f₁)
               (Map.traverseMissing f₂) $
               Map.zipWithAMatched f₃

-- CLASS DEFINITIONS: KBiFunctorM --

okbimapM𝐷 ∷ ∀ m k a b c. (Monad m,Ord k) ⇒ (k → a → m (𝑂 c)) → (k → b → m (𝑂 c)) → (k → a → b → m (𝑂 c)) → k ⇰ a → k ⇰ b → m (k ⇰ c)
okbimapM𝐷 = with (tohsMonad @m) HS.$ with (fcoercibleW_UNSAFE @m) HS.$
  coerce @((k → a → m (𝑂 c)) → (k → b → m (𝑂 c)) → (k → a → b → m (𝑂 c)) → Map.Map k a → Map.Map k b → m (Map.Map k c)) $ \ f₁ f₂ f₃ →
    Map.mergeA (Map.traverseMaybeMissing $ map tohs ∘∘ f₁)
               (Map.traverseMaybeMissing $ map tohs ∘∘ f₂) $
               Map.zipWithMaybeAMatched $ map tohs ∘∘∘ f₃

-- CLASS DEFINITIONS: Functor --

map𝐷 ∷ ∀ k a b. (a → b) → k ⇰ a → k ⇰ b
map𝐷 = coerce @((a → b) → Map.Map k a → Map.Map k b) Map.map

-- CLASS DEFINITIONS: OFunctor --

omap𝐷 ∷ ∀ k a b. (a → 𝑂 b) → k ⇰ a → k ⇰ b
omap𝐷 = coerce @((a → 𝑂 b) → Map.Map k a → Map.Map k b) $ \ f → Map.mapMaybe $ tohs ∘ f

-- CLASS DEFINITIONS: KFunctor --

kmap𝐷 ∷ ∀ k a b. (k → a → b) → k ⇰ a → k ⇰ b
kmap𝐷 = coerce @((k → a → b) → Map.Map k a → Map.Map k b) Map.mapWithKey

kmapAt𝐷 ∷ ∀ k a. (Ord k) ⇒ k → (a → a) → k ⇰ a → k ⇰ a
kmapAt𝐷 = coerce @(k → (a → a) → Map.Map k a → Map.Map k a) $ flip Map.adjust

-- CLASS DEFINITIONS: OKFunctor --

okmap𝐷 ∷ ∀ k a b. (k → a → 𝑂 b) → k ⇰ a → k ⇰ b
okmap𝐷 = coerce @((k → a → 𝑂 b) → Map.Map k a → Map.Map k b) $ \ f → Map.mapMaybeWithKey $ tohs ∘∘ f

okmapAt𝐷 ∷ ∀ k a. (Ord k) ⇒ k → (𝑂 a → 𝑂 a) → k ⇰ a → k ⇰ a
okmapAt𝐷 = coerce @(k → (𝑂 a → 𝑂 a) → Map.Map k a → Map.Map k a) $ \ k f → flip Map.alter k $ tohs ∘ f ∘ frhs

-- CLASS DEFINITIONS: BiFunctor --

bimap𝐷 ∷ ∀ k a b c. (Ord k) ⇒ (a → c) → (b → c) → (a → b → c) → k ⇰ a → k ⇰ b → k ⇰ c
bimap𝐷 = coerce @((a → c) → (b → c) → (a → b → c) → Map.Map k a → Map.Map k b → Map.Map k c) $ \ f₁ f₂ f₃ →
  Map.merge (Map.mapMissing $ const f₁)
            (Map.mapMissing $ const f₂) $
             Map.zipWithMatched $ const f₃

-- CLASS DEFINITIONS: OBiFunctor --

obimap𝐷 ∷ ∀ k a b c. (Ord k) ⇒ (a → 𝑂 c) → (b → 𝑂 c) → (a → b → 𝑂 c) → k ⇰ a → k ⇰ b → k ⇰ c
obimap𝐷 = coerce @((a → 𝑂 c) → (b → 𝑂 c) → (a → b → 𝑂 c) → Map.Map k a → Map.Map k b → Map.Map k c) $ \ f₁ f₂ f₃ →
  Map.merge (Map.mapMaybeMissing $ const $ tohs ∘ f₁)
            (Map.mapMaybeMissing $ const $ tohs ∘ f₂) $
             Map.zipWithMaybeMatched $ const $ tohs ∘∘ f₃

-- CLASS DEFINITIONS: KBiFunctor --

kbimap𝐷 ∷ ∀ k a b c. (Ord k) ⇒ (k → a → c) → (k → b → c) → (k → a → b → c) → k ⇰ a → k ⇰ b → k ⇰ c
kbimap𝐷 = coerce @((k → a → c) → (k → b → c) → (k → a → b → c) → Map.Map k a → Map.Map k b → Map.Map k c) $ \ f₁ f₂ f₃ →
  Map.merge (Map.mapMissing f₁)
            (Map.mapMissing f₂) $
             Map.zipWithMatched f₃

-- CLASS DEFINITIONS: OKBiFunctor --

okbimap𝐷 ∷ ∀ k a b c. (Ord k) ⇒ (k → a → 𝑂 c) → (k → b → 𝑂 c) → (k → a → b → 𝑂 c) → k ⇰ a → k ⇰ b → (k ⇰ c)
okbimap𝐷 = coerce @((k → a → 𝑂 c) → (k → b → 𝑂 c) → (k → a → b → 𝑂 c) → Map.Map k a → Map.Map k b → Map.Map k c) $ \ f₁ f₂ f₃ →
  Map.merge (Map.mapMaybeMissing $ tohs ∘∘ f₁)
            (Map.mapMaybeMissing $ tohs ∘∘ f₂) $
             Map.zipWithMaybeMatched $ tohs ∘∘∘ f₃

-- CLASS DEFINITIONS: CSized --

csize𝐷 ∷ ∀ k a. k ⇰ a → ℕ64
csize𝐷 = coerce @(Map.Map k a → ℕ64) $ natΩ64 ∘ frhs ∘ Map.size

-- CLASS DEFINITIONS: Show --

show𝐷 ∷ (Show k,Show a) ⇒ k ⇰ a → 𝕊
show𝐷 = showCollection "{" "}" "," $ \ (k :* x) → show𝕊 k ⧺ "⇒" ⧺ show𝕊 x

-- CLASS DEFINITIONS: ToIter --

iter𝐷 ∷ ∀ k a. k ⇰ a → 𝐼 (k ∧ a)
iter𝐷 = coerce @(Map.Map k a → 𝐼 (k ∧ a)) $ map frhs ∘ iterLL ∘ Map.toList

-- CLASS DEFINITIONS: Single --

single𝐷 ∷ k ∧ a → k ⇰ a
single𝐷 = uncurry (↦♭)

-- CLASS DEFINITIONS: Lookup --

lookup𝐷 ∷ ∀ k a. (Ord k) ⇒ k ⇰ a → k → 𝑂 a
lookup𝐷 = coerce @(Map.Map k a → k → 𝑂 a) $ \ d k → frhs $ d Map.!? k

-- CLASS DEFINITIONS: Monoid --

null𝐷 ∷ k ⇰ a
null𝐷 = dø𝐷

append𝐷 ∷ (Ord k,Append a) ⇒ k ⇰ a → k ⇰ a → k ⇰ a
append𝐷 = dunionBy𝐷 (⧺)

-- CLASS DEFINITIONS: Prodoid --

unit𝐷 ∷ (Ord k,Null k,Null a) ⇒ k ⇰ a
unit𝐷 = null ↦♭ null

cross𝐷 ∷ (Ord k,Append k,Append a,Cross a) ⇒ k ⇰ a → k ⇰ a → k ⇰ a
cross𝐷 d₁ d₂ = foldr dø𝐷 (dunionBy𝐷 (⧺)) $ do
  (k₁ :* x₁) ← iter d₁
  (k₂ :* x₂) ← iter d₂
  return $ (k₁ ⧺ k₂) ↦♭ (x₁ ⨳ x₂)

-- CLASS DEFINITIONS: Additive --

zero𝐷 ∷ k ⇰ a
zero𝐷 = dø𝐷

plus𝐷 ∷ (Ord k,Plus a) ⇒ k ⇰ a → k ⇰ a → k ⇰ a
plus𝐷 = dunionBy𝐷 (+)

-- CLASS DEFINITIONS: Minus --

minus𝐷 ∷ (Ord k,Zero a,Minus a) ⇒ k ⇰ a → k ⇰ a → k ⇰ a
minus𝐷 = bimap id (\ x → zero - x) (-)

-- CLASS DEFINITIONS: POrd --

plte𝐷 ∷ (Ord k,POrd a) ⇒ k ⇰ a → k ⇰ a → 𝔹
plte𝐷 = dlteBy𝐷 (⊑)

-- CLASS DEFINITIONS: Lattice --

bot𝐷 ∷ k ⇰ a
bot𝐷 = dø𝐷

join𝐷 ∷ (Ord k,Join a) ⇒ k ⇰ a → k ⇰ a → k ⇰ a
join𝐷 = dunionBy𝐷 (⊔)

meet𝐷 ∷ (Ord k,Meet a) ⇒ k ⇰ a → k ⇰ a → k ⇰ a
meet𝐷 = dinterBy𝐷 (⊓)

diff𝐷 ∷ (Ord k,Difference a) ⇒ k ⇰ a → k ⇰ a → k ⇰ a
diff𝐷 = dsdiffBy𝐷 $ Some ∘∘ (⊟)

-- CLASS DEFINITIONS: All --

all𝐷 ∷ (Ord k,All k,All a) ⇒ 𝐼 (k ⇰ a)
all𝐷 = foldrOnFrom all (return dø𝐷) $ \ k dsᵢ → do
    d ← map (k ↦♭) all
    dᵢ ← dsᵢ
    return $ d ⩌♭ dᵢ

-- OTHER DEFINITIONS --

dict𝐷 ∷ (Ord k,ToIter (k ⇰ a) t) ⇒ t → k ⇰ a
dict𝐷 = dict

assoc𝐷 ∷ (Ord k,ToIter (k ∧ a) t) ⇒ t → k ⇰ a
assoc𝐷 = assoc

dminKey𝐷 ∷ k ⇰ a → 𝑂 k
dminKey𝐷 = map fst ∘ dminElem𝐷

dmaxKey𝐷 ∷ k ⇰ a → 𝑂 k
dmaxKey𝐷 = map fst ∘ dmaxElem𝐷

---------------
-- INSTANCES --
---------------

instance                                       FunctorM       ((⇰) k) where mapM     = mapM𝐷
instance                                       OFunctorM      ((⇰) k) where omapM    = omapM𝐷
instance (Ord k)                             ⇒ KFunctorM    k ((⇰) k) where kmapM    = kmapM𝐷  ; kmapAtM  = kmapAtM𝐷
instance (Ord k)                             ⇒ OKFunctorM   k ((⇰) k) where okmapM   = okmapM𝐷 ; okmapAtM = okmapAtM𝐷
instance (Ord k)                             ⇒ BiFunctorM     ((⇰) k) where bimapM   = bimapM𝐷
instance (Ord k)                             ⇒ OBiFunctorM    ((⇰) k) where obimapM  = obimapM𝐷
instance (Ord k)                             ⇒ KBiFunctorM  k ((⇰) k) where kbimapM  = kbimapM𝐷
instance (Ord k)                             ⇒ OKBiFunctorM k ((⇰) k) where okbimapM = okbimapM𝐷
instance                                       Functor        ((⇰) k) where map      = map𝐷
instance                                       OFunctor       ((⇰) k) where omap     = omap𝐷
instance (Ord k)                             ⇒ KFunctor     k ((⇰) k) where kmap     = kmap𝐷  ; kmapAt  = kmapAt𝐷
instance (Ord k)                             ⇒ OKFunctor    k ((⇰) k) where okmap    = okmap𝐷 ; okmapAt = okmapAt𝐷
instance (Ord k)                             ⇒ BiFunctor      ((⇰) k) where bimap    = bimap𝐷
instance (Ord k)                             ⇒ OBiFunctor     ((⇰) k) where obimap   = obimap𝐷
instance (Ord k)                             ⇒ KBiFunctor   k ((⇰) k) where kbimap   = kbimap𝐷
instance (Ord k)                             ⇒ OKBiFunctor  k ((⇰) k) where okbimap  = okbimap𝐷
instance                                       CSized         (k ⇰ a) where csize    = csize𝐷
instance (Show k,Show a)                     ⇒ Show           (k ⇰ a) where show     = tohsChars ∘ show𝐷
instance                                       ToIter (k ∧ a) (k ⇰ a) where iter     = iter𝐷
instance (Ord k)                             ⇒ Single (k ∧ a) (k ⇰ a) where single   = single𝐷
instance (Ord k)                             ⇒ Lookup k a     (k ⇰ a) where (⋕?)     = lookup𝐷
instance                                       Null           (k ⇰ a) where null     = dø𝐷
instance (Ord k,Append a)                    ⇒ Append         (k ⇰ a) where (⧺)      = append𝐷
instance (Ord k,Append a)                    ⇒ Monoid         (k ⇰ a)
instance (Ord k,Null k,Null a)               ⇒ Unit           (k ⇰ a) where unit     = unit𝐷
instance (Ord k,Append k,Append a,Cross a)   ⇒ Cross          (k ⇰ a) where (⨳)      = cross𝐷
instance (Ord k,Monoid k,Prodoid a)          ⇒ Prodoid        (k ⇰ a)
instance                                       Zero           (k ⇰ a) where zero     = zero𝐷
instance (Ord k,Plus a)                      ⇒ Plus           (k ⇰ a) where (+)      = plus𝐷
instance (Ord k,Plus a)                      ⇒ Additive       (k ⇰ a)
instance (Ord k,Zero a,Minus a)              ⇒ Minus          (k ⇰ a) where (-)      = minus𝐷
instance (Ord k,POrd a)                      ⇒ POrd           (k ⇰ a) where (⊑)      = plte𝐷
instance                                       Bot            (k ⇰ a) where bot      = bot𝐷
instance (Ord k,Join a)                      ⇒ Join           (k ⇰ a) where (⊔)      = join𝐷
instance (Ord k,Join a)                      ⇒ JoinLattice    (k ⇰ a)
instance (Ord k,Meet a)                      ⇒ Meet           (k ⇰ a) where (⊓)      = meet𝐷
instance (Ord k,Difference a)                ⇒ Difference     (k ⇰ a) where (⊟)      = diff𝐷
instance (Ord k,All k,All a)                 ⇒ All            (k ⇰ a) where all      = all𝐷

instance (Ord k) ⇒ Dict k (𝑃 k) ((⇰) k) where
    dø       = dø𝐷
    (↦)      = (↦♭)
    dadd     = dadd𝐷
    drem     = drem𝐷
    dupd     = dupd𝐷
    dlteBy   = dlteBy𝐷
    dunionBy = dunionBy𝐷
    dinterBy = dinterBy𝐷
    dsdiffBy = dsdiffBy𝐷
    (⋿)      = (⋿♭)
    (⫑)      = (⫑♭)
    (⩌)      = (⩌♭)
    (⩍)      = (⩍♭)
    (⧅)      = (⧅♭)
    dminView = dminView𝐷
    dmaxView = dmaxView𝐷
    dkeyView = dkeyView𝐷
    dminElem = dminElem𝐷
    dmaxElem = dmaxElem𝐷
    dkeep    = dkeep𝐷
    dtoss    = dtoss𝐷
    dict𝐼    = dict𝐼𝐷
    dkeys    = dkeys𝐷
    dvals    = dvals𝐷

---------------------
-- ESD ABSTRACTION --
---------------------

data family Elem ∷ ★ → ★
data family ESet ∷ ★ → ★
data family EDct ∷ ★ → ★ → ★

class
  ( Set (Elem p) (ESet p)
  , Dict (Elem p) (ESet p) (EDct p)
  ) ⇒ ESD p

data StdESD (x ∷ ★)

newtype instance Elem (StdESD a) = StdESDElm { unStdESDElm ∷ a }
  deriving (Eq,Ord)
newtype instance ESet (StdESD e) = StdESDSet { unStdESDSet ∷ 𝑃 e }
  deriving
  ( CSized,Eq,Ord
  , Null,Append,Monoid
  , POrd
  , Bot,Join,JoinLattice,Meet,Difference
  )
newtype instance EDct (StdESD k) a = StdESDDct { unStdESDDct ∷ k ⇰ a }
  deriving
  ( Eq,Ord
  , Null,Append,Monoid
  , Bot,Join,JoinLattice
  -- , Single (Elem (StdESD k) ∧ a)
  -- , ToIter (Elem (StdESD k) ∧ a)
  -- , Lookup (Elem (StdESD k)) a
  , Functor
  )

instance (Ord e) ⇒ ToIter (Elem (StdESD e)) (ESet (StdESD e)) where
  iter ∷ ESet (StdESD e) → 𝐼 (Elem (StdESD e))
  iter = coerce @(𝑃 e → 𝐼 e) iter
instance (Ord e) ⇒ Single (Elem (StdESD e)) (ESet (StdESD e)) where
  single = coerce @(e → 𝑃 e) single
instance (Ord e) ⇒ Set (Elem (StdESD e)) (ESet (StdESD e)) where
  pø       = coerce @(𝑃 e)               pø
  psingle  = coerce @(e → 𝑃 e)           single
  padd     = coerce @(e → 𝑃 e → 𝑃 e)     padd
  prem     = coerce @(e → 𝑃 e → 𝑃 e)     prem
  (∈)      = coerce @(e → 𝑃 e → 𝔹)       (∈)
  (⊆)      = coerce @(𝑃 e → 𝑃 e → 𝔹)     (⊆)
  (∪)      = coerce @(𝑃 e → 𝑃 e → 𝑃 e)   (∪)
  (∩)      = coerce @(𝑃 e → 𝑃 e → 𝑃 e)   (∩)
  (∖)      = coerce @(𝑃 e → 𝑃 e → 𝑃 e)   (∖)
  pminView = coerce @(𝑃 e → 𝑂 (e ∧ 𝑃 e)) pminView
  pmaxView = coerce @(𝑃 e → 𝑂 (e ∧ 𝑃 e)) pmaxView
  pminElem = coerce @(𝑃 e → 𝑂 e)         pminElem
  pmaxElem = coerce @(𝑃 e → 𝑂 e)         pmaxElem
  pow𝐼     = coerce @(𝐼 e → 𝑃 e)         pow𝐼
  pvals    = coerce @(𝑃 e → 𝐼 e)         pvals

instance (Ord k) ⇒ FunctorM (EDct (StdESD k)) where
  mapM ∷ ∀ m a b. (Monad m) ⇒ (a → m b) → EDct (StdESD k) a → m (EDct (StdESD k) b)
  mapM = with (fcoercibleW_UNSAFE @m) HS.$ coerce @((a → m b) → k ⇰ a → m (k ⇰ b)) mapM
instance (Ord k) ⇒ Single (Elem (StdESD k) ∧ a) (EDct (StdESD k) a) where
  single = coerce @((k ∧ a) → k ⇰ a) single

-- instance ToIter (Elem (StdESD k) ∧ a) (EDct (StdESD k) a) where
--   iter = coerce @(𝐼 (k ∧ a)) @(𝐼 (Elem (StdESD k) ∧ a)) ∘ iter ∘ coerce @(EDct (StdESD k) a) @(k ⇰ a)
-- instance (Ord k) ⇒ Lookup (Elem (StdESD k)) a (EDct (StdESD k) a) where
--   kvs ⋕? k = coerce @(EDct (StdESD k) a) @(k ⇰ a) kvs ⋕? coerce @(Elem (StdESD k)) @k k
-- instance (Ord k,Show k) ⇒ GDict (Elem (StdESD k)) (ESet (StdESD k)) (EDct (StdESD k)) where
--   (⩌♯) ∷ ∀ a. EDct (StdESD k) a → EDct (StdESD k) a → EDct (StdESD k) a
--   (⩌♯) = coerce @(k ⇰ a → k ⇰ a → k ⇰ a) (⩌)
--   gkeys ∷ ∀ a. EDct (StdESD k) a → ESet (StdESD k)
--   gkeys = coerce @(k ⇰ a → 𝑃 k) keys
--   gkeep ∷ ∀ a. ESet (StdESD k) → EDct (StdESD k) a → EDct (StdESD k) a
--   gkeep = coerce @(𝑃 k → k ⇰ a → k ⇰ a) restrict
--   gtoss ∷ ∀ a. ESet (StdESD k) → EDct (StdESD k) a → EDct (StdESD k) a
--   gtoss = coerce @(𝑃 k → k ⇰ a → k ⇰ a) without
--   gmapWithKeyM ∷ ∀ m a b. (Monad m,FCoercibleRefl m) ⇒ (Elem (StdESD k) → a → m b) → EDct (StdESD k) a → m (EDct (StdESD k) b)
--   gmapWithKeyM = coerce @((k → a → m b) → k ⇰ a → m (k ⇰ b)) mapMWithKey
--   gbimapWithKeyM ∷ ∀ m a b c. (Monad m,FCoercibleRefl m) ⇒ (k → a → m c) → (k → b → m c) → (Elem (StdESD k) → a → b → m c) → EDct (StdESD k) a → EDct (StdESD k) b → m (EDct (StdESD k) c)
--   gbimapWithKeyM = coerce @((k → a → m c) → (k → b → m c) → (k → a → b → m c) → k ⇰ a → k ⇰ b → m (k ⇰ c)) bimapWithKeyM




-- SHOW INSTANCES --


-- CONTAINER INSTANCES --

-- PARTIAL ORDER INSTANCES --

-- MONOID INSTANCES --

-- PRODOID INSTANCES --

-- ADDITIVE INSTANCES --


-- LATTICE INSTNACES --

-- FUNCTOR INSTANCES --


-- ALL INSTANCES --

-- =========== --
-- DEFINITIONS --
-- =========== --

-- PRIMITIVES --


-- unionWithOn𝐷 ∷ (Ord k) ⇒ k ⇰ a → k ⇰ a → (a → a → a) → k ⇰ a
-- unionWithOn𝐷 = rotateL unionWith𝐷
--
-- unionsWith𝐷 ∷ (Ord k,ToIter (k ⇰ a) t) ⇒ (a → a → a) → t → k ⇰ a
-- unionsWith𝐷 = fold dø𝐷 ∘ unionWith𝐷
--
-- kunionWith𝐷 ∷ ∀ k a. (Ord k) ⇒ (k → a → a → a) → k ⇰ a → k ⇰ a → k ⇰ a
-- kunionWith𝐷 = coerce @((k → a → a → a) → Map.Map k a → Map.Map k a → Map.Map k a) Map.unionWithKey
--
-- kunionWithOn𝐷 ∷ (Ord k) ⇒ k ⇰ a → k ⇰ a → (k → a → a → a) → k ⇰ a
-- kunionWithOn𝐷 = rotateL kunionWith𝐷
--
-- kunionsWith𝐷 ∷ (Ord k,ToIter (k ⇰ a) t) ⇒ (k → a → a → a) → t → k ⇰ a
-- kunionsWith𝐷 = fold dø𝐷 ∘ kunionWith𝐷
--
-- interWithOn𝐷 ∷ (Ord k) ⇒ k ⇰ a → k ⇰ b → (a → b → c) → k ⇰ c
-- interWithOn𝐷 = rotateL interWith𝐷
--
-- intersWith𝐷 ∷ (Ord k,ToIter (k ⇰ a) t) ⇒ (a → a → a) → t → k ⇰ a
-- intersWith𝐷 = fold dø𝐷 ∘ interWith𝐷
--
-- diffnWithOn𝐷 ∷ (Ord k) ⇒ k ⇰ a → k ⇰ a → (a → a → a) → k ⇰ a
-- diffnWithOn𝐷 = rotateL diffnWith𝐷
--
-- diffnsWith𝐷 ∷ (Ord k,ToIter (k ⇰ a) t) ⇒ (a → a → a) → t → k ⇰ a
-- diffnsWith𝐷 = foldr dø𝐷 ∘ diffnWith𝐷

-- (⊎) ∷ (Ord k,Additive a) ⇒ k ⇰ a → k ⇰ a → k ⇰ a
-- (⊎) = unionWith𝐷 (+)

-- dict ∷ (Ord k,ToIter (k ⇰ a) t) ⇒ t → k ⇰ a
-- dict = foldr dø𝐷 (⩌♭) ∘ iter

-- assoc ∷ (Ord k,ToIter (k ∧ a) t) ⇒ t → k ⇰ a
-- assoc = dict ∘ map single ∘ iter

--- mapOnKeyWith ∷ (Ord k) ⇒ (a → a) → k → k ⇰ a → k ⇰ a
--- mapOnKeyWith f k = 𝐷 ∘ Map.adjust f k ∘ un𝐷
---
--- mapOnKey ∷ (Ord k) ⇒ k → (a → a) → k ⇰ a → k ⇰ a
--- mapOnKey = flip mapOnKeyWith

-- interWithM ∷ (Monad m,Ord k) ⇒ (v₁ → v₂ → m v₃) → k ⇰ v₁ → k ⇰ v₂ → m (k ⇰ v₃)
-- interWithM f kvs₁ kvs₂ = exchange $ interWith f kvs₁ kvs₂

-- interWithMOn ∷ (Monad m,Ord k) ⇒ k ⇰ v₁ → k ⇰ v₂ → (v₁ → v₂ → m v₃) → m (k ⇰ v₃)
-- interWithMOn = rotateL interWithM

-- djoin ∷ (Ord k,Ord v₁,Ord v₂) ⇒ k ⇰ 𝑃 v₁ → k ⇰ 𝑃 v₂ → k ⇰ 𝑃 (v₁ ∧ v₂)
-- djoin = interWith $ \ vs₁ vs₂ → pow $ zipWith (:*) vs₁ vs₂
