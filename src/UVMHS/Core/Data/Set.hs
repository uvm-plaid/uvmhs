module UVMHS.Core.Data.Set where

import UVMHS.Core.Init

import UVMHS.Core.Classes

import UVMHS.Core.Data.Iter
import UVMHS.Core.Data.Pair
import UVMHS.Core.Data.String

import qualified Data.Set as Set

infix 4 ⊆,⊇,∈,∉
infixl 5 ∪,∖
infixl 6 ∩

-------------------
-- GENERIC CLASS --
-------------------

class
  ( CSized s
  , Ord s
  , ToIter e s
  , Single e s
  , Monoid s
  , POrd s
  , JoinLattice s
  , Meet s
  , Difference s
  ) ⇒ Set e s | s→e
  where
    pø ∷ s
    pø = null
    psingle ∷ e → s
    psingle = single
    padd ∷ e → s → s
    padd e s = single e ∪ s
    prem ∷ e → s → s
    prem e s = s ∖ single e
    (∈) ∷ e → s → 𝔹
    (∈) e = (⊆) $ single e
    (⊆) ∷ s → s → 𝔹
    (⊆) = (⊑)
    (∪) ∷ s → s → s
    (∪) = (⊔)
    (∩) ∷ s → s → s
    (∩) = (⊓)
    (∖) ∷ s → s → s
    (∖) = (⊟)
    pminView ∷ s → 𝑂 (e ∧ s) -- NO DEFAULT
    pmaxView ∷ s → 𝑂 (e ∧ s) -- NO DEFAULT
    pminElem ∷ s → 𝑂 e
    pminElem = map fst ∘ pminView
    pmaxElem ∷ s → 𝑂 e
    pmaxElem = map fst ∘ pmaxView
    pow𝐼 ∷ 𝐼 e → s
    pow𝐼 = fold𝐼 pø (∪) ∘ map single
    pvals ∷ s → 𝐼 e
    pvals = iter

(⊇) ∷ (Set e s) ⇒ s → s → 𝔹
(⊇) = flip (⊆)

(∉) ∷ (Set e s) ⇒ e → s → 𝔹
(∉) = not ∘∘ (∈)

pow ∷ ∀ s t e. (ToIter e t,Set e s) ⇒ t → s
pow = pow𝐼 ∘ iter

unions ∷ (Set e s,ToIter s t) ⇒ t → s
unions ss = foldOnFrom ss pø (∪)

intersFrom ∷ (Set e s,ToIter s t) ⇒ s → t → s
intersFrom s ss = foldOnFrom ss s (∩)

sdiffsFrom ∷ (Set e s,ToIter s t) ⇒ s → t → s
sdiffsFrom s ss = foldOnFrom ss s (∖)

---------------------------
-- STANDARD SET DATATYPE --
---------------------------

-- CLASS DEFINITIONS: Set --

pø𝑃 ∷ 𝑃 a
pø𝑃 = coerce Set.empty

psingle𝑃 ∷ a → 𝑃 a
psingle𝑃 = coerce Set.singleton

padd𝑃 ∷ (Ord a) ⇒ a → 𝑃 a → 𝑃 a
padd𝑃 = coerce Set.insert

prem𝑃 ∷ (Ord a) ⇒ a → 𝑃 a → 𝑃 a
prem𝑃 = coerce Set.delete

(∈♭) ∷ (Ord a) ⇒ a → 𝑃 a → 𝔹
(∈♭) = coerce Set.member

(⊆♭) ∷ (Ord a) ⇒ 𝑃 a → 𝑃 a → 𝔹
(⊆♭) = coerce Set.isSubsetOf

(∪♭) ∷ (Ord a) ⇒ 𝑃 a → 𝑃 a → 𝑃 a
(∪♭) = coerce Set.union

(∩♭) ∷ (Ord a) ⇒ 𝑃 a → 𝑃 a → 𝑃 a
(∩♭) = coerce Set.intersection

(∖♭) ∷ (Ord a) ⇒ 𝑃 a → 𝑃 a → 𝑃 a
(∖♭) = coerce Set.difference

pminView𝑃 ∷ ∀ a. 𝑃 a → 𝑂 (a ∧ 𝑃 a)
pminView𝑃 = coerce @(Set.Set a → 𝑂 (a ∧ Set.Set a)) $ frhs ∘ Set.minView

pmaxView𝑃 ∷ ∀ a. 𝑃 a → 𝑂 (a ∧ 𝑃 a)
pmaxView𝑃 = coerce @(Set.Set a → 𝑂 (a ∧ Set.Set a)) $ frhs ∘ Set.maxView

pminElem𝑃 ∷ ∀ a. 𝑃 a → 𝑂 a
pminElem𝑃 = coerce @(Set.Set a → 𝑂 a) $ frhs ∘ Set.lookupMin

pmaxElem𝑃 ∷ ∀ a. 𝑃 a → 𝑂 a
pmaxElem𝑃 = coerce @(Set.Set a → 𝑂 a) $ frhs ∘ Set.lookupMax

pow𝐼𝑃 ∷ (Ord a) ⇒ 𝐼 a → 𝑃 a
pow𝐼𝑃 = 𝑃 ∘ Set.fromList ∘ lazyList

pvals𝑃 ∷ ∀ a. 𝑃 a → 𝐼 a
pvals𝑃 = coerce $ iterLL ∘ Set.toList

-- CLASS DEFINITIONS: CSized --

csize𝑃 ∷ 𝑃 a → ℕ64
csize𝑃 = coerce $ natΩ64 ∘ frhs ∘ Set.size

-- CLASS DEFINITIONS: Show --

show𝑃 ∷ (Show a) ⇒ 𝑃 a → 𝕊
show𝑃 = showCollection "{" "}" "," show𝕊

-- CLASS DEFINITIONS: ToIter --

iter𝑃 ∷ 𝑃 a → 𝐼 a
iter𝑃 = pvals𝑃

-- CLASS DEFINITIONS: Single --

single𝑃 ∷ a → 𝑃 a
single𝑃 = psingle𝑃

-- CLASS DEFINITIONS: Monoid --

null𝑃 ∷ 𝑃 a
null𝑃 = pø𝑃

append𝑃 ∷ (Ord a) ⇒ 𝑃 a → 𝑃 a → 𝑃 a
append𝑃 = (∪♭)

-- CLASS DEFINITIONS: Prodoid --

unit𝑃 ∷ (Ord a,Null a) ⇒ 𝑃 a
unit𝑃 = single𝑃 null

cross𝑃 ∷ (Ord a,Append a) ⇒ 𝑃 a → 𝑃 a → 𝑃 a
cross𝑃 xs ys = pow𝐼𝑃 $ do
  x ← iter xs
  y ← iter ys
  return $ x ⧺ y

-- CLASS DEFINITIONS: Additive --

zero𝑃 ∷ 𝑃 a
zero𝑃 = pø𝑃

plus𝑃 ∷ (Ord a) ⇒ 𝑃 a → 𝑃 a → 𝑃 a
plus𝑃 = (∪♭)

-- CLASS DEFINITIONS: Multiplicative --

one𝑃 ∷ (Ord a,Zero a) ⇒ 𝑃 a
one𝑃 = single𝑃 zero

times𝑃 ∷ (Ord a,Plus a) ⇒ 𝑃 a → 𝑃 a → 𝑃 a
times𝑃 xs ys = pow𝐼𝑃 $ do
  x ← iter xs
  y ← iter ys
  return $ x + y

-- CLASS DEFINITIONS: POrd --

plte𝑃 ∷ (Ord a) ⇒ 𝑃 a → 𝑃 a → 𝔹
plte𝑃 = (⊆♭)

-- CLASS DEFINITIONS: Lattice

bot𝑃 ∷ 𝑃 a
bot𝑃 = pø𝑃

join𝑃 ∷ (Ord a) ⇒ 𝑃 a → 𝑃 a → 𝑃 a
join𝑃 = (∪♭)

meet𝑃 ∷ (Ord a) ⇒ 𝑃 a → 𝑃 a → 𝑃 a
meet𝑃 = (∩♭)

diff𝑃 ∷ (Ord a) ⇒ 𝑃 a → 𝑃 a → 𝑃 a
diff𝑃 = (∖♭)

-- CLASS DEFINITIONS: All --

all𝑃 ∷ (Ord a,All a) ⇒ 𝐼 (𝑃 a)
all𝑃 = foldrOnFrom all (single pø𝑃) $ \ x xssᵢ → do
  xs ← xssᵢ
  iter $ [ xs , single x ∪♭ xs ]

-- OTHER DEFINITIONS --

pow𝑃 ∷ (Ord a,ToIter a t) ⇒ t → 𝑃 a
pow𝑃 = pow𝐼𝑃 ∘ iter

map𝑃 ∷ (Ord b) ⇒ (a → b) → 𝑃 a → 𝑃 b
map𝑃 = coerce Set.map

uniques𝑃 ∷ (Ord a,ToIter a t) ⇒ t → 𝐼 a
uniques𝑃 xs = filterMap id $ appto (iter xs) $ reiter pø𝑃 $ \ x seen →
  if x ∈♭ seen 
  then seen :* None 
  else (single𝑃 x ∪♭ seen) :* Some x

---------------
-- INSTANCES --
---------------

instance                    CSized      (𝑃 a) where csize  = csize𝑃
instance (Show a)         ⇒ Show        (𝑃 a) where show   = tohsChars ∘ show𝑃
instance                    ToIter a    (𝑃 a) where iter   = iter𝑃
instance (Ord a)          ⇒ Single a    (𝑃 a) where single = single𝑃
instance                    Null        (𝑃 a) where null   = null𝑃
instance (Ord a)          ⇒ Append      (𝑃 a) where (⧺)    = append𝑃
instance (Ord a)          ⇒ Monoid      (𝑃 a)
instance (Ord a,Null a)   ⇒ Unit        (𝑃 a) where unit   = unit𝑃
instance (Ord a,Append a) ⇒ Cross       (𝑃 a) where (⨳)    = cross𝑃
instance (Ord a,Monoid a) ⇒ Prodoid     (𝑃 a)
instance                    Zero        (𝑃 a) where zero   = zero𝑃
instance (Ord a)          ⇒ Plus        (𝑃 a) where (+)    = plus𝑃
instance (Ord a)          ⇒ Additive    (𝑃 a)
instance (Ord a,Zero a)   ⇒ One         (𝑃 a) where one    = one𝑃
instance (Ord a,Plus a)   ⇒ Times       (𝑃 a) where (×)    = times𝑃
instance (Ord a)          ⇒ POrd        (𝑃 a) where (⊑)    = plte𝑃
instance                    Bot         (𝑃 a) where bot    = bot𝑃
instance (Ord a)          ⇒ Join        (𝑃 a) where (⊔)    = join𝑃
instance (Ord a)          ⇒ JoinLattice (𝑃 a)
instance (Ord a)          ⇒ Meet        (𝑃 a) where (⊓)    = meet𝑃
instance (Ord a)          ⇒ Difference  (𝑃 a) where (⊟)    = diff𝑃
instance (Ord a,All a)    ⇒ All         (𝑃 a) where all    = all𝑃

instance (Ord e) ⇒ Set e (𝑃 e) where
  pø       = pø𝑃
  psingle  = psingle𝑃
  padd     = padd𝑃
  prem     = prem𝑃
  (∈)      = (∈♭) 
  (⊆)      = (⊆♭)
  (∪)      = (∪♭)
  (∩)      = (∩♭)
  (∖)      = (∖♭)
  pminView = pminView𝑃
  pmaxView = pmaxView𝑃
  pminElem = pminElem𝑃
  pmaxElem = pmaxElem𝑃
  pow𝐼     = pow𝐼𝑃
  pvals    = pvals𝑃
