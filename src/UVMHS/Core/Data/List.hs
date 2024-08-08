module UVMHS.Core.Data.List where

import UVMHS.Core.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data.LazyList ()
import UVMHS.Core.Data.Arithmetic ()

instance Lookup ℕ64 a (𝐿 a) where (⋕?)   = flip lookup𝐿
instance Single     a (𝐿 a) where single = single𝐿
instance ToIter     a (𝐿 a) where iter   = iter𝐿
instance Null         (𝐿 a) where null   = empty𝐿
instance Append       (𝐿 a) where (⧺)    = append𝐿
instance Monoid       (𝐿 a) 
instance Return       𝐿     where return = single𝐿
instance Bind         𝐿     where (≫=)   = bind𝐿
instance Functor      𝐿     where map    = map𝐿
instance Monad        𝐿
instance FunctorM     𝐿     where mapM   = mapM𝐿

empty𝐿 ∷ 𝐿 a
empty𝐿 = Nil

single𝐿 ∷ a → 𝐿 a
single𝐿 x = x :& Nil

lookup𝐿 ∷ ℕ64 → 𝐿 a → 𝑂 a
lookup𝐿 n = \case
  Nil → None
  x :& xs →
    if n ≡ zero
    then Some x
    else lookup𝐿 (n - one) xs

cons𝐿 ∷ a → 𝐿 a → 𝐿 a
cons𝐿 = (:&)

uncons𝐿 ∷ 𝐿 a → 𝑂 (a ∧ 𝐿 a)
uncons𝐿 = \case
  Nil → None
  x :& xs → Some $ x :* xs

snoc𝐿 ∷ 𝐿 a → a → 𝐿 a
snoc𝐿 xs x = case xs of
  Nil → x :& Nil
  x' :& xs' → x' :& snoc𝐿 xs' x

unsnoc𝐿 ∷ 𝐿 a → 𝑂 (𝐿 a ∧ a)
unsnoc𝐿 = \case
  Nil → None
  x₀ :& xs₀ → Some $
    let loop x xs = case xs of
          Nil → Nil :* x
          x' :& xs' →
            let xsᵣ :* xᵣ = loop x' xs'
            in (x :& xsᵣ) :* xᵣ
    in loop x₀ xs₀

append𝐿 ∷ 𝐿 a → 𝐿 a → 𝐿 a
append𝐿 xs ys = case xs of
  Nil → ys
  x :& xs' → x :& append𝐿 xs' ys

map𝐿 ∷ (a → b) → 𝐿 a → 𝐿 b
map𝐿 f xs = case xs of
  Nil → Nil
  x :& xs' → f x :& map f xs'

bind𝐿 ∷ 𝐿 a → (a → 𝐿 b) → 𝐿 b
bind𝐿 xs k = case xs of
  Nil → Nil
  x :& xs' → append𝐿 (k x) (bind𝐿 xs' k)

mapM𝐿 ∷ (Monad m) ⇒ (a → m b) → 𝐿 a → m (𝐿 b)
mapM𝐿 f xs = case xs of
  Nil → return Nil
  x :& xs' → do
    y ← f x
    ys ← mapM𝐿 f xs'
    return $ y :& ys

cart ∷ 𝐿 (𝐿 a) → 𝐿 (𝐿 a)
cart Nil = Nil :& Nil
cart (xs:&xss) = do
  x ← xs
  xs' ← cart xss
  return $ x :& xs'

swivelL ∷ 𝐿 a → a → a ∧ 𝐿 a
swivelL Nil x = x :* Nil
swivelL (x :& xs) y =
  let x' :* xs' = swivelL xs y
  in x :* (x' :& xs')

swivelR ∷ a → 𝐿 a → 𝐿 a ∧ a
swivelR x Nil = Nil :* x
swivelR x (y :& xs) =
  let xs' :* x' = swivelR y xs
  in (x :& xs') :* x'

iswivelL ∷ 𝐿 (a ∧ i) → a → a ∧ 𝐿 (i ∧ a)
iswivelL Nil x = x :* Nil
iswivelL ((x :* i) :& xis) y =
  let x' :* ixs = iswivelL xis y
  in x :* ((i :* x') :& ixs)

iswivelR ∷ a → 𝐿 (i ∧ a) → 𝐿 (a ∧ i) ∧ a
iswivelR x Nil = Nil :* x
iswivelR x ((i :* y) :& ixs) =
  let xis :* x' = iswivelR y ixs
  in ((x :* i) :& xis) :* x'

zipSameLength ∷ 𝐿 a → 𝐿 b → 𝑂 (𝐿 (a ∧ b))
zipSameLength xs ys = case (xs,ys) of
  (Nil,Nil) → Some Nil
  (x:&xs',y:&ys') → case zipSameLength xs' ys' of
    None → None
    Some xys → Some $ (x :* y) :& xys
  _ → None

split ∷ 𝐿 (a ∧ b) → 𝐿 a ∧ 𝐿 b
split = \case
  Nil → Nil :* Nil
  (x :* y) :& xys →
    let xs :* ys = split xys
    in (x :& xs) :* (y :& ys)

firstSome ∷ 𝐿 (𝑂 a) → 𝑂 a
firstSome = \case
  Nil → None
  None :& xOs → firstSome xOs
  Some x :& _ → Some x
