module UVMHS.Core.Data.List where

import UVMHS.Core.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data.LazyList ()

instance Null (𝐿 a) where 
  -- {-# INLINE null #-}
  null = empty𝐿
instance Append (𝐿 a) where 
  -- {-# INLINE (⧺) #-}
  (⧺) = append𝐿
instance Monoid (𝐿 a)
instance Functor 𝐿 where 
  -- {-# INLINE map #-}
  map = map𝐿
instance Return 𝐿 where
  -- {-# INLINE return #-}
  return = single𝐿
instance Bind 𝐿 where 
  -- {-# INLINE (≫=) #-}
  (≫=) = bind𝐿
instance Monad 𝐿
instance FunctorM 𝐿 where 
  -- {-# INLINE mapM #-}
  mapM = mapM𝐿
instance Single a (𝐿 a) where 
  -- {-# INLINE single #-}
  single = single𝐿
instance ToStream a (𝐿 a) where 
  -- {-# INLINE stream #-}
  stream = stream𝐿
instance ToIter a (𝐿 a) where 
  -- {-# INLINE iter #-}
  iter = iter𝑆 ∘ stream𝐿

-- {-# INLINE empty𝐿 #-}
empty𝐿 ∷ 𝐿 a
empty𝐿 = Nil

-- {-# INLINE single𝐿 #-}
single𝐿 ∷ a → 𝐿 a
single𝐿 x = x :& Nil

-- {-# INLINE cons𝐿 #-}
cons𝐿 ∷ a → 𝐿 a → 𝐿 a
cons𝐿 = (:&)

snoc𝐿 ∷ 𝐿 a → a → 𝐿 a
snoc𝐿 xs x = case xs of
  Nil → x :& Nil
  x' :& xs' → x' :& snoc𝐿 xs' x

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
