module UVMHS.Core.Data.List where

import UVMHS.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data.LazyList ()

instance Null (𝐿 a) where null = empty𝐿
instance Append (𝐿 a) where (⧺) = append𝐿
instance Monoid (𝐿 a)
instance Functor 𝐿 where map = map𝐿
instance Return 𝐿 where return = single𝐿
instance Bind 𝐿 where (≫=) = bind𝐿
instance Monad 𝐿
instance FunctorM 𝐿 where mapM = mapM𝐿
instance Single a (𝐿 a) where single = single𝐿
instance ToStream a (𝐿 a) where stream = stream𝐿
instance ToIter a (𝐿 a) where iter = iter𝑆 ∘ stream𝐿

empty𝐿 ∷ 𝐿 a
empty𝐿 = Nil

single𝐿 ∷ a → 𝐿 a
single𝐿 x = x :& Nil

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
