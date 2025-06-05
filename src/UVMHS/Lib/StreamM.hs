module UVMHS.Lib.StreamM where

import UVMHS.Core

data Step a = 
    Done_𝑆M 
  | Next_𝑆M a
  deriving (Eq,Ord,Show)
data Result a = 
    Skip_𝑆M 
  | Elem_𝑆M a
  deriving (Eq,Ord,Show)

-- == --
-- 𝑆M --
-- == --

newtype 𝑆M m a = 𝑆M { un𝑆M :: () → m (Step (Result a ∧ 𝑆M m a)) }

uncons𝑆M ∷ 𝑆M m a → m (Step (Result a ∧ 𝑆M m a))
uncons𝑆M = appto () ∘ un𝑆M

instance (Return m) ⇒ Return  (𝑆M m) where return = single𝑆M
instance (Monad m)  ⇒ Bind    (𝑆M m) where (≫=)   = bind𝑆M
instance (Monad m)  ⇒ Functor (𝑆M m) where map    = map𝑆M
instance (Monad m)  ⇒ Monad   (𝑆M m)

instance (Monad m) ⇒ MonadNondet (𝑆M m) where {mzero = empty𝑆M;(⊞) = append𝑆M}

instance (Return m) ⇒ Null (𝑆M m a) where null = empty𝑆M
instance (Monad m) ⇒ Append (𝑆M m a) where (⧺) = append𝑆M
instance (Monad m) ⇒ Monoid (𝑆M m a)

empty𝑆M ∷ (Return m) ⇒ 𝑆M m a
empty𝑆M = 𝑆M $ \ () → return Done_𝑆M

single𝑆M ∷ (Return m) ⇒ a → 𝑆M m a
single𝑆M x = 𝑆M $ \ () → return $ Next_𝑆M $ Elem_𝑆M x :* empty𝑆M

lift𝑆M ∷ (Monad m) ⇒ m a → 𝑆M m a
lift𝑆M xM = 𝑆M $ \ () → do
  x ← xM
  return $ Next_𝑆M $ Elem_𝑆M x :* empty𝑆M

append𝑆M ∷ (Monad m) ⇒ 𝑆M m a → 𝑆M m a → 𝑆M m a
append𝑆M xs ys = 𝑆M $ \ () → do
  r ← uncons𝑆M xs
  case r of
    Done_𝑆M → return $ Next_𝑆M $ Skip_𝑆M :* ys
    Next_𝑆M (x :* xs') → return $ Next_𝑆M $ x :* append𝑆M xs'  ys

map𝑆M ∷ (Monad m) ⇒ (a → b) → 𝑆M m a → 𝑆M m b
map𝑆M f xs = 𝑆M $ \ () → do
  r ← uncons𝑆M xs
  case r of
    Done_𝑆M → return Done_𝑆M
    Next_𝑆M (xO :* xs') → case xO of
      Skip_𝑆M → return $ Next_𝑆M $ Skip_𝑆M :* map𝑆M f xs'
      Elem_𝑆M x → return $ Next_𝑆M $ Elem_𝑆M (f x) :* map𝑆M f xs'

bind𝑆M ∷ (Monad m) ⇒ 𝑆M m a → (a → 𝑆M m b) → 𝑆M m b
bind𝑆M xs f = 𝑆M $ \ () → do
  r ← uncons𝑆M xs
  case r of
    Done_𝑆M → return Done_𝑆M
    Next_𝑆M (xO :* xs') → case xO of
      Skip_𝑆M → return $ Next_𝑆M $ Skip_𝑆M :* bind𝑆M xs' f
      Elem_𝑆M x → uncons𝑆M $ append𝑆M (f x) $ bind𝑆M xs' f

appendInterleave𝑆M ∷ (Monad m) ⇒ 𝑆M m a → 𝑆M m a → 𝑆M m a
appendInterleave𝑆M xs ys = 𝑆M $ \ () → do
  r ← uncons𝑆M xs
  case r of
    Done_𝑆M → return $ Next_𝑆M $ Skip_𝑆M :* ys
    Next_𝑆M (xO :* xs') → case xO of
      -- LHS returned a "skip", so just flip LHS and RHS
      Skip_𝑆M → return $ Next_𝑆M $ Skip_𝑆M :* appendInterleave𝑆M ys xs'
      -- LHS returned a value, so return that value and flip LHS and RHS
      Elem_𝑆M x → return $ Next_𝑆M $ Elem_𝑆M x :* appendInterleave𝑆M ys xs'

bindInterleave𝑆M ∷ (Monad m) ⇒ 𝑆M m a → (a → 𝑆M m b) → 𝑆M m b
bindInterleave𝑆M xs f = 𝑆M $ \ () → do
  r ← uncons𝑆M xs
  case r of
    Done_𝑆M → return Done_𝑆M
    Next_𝑆M (xO :* xs') → case xO of
      Skip_𝑆M → return $ Next_𝑆M $ Skip_𝑆M :* bindInterleave𝑆M xs' f
      Elem_𝑆M x → uncons𝑆M $ appendInterleave𝑆M (f x) $ bindInterleave𝑆M xs' f

-- === --
-- 𝑆MI --
-- === --

newtype 𝑆MI m a = 𝑆MI { un𝑆MI ∷ 𝑆M m a }

empty𝑆MI ∷ ∀ m a. (Return m) ⇒ 𝑆MI m a
empty𝑆MI = coerce @(𝑆M m a) empty𝑆M

single𝑆MI ∷ ∀ m a. (Return m) ⇒ a → 𝑆MI m a
single𝑆MI = coerce @(a → 𝑆M m a) single𝑆M

lift𝑆MI ∷ ∀ m a. (Monad m) ⇒ m a → 𝑆MI m a
lift𝑆MI = coerce @(m a → 𝑆M m a) lift𝑆M

append𝑆MI ∷ ∀ m a. (Monad m) ⇒ 𝑆MI m a → 𝑆MI m a → 𝑆MI m a
append𝑆MI = coerce @(𝑆M m a → 𝑆M m a → 𝑆M m a) appendInterleave𝑆M

map𝑆MI ∷ ∀ m a b. (Monad m) ⇒ (a → b) → 𝑆MI m a → 𝑆MI m b
map𝑆MI = coerce @((a → b) → 𝑆M m a → 𝑆M m b) map𝑆M

bind𝑆MI ∷ ∀ m a b. (Monad m) ⇒ 𝑆MI m a → (a → 𝑆MI m b) → 𝑆MI m b
bind𝑆MI = coerce @(𝑆M m a → (a → 𝑆M m b) → 𝑆M m b) bindInterleave𝑆M

instance (Return m) ⇒ Return  (𝑆MI m) where return = single𝑆MI
instance (Monad m)  ⇒ Bind    (𝑆MI m) where (≫=)   = bind𝑆MI
instance (Monad m)  ⇒ Functor (𝑆MI m) where map    = map𝑆MI
instance (Monad m)  ⇒ Monad   (𝑆MI m)

instance (Monad m) ⇒ MonadNondet (𝑆MI m) where {mzero = empty𝑆MI;(⊞) = append𝑆MI}

instance (Return m) ⇒ Null (𝑆MI m a) where null = empty𝑆MI
instance (Monad m) ⇒ Append (𝑆MI m a) where (⧺) = append𝑆MI
instance (Monad m) ⇒ Monoid (𝑆MI m a)
