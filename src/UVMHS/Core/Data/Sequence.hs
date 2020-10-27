module UVMHS.Core.Data.Sequence where

import UVMHS.Core.Init
import UVMHS.Core.Classes

import UVMHS.Core.Data.Iter
import UVMHS.Core.Data.Stream ()
import UVMHS.Core.Data.String

import qualified Prelude as HS
import qualified Data.Foldable as HS
import qualified Data.Sequence as Sequence

instance Null (𝑄 a) where 
  {-# INLINE null #-}
  null = qø
instance Append (𝑄 a) where 
  {-# INLINE (⧺) #-}
  (⧺) = append𝑄
instance Monoid (𝑄 a)
instance Single a (𝑄 a) where 
  {-# INLINE single #-}
  single = single𝑄

instance Functor 𝑄 where 
  {-# INLINE map #-}
  map = map𝑄
instance Return 𝑄 where 
  {-# INLINE return #-}
  return = single
instance Bind 𝑄 where 
  {-# INLINE (≫=) #-}
  (≫=) = bind𝑄
instance Monad 𝑄

instance ToStream a (𝑄 a) where 
  {-# INLINE stream #-}
  stream = stream𝑄
instance ToIter a (𝑄 a) where 
  {-# INLINE iter #-}
  iter = iter ∘ stream

instance (Show a) ⇒ Show (𝑄 a) where 
  {-# INLINE show #-}
  show = chars ∘ showCollection "[" "]" "," show𝕊

{-# INLINE qø #-}
qø ∷ 𝑄 a
qø = 𝑄 Sequence.empty

{-# INLINE cons𝑄 #-}
cons𝑄 ∷ a → 𝑄 a → 𝑄 a
cons𝑄 x xs = 𝑄 $ x Sequence.:<| un𝑄 xs

{-# INLINE uncons𝑄 #-}
uncons𝑄 ∷ 𝑄 a → 𝑂 (a ∧ 𝑄 a)
uncons𝑄 xs = case Sequence.viewl $ un𝑄 xs of
  Sequence.EmptyL → None
  x Sequence.:< xs' → Some $ x :* 𝑄 xs'

{-# INLINE snoc𝑄 #-}
snoc𝑄 ∷ 𝑄 a → a → 𝑄 a
snoc𝑄 xs x = 𝑄 $ un𝑄 xs Sequence.:|> x

{-# INLINE unsnoc𝑄 #-}
unsnoc𝑄 ∷ 𝑄 a → 𝑂 (𝑄 a ∧ a)
unsnoc𝑄 xs = case Sequence.viewr $ un𝑄 xs of
  Sequence.EmptyR → None
  xs' Sequence.:> x → Some $ 𝑄 xs' :* x

{-# INLINE single𝑄 #-}
single𝑄 ∷ a → 𝑄 a
single𝑄 = 𝑄 ∘ Sequence.singleton

{-# INLINE append𝑄 #-}
append𝑄 ∷ 𝑄 a → 𝑄 a → 𝑄 a
append𝑄 xs ys = 𝑄 $ un𝑄 xs Sequence.>< un𝑄 ys

{-# INLINE map𝑄 #-}
map𝑄 ∷ (a → b) → 𝑄 a → 𝑄 b
map𝑄 f = 𝑄 ∘ HS.fmap f ∘ un𝑄

{-# INLINE bind𝑄 #-}
bind𝑄 ∷ 𝑄 a → (a → 𝑄 b) → 𝑄 b
bind𝑄 xs f = 𝑄 $ un𝑄 xs HS.>>= (un𝑄 ∘ f)

{-# INLINE stream𝑄 #-}
stream𝑄 ∷ 𝑄 a → 𝑆 a
stream𝑄 = stream ∘ HS.toList ∘ un𝑄

{-# INLINE seq𝐼 #-}
seq𝐼 ∷ 𝐼 a → 𝑄 a
seq𝐼 = 𝑄 ∘ Sequence.fromList ∘ lazyList

{-# INLINE seq #-}
seq ∷ (ToIter a t) ⇒ t → 𝑄 a
seq = seq𝐼 ∘ iter
