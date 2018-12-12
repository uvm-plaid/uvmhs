module UVMHS.Core.Data.Sequence where

import UVMHS.Init
import UVMHS.Core.Classes

import UVMHS.Core.Data.Iter
import UVMHS.Core.Data.LazyList
import UVMHS.Core.Data.Stream ()
import UVMHS.Core.Data.String

import qualified Prelude as HS
import qualified Data.Foldable as HS
import qualified Data.Sequence as Sequence

instance Null (𝑄 a) where null = qø
instance Append (𝑄 a) where (⧺) = append𝑄
instance Monoid (𝑄 a)
instance Single a (𝑄 a) where single = single𝑄

instance Functor 𝑄 where map = map𝑄
instance Return 𝑄 where return = single
instance Bind 𝑄 where (≫=) = bind𝑄
instance Monad 𝑄

instance ToStream a (𝑄 a) where stream = stream𝑄
instance ToIter a (𝑄 a) where iter = iter ∘ stream

instance (Show a) ⇒ Show (𝑄 a) where show = chars ∘ showCollection "[" "]" "," show𝕊

qø ∷ 𝑄 a
qø = 𝑄 Sequence.empty

cons𝑄 ∷ a → 𝑄 a → 𝑄 a
cons𝑄 x xs = 𝑄 $ x Sequence.:<| un𝑄 xs

uncons𝑄 ∷ 𝑄 a → 𝑂 (a ∧ 𝑄 a)
uncons𝑄 xs = case Sequence.viewl $ un𝑄 xs of
  Sequence.EmptyL → None
  x Sequence.:< xs' → Some $ x :* 𝑄 xs'

snoc𝑄 ∷ 𝑄 a → a → 𝑄 a
snoc𝑄 xs x = 𝑄 $ un𝑄 xs Sequence.:|> x

unsnoc𝑄 ∷ 𝑄 a → 𝑂 (𝑄 a ∧ a)
unsnoc𝑄 xs = case Sequence.viewr $ un𝑄 xs of
  Sequence.EmptyR → None
  xs' Sequence.:> x → Some $ 𝑄 xs' :* x

single𝑄 ∷ a → 𝑄 a
single𝑄 = 𝑄 ∘ Sequence.singleton

append𝑄 ∷ 𝑄 a → 𝑄 a → 𝑄 a
append𝑄 xs ys = 𝑄 $ un𝑄 xs Sequence.>< un𝑄 ys

map𝑄 ∷ (a → b) → 𝑄 a → 𝑄 b
map𝑄 f = 𝑄 ∘ HS.fmap f ∘ un𝑄

bind𝑄 ∷ 𝑄 a → (a → 𝑄 b) → 𝑄 b
bind𝑄 xs f = 𝑄 $ un𝑄 xs HS.>>= (un𝑄 ∘ f)

stream𝑄 ∷ 𝑄 a → 𝑆 a
stream𝑄 = stream ∘ HS.toList ∘ un𝑄

seq𝐼 ∷ 𝐼 a → 𝑄 a
seq𝐼 = 𝑄 ∘ Sequence.fromList ∘ lazyList

seq ∷ (ToIter a t) ⇒ t → 𝑄 a
seq = seq𝐼 ∘ iter
