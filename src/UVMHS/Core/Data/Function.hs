module UVMHS.Core.Data.Function where

import UVMHS.Core.Init
import UVMHS.Core.Classes

instance Functor ((→) r) where 
  map f g = f ∘ g
instance Return ((→) r) where 
  return = const
instance Bind ((→) r) where 
  f ≫= k = \ r → k (f r) r
instance Monad ((→) r)

instance (Null a) ⇒ Null (r → a) where
  null = const null
instance (Append a) ⇒ Append (r → a) where
  f ⧺ g = \ r → f r ⧺ g r
instance (Monoid a) ⇒ Monoid (r → a)

instance (Eq a) ⇒ Eq (() → a) where
  f == g = f () ≡ g ()
instance (Ord a) ⇒ Ord (() → a) where
  compare f g = f () ⋚ g ()
instance (Show a) ⇒ Show (() → a) where
  show = show ∘ appto ()

appto ∷ a → (a → b) → b
appto x f = f x

pipe ∷ (a → b) → (b → c) → a → c
pipe = flip (∘)

iterateWith ∷ (a → 𝑂 a) → a → a
iterateWith f = 
  let loop' x = case f x of
        None → x
        Some x' → loop' x'
  in loop'

iterateFrom ∷ a → (a → 𝑂 a) → a
iterateFrom = flip iterateWith
