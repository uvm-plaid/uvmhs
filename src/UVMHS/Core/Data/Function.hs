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

appto ∷ a → (a → b) → b
appto x f = f x

pipe ∷ (a → b) → (b → c) → a → c
pipe = flip (∘)
