module UVMHS.Core.Data.Function where

import UVMHS.Core.Init
import UVMHS.Core.Classes

instance Functor ((→) r) where 
  {-# INLINE map #-}
  map f g = f ∘ g
instance Return ((→) r) where 
  {-# INLINE return #-} 
  return = const
instance Bind ((→) r) where 
  {-# INLINE (≫=) #-} 
  f ≫= k = \ r → k (f r) r
instance Monad ((→) r)

instance (Null a) ⇒ Null (r → a) where
  {-# INLINE null #-}
  null = const null
instance (Append a) ⇒ Append (r → a) where
  {-# INLINE (⧺) #-}
  f ⧺ g = \ r → f r ⧺ g r
instance (Monoid a) ⇒ Monoid (r → a)

arg ∷ a → (a → b) → b
arg x f = f x
