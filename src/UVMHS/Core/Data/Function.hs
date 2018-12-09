module UVMHS.Core.Data.Function where

import UVMHS.Init
import UVMHS.Core.Classes

instance Functor ((→) r) where map f g = f ∘ g
instance Return ((→) r) where return = const
instance Bind ((→) r) where f ≫= k = \ r → k (f r) r
instance Monad ((→) r)

