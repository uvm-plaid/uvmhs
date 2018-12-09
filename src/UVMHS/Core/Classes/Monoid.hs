module UVMHS.Core.Classes.Monoid where

infixl 4 ⧺

class Null a where null ∷ a
class Append a where (⧺) ∷ a → a → a
class (Null a,Append a) ⇒ Monoid a
