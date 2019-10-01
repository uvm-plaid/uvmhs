module UVMHS.Core.Classes.Monoid where

infixl 4 ⧺

class Null a where null ∷ a
class Append a where (⧺) ∷ a → a → a
class (Null a,Append a) ⇒ Monoid a
class Unit a where unit ∷ a
class Cross a where (⨳) ∷ a → a → a
class (Monoid a,Unit a,Cross a) ⇒ Prodoid a

class Eps a where eps ∷ a
class Seq a where (▷) ∷ a → a → a
class (Monoid a,Eps a,Seq a) ⇒ Seqoid a
