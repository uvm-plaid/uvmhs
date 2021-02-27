module UVMHS.Core.Classes.Monoid where

import UVMHS.Core.Init

infixl 5 ⧺
infixl 6 ⨳

class Null a where null ∷ a
class Append a where (⧺) ∷ a → a → a
class (Null a,Append a) ⇒ Monoid a

prepend ∷ (Append a) ⇒ a → a → a
prepend = (⧺)

pospend ∷ (Append a) ⇒ a → a → a
pospend = flip (⧺)

class Unit a where unit ∷ a
class Cross a where (⨳) ∷ a → a → a
class (Monoid a,Unit a,Cross a) ⇒ Prodoid a

class Eps a where eps ∷ a
class Seq a where (▷) ∷ a → a → a
class (Monoid a,Eps a,Seq a) ⇒ Seqoid a

opt ∷ (Append a,Eps a) ⇒ a → a
opt x = x ⧺ eps

class Star a where star ∷ a → a
class (Seqoid a,Star a) ⇒ Kleene a

oom ∷ (Kleene a) ⇒ a → a
oom x = x ▷ star x
