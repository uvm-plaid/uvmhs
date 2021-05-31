module UVMHS.Core.Classes.Monoid where

import UVMHS.Core.Init

import UVMHS.Core.Classes.Monad

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


newtype Compose a = Compose { unCompose ∷ a → a }

instance Null (Compose a) where null = Compose id
instance Append (Compose a) where g ⧺ f = Compose $ unCompose g ∘ unCompose f
instance Monoid (Compose a)

newtype MCompose m a = MCompose { unMCompose ∷ a → m a }

instance (Return m) ⇒ Null (MCompose m a) where null = MCompose return
instance (Bind m) ⇒ Append (MCompose m a) where g ⧺ f = MCompose $ unMCompose g *∘ unMCompose f
instance (Monad m) ⇒ Monoid (MCompose m a)
