module UVMHS.Lib.ZerInf where

import UVMHS.Core

-- ====== --
-- AddZer --
-- ====== --

data AddZer a = Zer | AddZer a
  deriving (Eq,Ord,Show)

elimAddZer ∷ b → (a → b) → AddZer a → b
elimAddZer i f = \case
  Zer → i
  AddZer x → f x

instance Bot (AddZer a) where bot = Zer
instance (Join a) ⇒ Join (AddZer a) where
  Zer ⊔ x = x
  x ⊔ Zer = x
  AddZer x ⊔ AddZer y = AddZer $ x ⊔ y
instance (Top a) ⇒ Top (AddZer a) where top = AddZer top
instance (Meet a) ⇒ Meet (AddZer a) where
  Zer ⊓ _ = Zer
  _ ⊓ Zer = Zer
  AddZer x ⊓ AddZer y = AddZer $ x ⊓ y
instance (Join a) ⇒ JoinLattice (AddZer a)
instance (MeetLattice a) ⇒ MeetLattice (AddZer a)
instance (Join a,MeetLattice a) ⇒ Lattice (AddZer a)
instance Functor AddZer where map = mmap
instance Return AddZer where return = AddZer
instance Bind AddZer where xM ≫= f = case xM of {Zer → Zer;AddZer x → f x}
instance Monad AddZer
instance FunctorM AddZer where mapM f xM = case xM of {Zer → return Zer;AddZer x → map AddZer $ f x}

instance Zero (AddZer a) where zero = Zer
instance (One a) ⇒ One (AddZer a) where one = AddZer one
instance (Plus a) ⇒ Plus (AddZer a) where
  Zer + y = y
  x + Zer = x
  AddZer x + AddZer y = AddZer $ x + y
instance (Times a) ⇒ Times (AddZer a) where
  Zer × y = y
  x × Zer = x
  AddZer x × AddZer y = AddZer $ x × y
instance (Divide a,Top a) ⇒ Divide (AddZer a) where
  Zer / Zer = error "0 / 0"
  Zer / _ = Zer
  _ / Zer = top
  AddZer x / AddZer y = AddZer $ x / y
instance (DivMod a,Top a) ⇒ DivMod (AddZer a) where
  Zer ⌿ Zer = error "0 ⌿ 0"
  Zer ⌿ _ = Zer
  _ ⌿ Zer = top
  AddZer x ⌿ AddZer y = AddZer $ x ⌿ y
  Zer ÷ _ = Zer
  _ ÷ Zer = error "mod Zer"
  AddZer x ÷ AddZer y = AddZer $ x ÷ y
instance (Pon a) ⇒ Pon (AddZer a) where
  Zer ^^ _ = Zer
  AddZer x ^^ n = AddZer $ x ^^ n
instance (Pow a,One a) ⇒ Pow (AddZer a) where
  _ ^ Zer = AddZer one
  Zer ^ _ = Zer
  AddZer x ^ AddZer y = AddZer $ x ^ y
instance (Root a) ⇒ Root (AddZer a) where
  root Zer = Zer
  root (AddZer x) = AddZer $ root x
instance (Log a,Top a,Minus a) ⇒ Log (AddZer a) where
  log Zer = error "log of Zer"
  log (AddZer x) = AddZer $ log x
instance (Efn a,One a) ⇒ Efn (AddZer a) where
  efn Zer = AddZer one
  efn (AddZer x) = AddZer $ efn x

-- ====== --
-- AddInf --
-- ====== --

data AddInf a = Inf | AddInf a
  deriving (Eq,Ord,Show)

elimAddInf ∷ b → (a → b) → AddInf a → b
elimAddInf i f = \case
  Inf → i
  AddInf x → f x

instance (Null a) ⇒ Null (AddInf a) where null = AddInf null
instance (Append a) ⇒ Append (AddInf a) where
  Inf ⧺ _ = Inf
  _ ⧺ Inf = Inf
  AddInf x ⧺ AddInf y = AddInf (x ⧺ y)
instance (Monoid a) ⇒ Monoid (AddInf a)

instance (Bot a) ⇒ Bot (AddInf a) where bot = AddInf bot
instance (Join a) ⇒ Join (AddInf a) where
  Inf ⊔ _ = Inf
  _ ⊔ Inf = Inf
  AddInf x ⊔ AddInf y = AddInf $ x ⊔ y
instance Top (AddInf a) where top = Inf
instance (Meet a) ⇒ Meet (AddInf a) where
  Inf ⊓ x = x
  x ⊓ Inf = x
  AddInf x ⊓ AddInf y = AddInf $ x ⊓ y
instance (JoinLattice a) ⇒ JoinLattice (AddInf a)
instance (Meet a) ⇒ MeetLattice (AddInf a)
instance (JoinLattice a,Meet a) ⇒ Lattice (AddInf a)
instance Functor AddInf where map = mmap
instance Return AddInf where return = AddInf
instance Bind AddInf where xM ≫= f = case xM of {Inf → Inf;AddInf x → f x}
instance Monad AddInf
instance FunctorM AddInf where mapM f xM = case xM of {Inf → return Inf;AddInf x → map AddInf $ f x}

instance (Zero a) ⇒ Zero (AddInf a) where zero = AddInf zero
instance (One a) ⇒ One (AddInf a) where one = AddInf one
instance (Plus a) ⇒ Plus (AddInf a) where
  Inf + _ = Inf
  _ + Inf = Inf
  AddInf x + AddInf y = AddInf $ x + y
instance (Times a) ⇒ Times (AddInf a) where
  Inf × _ = Inf
  _ × Inf = Inf
  AddInf x × AddInf y = AddInf $ x × y
instance (Divide a,Zero a,Eq a) ⇒ Divide (AddInf a) where
  Inf / Inf = error "∞ / ∞"
  Inf / y
    | y ≡ zero = error "∞ / 0"
    | otherwise = Inf
  _ / Inf = zero
  AddInf x / AddInf y = AddInf $ x / y
instance (DivMod a,Zero a) ⇒ DivMod (AddInf a) where
  Inf ⌿ _ = Inf
  _ ⌿ Inf = zero
  AddInf x ⌿ AddInf y = AddInf $ x ⌿ y
  Inf ÷ x = x
  x ÷ Inf = x
  AddInf x ÷ AddInf y = AddInf $ x ÷ y
instance (Pon a,One a) ⇒ Pon (AddInf a) where
  Inf ^^ n
    | n ≡ zero = one
    | otherwise = Inf
  AddInf x ^^ n = AddInf $ x ^^ n
instance (Pow a,One a) ⇒ Pow (AddInf a) where
  _ ^ _ = undefined
instance (Root a) ⇒ Root (AddInf a) where
  root _ = undefined
instance (Log a,Top a,Minus a) ⇒ Log (AddInf a) where
  log _ = undefined
instance (Efn a,One a) ⇒ Efn (AddInf a) where
  efn _ = undefined

-- ===== --
-- AddZI --
-- ===== --

data AddZI a = ZerZI | InfZI | AddZI a
  deriving (Eq,Ord,Show)

instance Bot (AddZI a) where bot = ZerZI
instance (Join a) ⇒ Join (AddZI a) where
  ZerZI ⊔ x = x
  x ⊔ ZerZI = x
  InfZI ⊔ _ = InfZI
  _ ⊔ InfZI = InfZI
  AddZI x ⊔ AddZI y = AddZI $ x ⊔ y
instance Top (AddZI a) where top = InfZI
instance (Meet a) ⇒ Meet (AddZI a) where
  ZerZI ⊓ _ = ZerZI
  _ ⊓ ZerZI = ZerZI
  InfZI ⊓ x = x
  x ⊓ InfZI = x
  AddZI x ⊓ AddZI y = AddZI $ x ⊓ y
instance (Join a) ⇒ JoinLattice (AddZI a)
instance (Meet a) ⇒ MeetLattice (AddZI a)
instance (Join a,Meet a) ⇒ Lattice (AddZI a)
instance Functor AddZI where map = mmap
instance Return AddZI where return = AddZI
instance Bind AddZI where xM ≫= f = case xM of {InfZI → InfZI;ZerZI → ZerZI;AddZI x → f x}
instance Monad AddZI
instance FunctorM AddZI where mapM f xM = case xM of {InfZI → return InfZI;ZerZI → return ZerZI;AddZI x → map AddZI $ f x}
