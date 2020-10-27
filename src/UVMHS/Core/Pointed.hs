module UVMHS.Core.Pointed where

import UVMHS.Core.Init
import UVMHS.Core.Classes

-- ======= --
-- AddNull --
-- ======= --

data AddNull a = Null | AddNull a
  deriving (Eq,Ord,Show)

{-# INLINE elimAddNull #-}
elimAddNull ∷ b → (a → b) → AddNull a → b
elimAddNull i f = \case
  Null → i
  AddNull x → f x

instance Null (AddNull a) where 
  {-# INLINE null #-}
  null = Null
instance (Append a) ⇒ Append (AddNull a) where
  {-# INLINE (⧺) #-}
  Null ⧺ x = x
  x ⧺ Null = x
  AddNull x ⧺ AddNull y = AddNull $ x ⧺ y
instance (Append a) ⇒ Monoid (AddNull a)
instance Functor AddNull where 
  {-# INLINE map #-}
  map = mmap
instance Return AddNull where 
  {-# INLINE return #-}
  return = AddNull
instance Bind AddNull where 
  {-# INLINE (≫=) #-}
  xM ≫= f = case xM of {Null → Null;AddNull x → f x}
instance Monad AddNull
instance FunctorM AddNull where 
  {-# INLINE mapM #-}
  mapM f xM = case xM of {Null → return Null;AddNull x → map AddNull $ f x}

-- ====== --
-- AddBot --
-- ====== --

data AddBot a = Bot | AddBot a
  deriving (Eq,Ord,Show)

{-# INLINE elimAddBot #-}
elimAddBot ∷ b → (a → b) → AddBot a → b
elimAddBot i f = \case
  Bot → i
  AddBot x → f x

instance Bot (AddBot a) where 
  {-# INLINE bot #-}
  bot = Bot
instance (Join a) ⇒ Join (AddBot a) where
  {-# INLINE (⊔) #-}
  Bot ⊔ x = x
  x ⊔ Bot = x
  AddBot x ⊔ AddBot y = AddBot $ x ⊔ y
instance (Top a) ⇒ Top (AddBot a) where 
  {-# INLINE top #-}
  top = AddBot top
instance (Meet a) ⇒ Meet (AddBot a) where
  {-# INLINE (⊓) #-}
  Bot ⊓ _ = Bot
  _ ⊓ Bot = Bot
  AddBot x ⊓ AddBot y = AddBot $ x ⊓ y
instance (Join a) ⇒ JoinLattice (AddBot a)
instance (MeetLattice a) ⇒ MeetLattice (AddBot a)
instance (Join a,MeetLattice a) ⇒ Lattice (AddBot a)
instance Functor AddBot where 
  {-# INLINE map #-}
  map = mmap
instance Return AddBot where 
  {-# INLINE return #-}
  return = AddBot
instance Bind AddBot where 
  {-# INLINE (≫=) #-}
  xM ≫= f = case xM of {Bot → Bot;AddBot x → f x}
instance Monad AddBot
instance FunctorM AddBot where 
  {-# INLINE mapM #-}
  mapM f xM = case xM of {Bot → return Bot;AddBot x → map AddBot $ f x}

-- ====== --
-- AddTop --
-- ====== --

data AddTop a = Top | AddTop a
  deriving (Eq,Ord,Show)

{-# INLINE elimAddTop #-}
elimAddTop ∷ b → (a → b) → AddTop a → b
elimAddTop i f = \case
  Top → i
  AddTop x → f x

-- instance (Null a) ⇒ Null (AddTop a) where 
--   {-# INLINE null #-}
--   null = AddTop null
-- instance (Append a) ⇒ Append (AddTop a) where
--   {-# INLINE (⧺) #-}
--   Top ⧺ _ = Top
--   _ ⧺ Top = Top
--   AddTop x ⧺ AddTop y = AddTop (x ⧺ y)
-- instance (Monoid a) ⇒ Monoid (AddTop a)

instance (Bot a) ⇒ Bot (AddTop a) where 
  {-# INLINE bot #-}
  bot = AddTop bot
instance (Join a) ⇒ Join (AddTop a) where
  {-# INLINE (⊔) #-}
  Top ⊔ _ = Top
  _ ⊔ Top = Top
  AddTop x ⊔ AddTop y = AddTop $ x ⊔ y
instance Top (AddTop a) where 
  {-# INLINE top #-}
  top = Top
instance (Meet a) ⇒ Meet (AddTop a) where
  {-# INLINE (⊓) #-}
  Top ⊓ x = x
  x ⊓ Top = x
  AddTop x ⊓ AddTop y = AddTop $ x ⊓ y
instance (JoinLattice a) ⇒ JoinLattice (AddTop a)
instance (Meet a) ⇒ MeetLattice (AddTop a)
instance (JoinLattice a,Meet a) ⇒ Lattice (AddTop a)
instance Functor AddTop where 
  {-# INLINE map #-}
  map = mmap
instance Return AddTop where 
  {-# INLINE return #-}
  return = AddTop
instance Bind AddTop where 
  {-# INLINE (≫=) #-}
  xM ≫= f = case xM of {Top → Top;AddTop x → f x}
instance Monad AddTop
instance FunctorM AddTop where 
  {-# INLINE mapM #-}
  mapM f xM = case xM of {Top → return Top;AddTop x → map AddTop $ f x}

-- ===== --
-- AddBT --
-- ===== --

data AddBT a = BotBT | TopBT | AddBT a
  deriving (Eq,Ord,Show)

instance Bot (AddBT a) where 
  {-# INLINE bot #-}
  bot = BotBT
instance (Join a) ⇒ Join (AddBT a) where
  {-# INLINE (⊔) #-}
  BotBT ⊔ x = x
  x ⊔ BotBT = x
  TopBT ⊔ _ = TopBT
  _ ⊔ TopBT = TopBT
  AddBT x ⊔ AddBT y = AddBT $ x ⊔ y
instance Top (AddBT a) where 
  {-# INLINE top #-}
  top = TopBT
instance (Meet a) ⇒ Meet (AddBT a) where
  {-# INLINE (⊓) #-}
  BotBT ⊓ _ = BotBT
  _ ⊓ BotBT = BotBT
  TopBT ⊓ x = x
  x ⊓ TopBT = x
  AddBT x ⊓ AddBT y = AddBT $ x ⊓ y
instance (Join a) ⇒ JoinLattice (AddBT a)
instance (Meet a) ⇒ MeetLattice (AddBT a)
instance (Join a,Meet a) ⇒ Lattice (AddBT a)
instance Functor AddBT where 
  {-# INLINE map #-}
  map = mmap
instance Return AddBT where 
  {-# INLINE return #-}
  return = AddBT
instance Bind AddBT where 
  {-# INLINE (≫=) #-}
  xM ≫= f = case xM of {TopBT → TopBT;BotBT → BotBT;AddBT x → f x}
instance Monad AddBT
instance FunctorM AddBT where 
  {-# INLINE mapM #-}
  mapM f xM = case xM of {TopBT → return TopBT;BotBT → return BotBT;AddBT x → map AddBT $ f x}
