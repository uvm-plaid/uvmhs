module UVMHS.Core.Pointed where

import UVMHS.Core.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data

-- ======= --
-- AddNull --
-- ======= --

data AddNull a = Null | AddNull a
  deriving (Eq,Ord,Show)

elimAddNull ∷ b → (a → b) → AddNull a → b
elimAddNull i f = \case
  Null → i
  AddNull x → f x

instance Null (AddNull a) where 
  null = Null
instance (Append a) ⇒ Append (AddNull a) where
  Null ⧺ x = x
  x ⧺ Null = x
  AddNull x ⧺ AddNull y = AddNull $ x ⧺ y
instance (Append a) ⇒ Monoid (AddNull a)
instance Functor AddNull where 
  map = mmap
instance Return AddNull where 
  return = AddNull
instance Bind AddNull where 
  xM ≫= f = case xM of {Null → Null;AddNull x → f x}
instance Monad AddNull
instance FunctorM AddNull where 
  mapM f xM = case xM of {Null → return Null;AddNull x → map AddNull $ f x}

-- === --
-- ZOM --
-- === --

data ZOM a = NullZOM | OneZOM a | MoreZOM
  deriving (Eq,Ord,Show)

elimZOM ∷ b → (a → b) → b → ZOM a → b
elimZOM i₁ f i₂= \case
  NullZOM → i₁
  OneZOM x → f x
  MoreZOM → i₂

instance Null (ZOM a) where 
  null = NullZOM
instance Append (ZOM a) where
  NullZOM ⧺ x = x
  x ⧺ NullZOM = x
  _ ⧺ _ = MoreZOM
instance Monoid (ZOM a)
instance Functor ZOM where 
  map = mmap
instance Return ZOM where 
  return = OneZOM
instance Bind ZOM where 
  xM ≫= f = case xM of {NullZOM → NullZOM;OneZOM x → f x;MoreZOM → MoreZOM}
instance Monad ZOM
instance FunctorM ZOM where 
  mapM f xM = case xM of {NullZOM → return NullZOM;OneZOM x → map OneZOM $ f x;MoreZOM → return MoreZOM}

instance Single a (ZOM a) where single = OneZOM

-- ====== --
-- AddBot --
-- ====== --

data AddBot a = Bot | AddBot a
  deriving (Eq,Ord,Show)

elimAddBot ∷ b → (a → b) → AddBot a → b
elimAddBot i f = \case
  Bot → i
  AddBot x → f x

instance (POrd a) ⇒ POrd (AddBot a) where
  xT ⊑ yT = case (xT,yT) of
    (Bot,Bot) → True
    (Bot,AddBot _) → True
    (AddBot _,Bot) → False
    (AddBot x,AddBot y) → x ⊑ y
instance Bot (AddBot a) where 
  bot = Bot
instance (Join a) ⇒ Join (AddBot a) where
  Bot ⊔ x = x
  x ⊔ Bot = x
  AddBot x ⊔ AddBot y = AddBot $ x ⊔ y
instance (Top a) ⇒ Top (AddBot a) where 
  top = AddBot top
instance (Meet a) ⇒ Meet (AddBot a) where
  Bot ⊓ _ = Bot
  _ ⊓ Bot = Bot
  AddBot x ⊓ AddBot y = AddBot $ x ⊓ y
instance (Join a) ⇒ JoinLattice (AddBot a)
instance (MeetLattice a) ⇒ MeetLattice (AddBot a)
instance (Join a,MeetLattice a) ⇒ Lattice (AddBot a)
instance Functor AddBot where 
  map = mmap
instance Return AddBot where 
  return = AddBot
instance Bind AddBot where 
  xM ≫= f = case xM of {Bot → Bot;AddBot x → f x}
instance Monad AddBot
instance FunctorM AddBot where 
  mapM f xM = case xM of {Bot → return Bot;AddBot x → map AddBot $ f x}

-- ====== --
-- AddTop --
-- ====== --

data AddTop a = AddTop a | Top
  deriving (Eq,Ord,Show)

elimAddTop ∷ b → (a → b) → AddTop a → b
elimAddTop i f = \case
  Top → i
  AddTop x → f x

instance (POrd a) ⇒ POrd (AddTop a) where
  xT ⊑ yT = case (xT,yT) of
    (Top,Top) → True
    (Top,AddTop _) → False
    (AddTop _,Top) → True
    (AddTop x,AddTop y) → x ⊑ y
instance (Bot a) ⇒ Bot (AddTop a) where 
  bot = AddTop bot
instance (Join a) ⇒ Join (AddTop a) where
  Top ⊔ _ = Top
  _ ⊔ Top = Top
  AddTop x ⊔ AddTop y = AddTop $ x ⊔ y
instance Top (AddTop a) where 
  top = Top
instance (Meet a) ⇒ Meet (AddTop a) where
  Top ⊓ x = x
  x ⊓ Top = x
  AddTop x ⊓ AddTop y = AddTop $ x ⊓ y
instance (JoinLattice a) ⇒ JoinLattice (AddTop a)
instance (Meet a) ⇒ MeetLattice (AddTop a)
instance (JoinLattice a,Meet a) ⇒ Lattice (AddTop a)
instance (Bot a,Difference a) ⇒ Difference (AddTop a) where
  _ ⊟ Top = bot
  Top ⊟ AddTop _ = top
  AddTop x ⊟ AddTop y = AddTop $ x ⊟ y

instance (Null a) ⇒ Null (AddTop a) where
  null = AddTop null
instance (Append a) ⇒ Append (AddTop a) where
  Top ⧺ _ = Top
  _ ⧺ Top = Top
  AddTop x ⧺ AddTop y = AddTop $ x ⧺ y
instance (Monoid a) ⇒ Monoid (AddTop a)

instance (Zero a) ⇒ Zero (AddTop a) where
  zero = AddTop zero
instance (Plus a) ⇒ Plus (AddTop a) where
  Top + _ = Top
  _ + Top = Top
  AddTop x + AddTop y = AddTop $ x + y
instance (Additive a) ⇒ Additive (AddTop a)

instance (Times a) ⇒ Times (AddTop a) where
  Top × _ = Top
  _ × Top = Top
  AddTop x × AddTop y = AddTop $ x × y

instance Functor AddTop where 
  map = mmap
instance Return AddTop where 
  return = AddTop
instance Bind AddTop where 
  xM ≫= f = case xM of {Top → Top;AddTop x → f x}
instance Monad AddTop
instance FunctorM AddTop where 
  mapM f xM = case xM of {Top → return Top;AddTop x → map AddTop $ f x}

-- ===== --
-- AddBT --
-- ===== --

data AddBT a = BotBT | AddBT a | TopBT
  deriving (Eq,Ord,Show)

instance Bot (AddBT a) where 
  bot = BotBT
instance (Join a) ⇒ Join (AddBT a) where
  BotBT ⊔ x = x
  x ⊔ BotBT = x
  TopBT ⊔ _ = TopBT
  _ ⊔ TopBT = TopBT
  AddBT x ⊔ AddBT y = AddBT $ x ⊔ y
instance Top (AddBT a) where 
  top = TopBT
instance (Meet a) ⇒ Meet (AddBT a) where
  BotBT ⊓ _ = BotBT
  _ ⊓ BotBT = BotBT
  TopBT ⊓ x = x
  x ⊓ TopBT = x
  AddBT x ⊓ AddBT y = AddBT $ x ⊓ y
instance (Join a) ⇒ JoinLattice (AddBT a)
instance (Meet a) ⇒ MeetLattice (AddBT a)
instance (Join a,Meet a) ⇒ Lattice (AddBT a)
instance Functor AddBT where 
  map = mmap
instance Return AddBT where 
  return = AddBT
instance Bind AddBT where 
  xM ≫= f = case xM of {TopBT → TopBT;BotBT → BotBT;AddBT x → f x}
instance Monad AddBT
instance FunctorM AddBT where 
  mapM f xM = case xM of {TopBT → return TopBT;BotBT → return BotBT;AddBT x → map AddBT $ f x}

-- LENSES --

addTopL ∷ AddTop a ⌲ a
addTopL = prism AddTop $ \case
  AddTop x → Some x
  _ → None

nullZOML ∷ ZOM a ⌲ ()
nullZOML = prism (const NullZOM) $ \case
  NullZOM → Some ()
  _ → None

oneZOML ∷ ZOM a ⌲ a
oneZOML = prism OneZOM $ \case
  OneZOM x → Some x
  _ → None

moreZOML ∷ ZOM a ⌲ ()
moreZOML = prism (const MoreZOM) $ \case
  MoreZOM → Some ()
  _ → None

