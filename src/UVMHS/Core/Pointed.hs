module UVMHS.Core.Pointed where

import UVMHS.Init
import UVMHS.Core.Classes

-- ======= --
-- AddNull --
-- ======= --

data AddNull a = Null | AddNull a
  deriving (Eq,Ord)

instance Null (AddNull a) where null = Null
instance (Append a) ⇒ Append (AddNull a) where
  Null ⧺ x = x
  x ⧺ Null = x
  AddNull x ⧺ AddNull y = AddNull $ x ⧺ y
instance (Append a) ⇒ Monoid (AddNull a)
instance Functor AddNull where map = mmap
instance Return AddNull where return = AddNull
instance Bind AddNull where xM ≫= f = case xM of {Null → Null;AddNull x → f x}
instance Monad AddNull
instance FunctorM AddNull where mapM f xM = case xM of {Null → return Null;AddNull x → map AddNull $ f x}

-- ====== --
-- AddBot --
-- ====== --

data AddBot a = Bot | AddBot a
  deriving (Eq,Ord)

elimAddBot ∷ b → (a → b) → AddBot a → b
elimAddBot i f = \case
  Bot → i
  AddBot x → f x

instance Bot (AddBot a) where bot = Bot
instance (Join a) ⇒ Join (AddBot a) where
  Bot ⊔ x = x
  x ⊔ Bot = x
  AddBot x ⊔ AddBot y = AddBot $ x ⊔ y
instance (Top a) ⇒ Top (AddBot a) where top = AddBot top
instance (Meet a) ⇒ Meet (AddBot a) where
  Bot ⊓ _ = Bot
  _ ⊓ Bot = Bot
  AddBot x ⊓ AddBot y = AddBot $ x ⊓ y
instance (Join a) ⇒ JoinLattice (AddBot a)
instance (MeetLattice a) ⇒ MeetLattice (AddBot a)
instance (Join a,MeetLattice a) ⇒ Lattice (AddBot a)
instance Functor AddBot where map = mmap
instance Return AddBot where return = AddBot
instance Bind AddBot where xM ≫= f = case xM of {Bot → Bot;AddBot x → f x}
instance Monad AddBot
instance FunctorM AddBot where mapM f xM = case xM of {Bot → return Bot;AddBot x → map AddBot $ f x}

-- ====== --
-- AddTop --
-- ====== --

data AddTop a = Top | AddTop a
  deriving (Eq,Ord)

elimAddTop ∷ b → (a → b) → AddTop a → b
elimAddTop i f = \case
  Top → i
  AddTop x → f x

instance (Null a) ⇒ Null (AddTop a) where null = AddTop null
instance (Append a) ⇒ Append (AddTop a) where
  Top ⧺ _ = Top
  _ ⧺ Top = Top
  AddTop x ⧺ AddTop y = AddTop (x ⧺ y)
instance (Monoid a) ⇒ Monoid (AddTop a)

instance (Bot a) ⇒ Bot (AddTop a) where bot = AddTop bot
instance (Join a) ⇒ Join (AddTop a) where
  Top ⊔ _ = Top
  _ ⊔ Top = Top
  AddTop x ⊔ AddTop y = AddTop $ x ⊔ y
instance Top (AddTop a) where top = Top
instance (Meet a) ⇒ Meet (AddTop a) where
  Top ⊓ x = x
  x ⊓ Top = x
  AddTop x ⊓ AddTop y = AddTop $ x ⊓ y
instance (JoinLattice a) ⇒ JoinLattice (AddTop a)
instance (Meet a) ⇒ MeetLattice (AddTop a)
instance (JoinLattice a,Meet a) ⇒ Lattice (AddTop a)
instance Functor AddTop where map = mmap
instance Return AddTop where return = AddTop
instance Bind AddTop where xM ≫= f = case xM of {Top → Top;AddTop x → f x}
instance Monad AddTop
instance FunctorM AddTop where mapM f xM = case xM of {Top → return Top;AddTop x → map AddTop $ f x}

-- ===== --
-- AddBT --
-- ===== --

data AddBT a = BotBT | TopBT | AddBT a
  deriving (Eq,Ord)

instance Bot (AddBT a) where bot = BotBT
instance (Join a) ⇒ Join (AddBT a) where
  BotBT ⊔ x = x
  x ⊔ BotBT = x
  TopBT ⊔ _ = TopBT
  _ ⊔ TopBT = TopBT
  AddBT x ⊔ AddBT y = AddBT $ x ⊔ y
instance Top (AddBT a) where top = TopBT
instance (Meet a) ⇒ Meet (AddBT a) where
  BotBT ⊓ _ = BotBT
  _ ⊓ BotBT = BotBT
  TopBT ⊓ x = x
  x ⊓ TopBT = x
  AddBT x ⊓ AddBT y = AddBT $ x ⊓ y
instance (Join a) ⇒ JoinLattice (AddBT a)
instance (Meet a) ⇒ MeetLattice (AddBT a)
instance (Join a,Meet a) ⇒ Lattice (AddBT a)
instance Functor AddBT where map = mmap
instance Return AddBT where return = AddBT
instance Bind AddBT where xM ≫= f = case xM of {TopBT → TopBT;BotBT → BotBT;AddBT x → f x}
instance Monad AddBT
instance FunctorM AddBT where mapM f xM = case xM of {TopBT → return TopBT;BotBT → return BotBT;AddBT x → map AddBT $ f x}

-- EFFECTS --

-- ---------
-- -- Top --
-- ---------
-- 
-- newtype TopT m a = TopT { unTopT ∷ m (AddTop a) }
-- 
-- instance (Functor m) ⇒ Functor (TopT m) where 
--   map ∷ ∀ a b. (a → b) → TopT m a → TopT m b
--   map f xM = TopT $ map (map f) $ unTopT xM
-- 
-- instance (Return m) ⇒ Return (TopT m) where
--   return ∷ ∀ a. a → TopT m a
--   return x = TopT $ return $ AddTop x
-- instance (Monad m) ⇒ Bind (TopT m) where
--   (≫=) ∷ ∀ a b. TopT m a → (a → TopT m b) → TopT m b
--   xM ≫= k = TopT $ do
--     xO ← unTopT xM
--     case xO of
--       Top → return Top
--       AddTop x → unTopT $ k x
-- instance (Monad m) ⇒ Monad (TopT m)
-- instance Functorial Monad TopT where functorial = W
-- 
-- instance Functor2 TopT where
--   map2 ∷ ∀ m₁ m₂. (∀ a. m₁ a → m₂ a) → (∀ a. TopT m₁ a → TopT m₂ a) 
--   map2 f xM = TopT $ f $ unTopT xM
-- 
-- instance (Return m) ⇒ MonadTop (TopT m) where
--   mtop ∷ ∀ a. TopT m a
--   mtop = TopT $ return Top
-- 
-- instance (Functorial Null m,Null a) ⇒ Null (TopT m a) where
--   null ∷ TopT m a
--   null = case functorial ∷ W (Null (m (AddTop a))) of
--     W → TopT null
-- instance (Functorial Append m,Append a) ⇒ Append (TopT m a) where
--   (⧺) ∷ TopT m a → TopT m a → TopT m a
--   (⧺) = case functorial ∷ W (Append (m (AddTop a))) of
--     W → \ xM₁ xM₂ → TopT $ unTopT xM₁ ⧺ unTopT xM₂
-- instance (MonoidFunctorial m,Monoid a) ⇒ Monoid (TopT m a)
-- 
-- instance (Functorial Null m) ⇒ Functorial Null (TopT m) where functorial = W
-- instance (Functorial Append m) ⇒ Functorial Append (TopT m) where functorial = W
-- instance (MonoidFunctorial m) ⇒ Functorial Monoid (TopT m) where functorial = W
-- instance (MonoidFunctorial m) ⇒ MonoidFunctorial (TopT m) 
-- --
-- ---------
-- -- TOP --
-- ---------
-- 
-- instance LiftIO TopT where
--   liftIO ∷ ∀ m. (Monad m) ⇒ (∀ a. IO a → m a) → (∀ a. IO a → TopT m a)
--   liftIO ioM xM = TopT $ do
--     x ← ioM xM
--     return $ AddTop x
-- 
-- instance LiftReader TopT where
--   liftAsk ∷ ∀ m r. (Monad m) ⇒ m r → TopT m r
--   liftAsk askM = TopT $ do
--     r ← askM
--     return $ AddTop r
-- 
--   liftLocal ∷ ∀ m r. (Monad m) ⇒ (∀ a. r → m a → m a) → (∀ a. r → TopT m a → TopT m a)
--   liftLocal localM r xM = TopT $ localM r $ unTopT xM
-- 
-- instance LiftWriter TopT where
--   liftTell ∷ ∀ m o. (Monad m) ⇒ (o → m ()) → (o → TopT m ())
--   liftTell tellM o = TopT $ do
--     tellM o
--     return $ AddTop ()
-- 
--   liftListen ∷ ∀ m o. (Monad m) ⇒ (∀ a. m a → m (o ∧ a)) → (∀ a. TopT m a → TopT m (o ∧ a))
--   liftListen listenM xM = TopT $ do
--     (o :* xO) ← listenM $ unTopT xM
--     case xO of
--       Top → return Top
--       AddTop x → return $ AddTop (o :* x)
-- 
-- instance LiftState TopT where
--   liftGet ∷ ∀ m s. (Monad m) ⇒ m s → TopT m s
--   liftGet getM = TopT $ do
--     s ← getM
--     return $ AddTop s
-- 
--   liftPut ∷ ∀ m s. (Monad m) ⇒ (s → m ()) → (s → TopT m ())
--   liftPut putM s = TopT $ do
--     putM s
--     return $ AddTop ()
-- 
-- instance LiftFail TopT where
--   liftAbort ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. TopT m a)
--   liftAbort abortM = TopT $ abortM
-- 
--   liftTry ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. TopT m a → TopT m a → TopT m a)
--   liftTry tryM xM₁ xM₂ = TopT $ tryM (unTopT xM₁) (unTopT xM₂)
-- 
-- instance LiftError TopT where
--   liftThrow ∷ ∀ e m. (Monad m) ⇒ (∀ a. e → m a) → (∀ a. e → TopT m a)
--   liftThrow throwM e = TopT $ throwM e
--     
--   liftCatch ∷ ∀ e m. (Monad m) ⇒ (∀ a. m a → (e → m a) → m a) → (∀ a. TopT m a → (e → TopT m a) → TopT m a)
--   liftCatch catchM xM k = TopT $ catchM (unTopT xM) $ \ e → unTopT $ k e
-- 
-- instance LiftNondet TopT where
--   liftMzero ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. TopT m a)
--   liftMzero mzeroM = TopT $ mzeroM
-- 
--   liftMplus ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. TopT m a → TopT m a → TopT m a)
--   liftMplus mplusM xM₁ xM₂ = TopT $ mplusM (unTopT xM₁) (unTopT xM₂)
-- 
-- instance LiftTop TopT where
--   liftMtop ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. TopT m a)
--   liftMtop mtopM = TopT $ mtopM
-- 
-- instance (Monad m,MonadCont (AddTop r) m) ⇒ MonadCont r (TopT m) where
--   callCC ∷ ∀ a. ((a → TopT m r) → TopT m r) → TopT m a
--   callCC kk = TopT $
--     callCC $ \ (k ∷ AddTop a → m (AddTop r)) →
--       unTopT $ kk $ \ (x ∷ a) → 
--         TopT $ k (AddTop x)
-- 
--   withC ∷ ∀ a. (a → TopT m r) → TopT m a → TopT m r
--   withC k xM = TopT $
--     withC 
--     (\ (xO ∷ AddTop a) → case xO of
--          Top → return Top
--          AddTop x → unTopT $ k x)
--     (unTopT xM)
-- 
