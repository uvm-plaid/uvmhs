module UVMHS.Core.Monads where

import UVMHS.Core.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data

import UVMHS.Core.Effects
import UVMHS.Core.Transformers

import qualified Prelude as HS

newtype MU m = MU { unMU ∷ m () }
instance (Return m) ⇒ Null (MU m) where null = MU $ return ()
instance (Bind m) ⇒ Append (MU m) where x ⧺ y = MU $ unMU x ≫ unMU y
instance (Monad m) ⇒ Monoid (MU m)

instance MonadIO IO where 
  io = id

instance Functor IO where 
  map = mmap
instance Return IO where 
  return = HS.return
instance Bind IO where 
  (≫=) = (HS.>>=)
instance Monad IO

newtype ID a = ID { unID ∷ a }
  deriving 
  (Null,Append,Monoid
  ,Bot,Join,JoinLattice
  ,Top,Meet,MeetLattice
  ,Lattice,Dual,Difference)

instance Functor ID where 
  map = mmap
instance Return ID where
  return ∷ ∀ a. a → ID a
  return = ID
instance Bind ID where
  (≫=) ∷ ∀ a b. ID a → (a → ID b) → ID b
  x ≫= f = f $ unID x
instance Monad ID

instance Extract ID where
  extract ∷ ∀ a. ID a → a
  extract = unID
instance Cobind ID where
  (=≫) ∷ ∀ a b. ID a → (ID a → b) → ID b
  xM =≫ f = ID $ f xM
instance Comonad ID

------------
-- READER --
------------

newtype ReaderT r m a = ReaderT { unReaderT ∷ r → m a }

runReaderT ∷ ∀ r m a. r → ReaderT r m a → m a
runReaderT r xM = unReaderT xM r

instance (Functor m) ⇒ Functor (ReaderT r m) where 
  map ∷ ∀ a b. (a → b) → ReaderT r m a → ReaderT r m b
  map f = ReaderT ∘ map (map f) ∘ unReaderT
instance (Return m) ⇒ Return (ReaderT r m) where
  return ∷ ∀ a. a → ReaderT r m a
  return x = ReaderT $ \ _ → return x
instance (Bind m) ⇒ Bind (ReaderT r m) where
  (≫=) ∷ ∀ a b. ReaderT r m a → (a → ReaderT r m b) → ReaderT r m b
  xM ≫= k = ReaderT $ \ r → do
    x ← unReaderT xM r
    unReaderT (k x) r
instance (Monad m) ⇒ Monad (ReaderT r m)

instance Functor2 (ReaderT r) where
  map2 ∷ ∀ m₁ m₂. (∀ a. m₁ a → m₂ a) → (∀ a. ReaderT r m₁ a → ReaderT r m₂ a)
  map2 f = ReaderT ∘ map f ∘ unReaderT

instance (Monad m) ⇒ MonadReader r (ReaderT r m) where
  ask ∷ ReaderT r m r
  ask = ReaderT $ \ r → return r

  local ∷ ∀ a. r → ReaderT r m a → ReaderT r m a
  local r xM = ReaderT $ \ _ → unReaderT xM r

instance (∀ a'. Null a' ⇒ Null (m a'),Null a) ⇒ Null (ReaderT r m a) where
  null ∷ ReaderT r m a
  null = ReaderT $ \ _ → null
instance (∀ a'. Append a' ⇒ Append (m a'),Append a) ⇒ Append (ReaderT r m a) where
  (⧺) ∷ ReaderT r m a → ReaderT r m a → ReaderT r m a
  (⧺) xM₁ xM₂ = ReaderT $ \ r → unReaderT xM₁ r ⧺ unReaderT xM₂ r

instance Transformer (ReaderT r) where
  lift ∷ ∀ m a. (Monad m) ⇒ m a → ReaderT r m a
  lift xM = ReaderT $ \ _ → xM

------------
-- WRITER --
------------

newtype WriterT o m a = WriterT { unWriterT ∷ m (o ∧ a) }

evalWriterT ∷ ∀ o m a. (Functor m) ⇒ WriterT o m a → m a
evalWriterT = map snd ∘ unWriterT

instance (Functor m) ⇒ Functor (WriterT o m) where 
  map ∷ ∀ a b. (a → b) → WriterT o m a → WriterT o m b
  map f = WriterT ∘ map (map f) ∘ unWriterT

instance (Return m,Null o) ⇒ Return (WriterT o m) where
  return ∷ ∀ a. a → WriterT o m a
  return x = WriterT $ return (null :* x)
instance (Monad m,Append o) ⇒ Bind (WriterT o m) where
  (≫=) ∷ ∀ a b. WriterT o m a → (a → WriterT o m b) → WriterT o m b
  xM ≫= k = WriterT $ do
    (o₁ :* x) ← unWriterT xM
    (o₂ :* y) ← unWriterT $ k x
    return ((o₁ ⧺ o₂) :* y)
instance (Monad m,Monoid o) ⇒ Monad (WriterT o m)

instance (Monoid o) ⇒ Functor2 (WriterT o) where
  map2 ∷ ∀ m₁ m₂. (∀ a. m₁ a → m₂ a) → (∀ a. WriterT o m₁ a → WriterT o m₂ a)
  map2 f = WriterT ∘ f ∘ unWriterT

instance (Monad m,Null o) ⇒ MonadWriter o (WriterT o m) where
  tell ∷ o → WriterT o m ()
  tell o = WriterT $ return (o :* ())

  hijack ∷ ∀ a. WriterT o m a → WriterT o m (o ∧ a)
  hijack xM = WriterT $ do
    oa ← unWriterT xM
    return $ null :* oa

instance (∀ a'. Null a' ⇒ Null (m a'),Null o,Null a) ⇒ Null (WriterT o m a) where
  null ∷ WriterT o m a
  null = WriterT null
instance (∀ a'. Append a' ⇒ Append (m a'),Append o,Append a) ⇒ Append (WriterT o m a) where
  (⧺) ∷ WriterT o m a → WriterT o m a → WriterT o m a
  xM₁ ⧺ xM₂ = WriterT $ unWriterT xM₁ ⧺ unWriterT xM₂
instance 
  (∀ a'. Null a' ⇒ Null (m a')
  ,∀ a'. Append a' ⇒ Append (m a')
  ,∀ a'. Monoid a' ⇒ Monoid (m a')
  ,Monoid o,Monoid a) 
  ⇒ Monoid (WriterT o m a)

instance (Null o) ⇒ Transformer (WriterT o) where
  lift ∷ ∀ m a. (Monad m) ⇒ m a → WriterT o m a
  lift xM = WriterT $ (null :*) ^$ xM

-----------
-- STATE --
-----------

newtype StateT s m a = StateT { unStateT ∷ s → m (s ∧ a) }

runStateT ∷ ∀ s m a. s → StateT s m a → m (s ∧ a)
runStateT s xM = unStateT xM s

evalStateT ∷ ∀ s m a. (Functor m) ⇒ s → StateT s m a → m a
evalStateT s = map snd ∘ runStateT s

instance (Functor m) ⇒ Functor (StateT s m) where 
  map ∷ ∀ a b. (a → b) → StateT s m a → StateT s m b
  map f = StateT ∘ map (map (map f)) ∘ unStateT

instance (Return m) ⇒ Return (StateT s m) where
  return ∷ ∀ a. a → StateT s m a
  return x = StateT $ \ s → return (s :* x)
instance (Bind m) ⇒ Bind (StateT s m) where
  (≫=) ∷ ∀ a b. StateT s m a → (a → StateT s m b) → StateT s m b
  xM ≫= k = StateT $ \ s → do
    (s' :* x) ← unStateT xM s
    unStateT (k x) s'
instance (Monad m) ⇒ Monad (StateT s m)

instance Functor2 (StateT s) where
  map2 ∷ ∀ m₁ m₂. (∀ a. m₁ a → m₂ a) → (∀ a. StateT s m₁ a → StateT s m₂ a)
  map2 f = StateT ∘ map f ∘ unStateT

instance (Return m) ⇒ MonadState s (StateT s m) where
  get ∷ StateT s m s
  get = StateT $ \ s → return (s :* s)
  
  put ∷ s → StateT s m ()
  put s = StateT $ \ _ → return (s :* ())

instance (∀ a'. Null a' ⇒ Null (m a'),Null s,Null a) ⇒ Null (StateT s m a) where
  null ∷ StateT s m a
  null = StateT $ \ _ → null
instance (∀ a'. Append a' ⇒ Append (m a'),Append s,Append a) ⇒ Append (StateT s m a) where
  (⧺) ∷ StateT s m a → StateT s m a → StateT s m a
  xM₁ ⧺ xM₂ = StateT $ \ s → unStateT xM₁ s ⧺ unStateT xM₂ s
instance 
  (∀ a'. Null a' ⇒ Null (m a')
  ,∀ a'. Append a' ⇒ Append (m a')
  ,∀ a'. Monoid a' ⇒ Monoid (m a')
  ,Monoid s,Monoid a) 
  ⇒ Monoid (StateT s m a)

type State s = StateT s ID

mkState ∷ (s → s ∧ a) → State s a
mkState f = StateT $ ID ∘ f

runState ∷ s → State s a → (s ∧ a)
runState s = unID ∘ runStateT s

evalState ∷ s → State s a → a
evalState s = unID ∘ evalStateT s

instance Transformer (StateT s) where
  lift ∷ ∀ m a. (Monad m) ⇒ m a → StateT s m a
  lift xM = StateT $ \ s → (s :*) ^$ xM

----------
-- FAIL --
----------

newtype FailT m a = FailT { unFailT ∷ m (𝑂 a) }

instance (Functor m) ⇒ Functor (FailT m) where 
  map ∷ ∀ a b. (a → b) → FailT m a → FailT m b
  map f = FailT ∘ map (map f) ∘ unFailT

instance (Return m) ⇒ Return (FailT m) where
  return ∷ ∀ a. a → FailT m a
  return x = FailT $ return $ Some x
instance (Monad m) ⇒ Bind (FailT m) where
  (≫=) ∷ ∀ a b. FailT m a → (a → FailT m b) → FailT m b
  xM ≫= k = FailT $ do
    xO ← unFailT xM
    case xO of
      None → return None
      Some x → unFailT $ k x
instance (Monad m) ⇒ Monad (FailT m)

instance Functor2 FailT where
  map2 ∷ ∀ m₁ m₂. (∀ a. m₁ a → m₂ a) → (∀ a. FailT m₁ a → FailT m₂ a) 
  map2 f = FailT ∘ f ∘ unFailT

instance (Monad m) ⇒ MonadFail (FailT m) where
  abort ∷ ∀ a. FailT m a
  abort = FailT $ return None

  (⎅) ∷ ∀ a. FailT m a → FailT m a → FailT m a
  xM₁ ⎅ xM₂ = FailT $ do
    xO₁ ← unFailT xM₁
    case xO₁ of
      None → unFailT xM₂
      Some x → return $ Some x

instance (∀ a'. Null a' ⇒ Null (m a'),Null a) ⇒ Null (FailT m a) where
  null ∷ FailT m a
  null = FailT null
instance (∀ a'. Append a' ⇒ Append (m a'),Append a) ⇒ Append (FailT m a) where
  (⧺) ∷ FailT m a → FailT m a → FailT m a
  xM₁ ⧺ xM₂ = FailT $ unFailT xM₁ ⧺ unFailT xM₂
instance 
  (∀ a'. Null a' ⇒ Null (m a')
  ,∀ a'. Append a' ⇒ Append (m a')
  ,∀ a'. Monoid a' ⇒ Monoid (m a')
  ,Monoid a) 
  ⇒ Monoid (FailT m a)

instance Transformer FailT where
  lift ∷ ∀ m a. (Monad m) ⇒ m a → FailT m a
  lift xM = FailT $ Some ^$ xM

-----------
-- ERROR --
-----------

newtype ErrorT e m a = ErrorT { unErrorT ∷ m (e ∨ a) }

instance (Functor m) ⇒ Functor (ErrorT e m) where
  map ∷ ∀ a b. (a → b) → ErrorT e m a → ErrorT e m b
  map f = ErrorT ∘ map (map f) ∘ unErrorT

instance (Return m) ⇒ Return (ErrorT e m) where
  return ∷ ∀ a. a → ErrorT e m a
  return x = ErrorT $ return $ Inr x
instance (Monad m) ⇒ Bind (ErrorT e m) where
  (≫=) ∷ ∀ a b. ErrorT e m a → (a → ErrorT e m b) → ErrorT e m b
  xM ≫= k = ErrorT $ do
    ex ← unErrorT xM
    case ex of
      Inl e → return $ Inl e
      Inr x → unErrorT $ k x
instance (Monad m) ⇒ Monad (ErrorT e m)

instance Functor2 (ErrorT e) where
  map2 ∷ ∀ m₁ m₂. (∀ a. m₁ a → m₂ a) → (∀ a. ErrorT e m₁ a → ErrorT e m₂ a)
  map2 f = ErrorT ∘ f ∘ unErrorT

instance (Monad m) ⇒ MonadError e (ErrorT e m) where
  throw ∷ ∀ a. e → ErrorT e m a
  throw e = ErrorT $ return $ Inl e

  catch ∷ ∀ a. ErrorT e m a → (e → ErrorT e m a) → ErrorT e m a
  catch xM k = ErrorT $ do
    ex ← unErrorT xM
    case ex of
      Inl e → unErrorT $ k e
      Inr x → return $ Inr x

instance (∀ a'. Null a' ⇒ Null (m a'),Null a) ⇒ Null (ErrorT e m a) where
  null ∷ ErrorT e m a
  null = ErrorT null
instance (∀ a'. Append a' ⇒ Append (m a'),Append e,Append a) ⇒ Append (ErrorT e m a) where
  (⧺) ∷ ErrorT e m a → ErrorT e m a → ErrorT e m a
  xM₁ ⧺ xM₂ = ErrorT $ unErrorT xM₁ ⧺ unErrorT xM₂
instance 
  (∀ a'. Null a' ⇒ Null (m a')
  ,∀ a'. Append a' ⇒ Append (m a')
  ,∀ a'. Monoid a' ⇒ Monoid (m a')
  ,Append e,Monoid a) 
  ⇒ Monoid (ErrorT e m a)

instance Transformer (ErrorT e) where
  lift ∷ ∀ m a. (Monad m) ⇒ m a → ErrorT e m a
  lift xM = ErrorT $ Inr ^$ xM

------------
-- NONDET --
------------

newtype NondetT m a = NondetT { unNondetT ∷ m (𝑄 a) }

instance (Functor m) ⇒ Functor (NondetT m) where 
  map ∷ ∀ a b. (a → b) → NondetT m a → NondetT m b
  map f xM = NondetT $ map (map f) $ unNondetT xM

instance (Return m) ⇒ Return (NondetT m) where
  return ∷ ∀ a. a → NondetT m a
  return x = NondetT $ return $ single x
instance (Bind m,∀ a'. Monoid a' ⇒ Monoid (m a')) ⇒ Bind (NondetT m) where
  (≫=) ∷ ∀ a b. NondetT m a → (a → NondetT m b) → NondetT m b
  xM ≫= k = NondetT $ do
    xs ← unNondetT xM
    unNondetT $ foldr mzero (⊞) $ map k $ iter xs
instance (Monad m,∀ a'. Monoid a' ⇒ Monoid (m a')) ⇒ Monad (NondetT m)

instance (∀ a'. Monoid a' ⇒ Monoid (m a')) ⇒ MonadNondet (NondetT m) where
  mzero ∷ ∀ a. NondetT m a
  mzero = NondetT $ null

  (⊞) ∷ ∀ a. NondetT m a → NondetT m a → NondetT m a
  xM₁ ⊞ xM₂ = NondetT $ unNondetT xM₁ ⧺ unNondetT xM₂

instance Transformer NondetT where
  lift ∷ ∀ m a. (Monad m) ⇒ m a → NondetT m a
  lift xM = NondetT $ single ^$ xM

----------
-- Cont --
----------

newtype ContT r m a = ContT { unContT ∷ (a → m r) → m r }

runContT ∷ (a → m r) → ContT r m a → m r
runContT = flip unContT

evalContT ∷ (Return m) ⇒ ContT r m r → m r
evalContT = runContT return

instance Functor (ContT r m) where
  map ∷ ∀ a b. (a → b) → ContT r m a → ContT r m b
  map f xM = ContT $ \ (k ∷ b → m r) → unContT xM $ \ x → k $ f x

instance Return (ContT r m) where
  return ∷ ∀ a. a → ContT r m a
  return x = ContT $ \ (k ∷ a → m r) → k x
instance Bind (ContT r m) where
  (≫=) ∷ ∀ a b. ContT r m a → (a → ContT r m b) → ContT r m b
  xM ≫= kk = ContT $ \ (k ∷ b → m r) → unContT xM $ \ (x ∷ a) → unContT (kk x) k
instance Monad (ContT r m)

instance Functor2Iso (ContT r) where
  map2iso ∷ ∀ m₁ m₂. Iso2 m₁ m₂ → ∀ a. ContT r m₁ a → ContT r m₂ a
  map2iso i xM = ContT $ \ (k ∷ a → m₂ r) → 
    ito2 i $ unContT xM $ \ (x ∷ a) → 
      ifr2 i $ k x

instance (Monad m) ⇒ MonadCont r (ContT r m) where
  callCC ∷ ∀ a. ((a → ContT r m r) → ContT r m r) → ContT r m a
  callCC kk = ContT $ \ (k ∷ a → m r) → 
    runContT return $ kk $ \ (x ∷ a) → 
      ContT $ \ (k' ∷ r → m r) → 
        k' *$ k x

  withC ∷ ∀ a. (a → ContT r m r) → ContT r m a → ContT r m r
  withC k₁ xM = ContT $ \ (k₂ ∷ r → m r) →
    k₂ *$ unContT xM $ \ (x ∷ a) → 
      runContT return $ k₁ x

instance (∀ a'. Null a' ⇒ Null (m a'),Null r) ⇒ Null (ContT r m a) where
  null ∷ ContT r m a
  null = ContT $ \ (_ ∷ a → m r) → null
instance (∀ a'. Append a' ⇒ Append (m a'),Append r) ⇒ Append (ContT r m a) where
  (⧺) ∷ ContT r m a → ContT r m a → ContT r m a
  xM₁ ⧺ xM₂ = ContT $ \ (k ∷ a → m r) → unContT xM₁ k ⧺ unContT xM₂ k
instance 
  (∀ a'. Null a' ⇒ Null (m a')
  ,∀ a'. Append a' ⇒ Append (m a')
  ,∀ a'. Monoid a' ⇒ Monoid (m a')
  ,Monoid r) 
  ⇒ Monoid (ContT r m a)

instance Transformer (ContT r) where
  lift ∷ ∀ m a. (Monad m) ⇒ m a → ContT r m a
  lift xM = ContT $ \ (κ ∷ a → m r) → κ *$ xM

-----------
-- UCont --
-----------

newtype UContT m a = UContT { unUContT ∷ ∀ u. (a → m u) → m u }

runUContT ∷ ∀ u m a. (a → m u) → UContT m a → m u
runUContT = flip unUContT

evalUContT ∷ (Return m) ⇒ UContT m a → m a
evalUContT = runUContT return

instance Functor (UContT m) where
  map ∷ ∀ a b. (a → b) → UContT m a → UContT m b
  map f xM = UContT HS.$ \ (k ∷ b → m u) → unUContT xM $ \ x → k $ f x

instance Return (UContT m) where
  return ∷ ∀ a. a → UContT m a
  return x = UContT HS.$ \ (k ∷ a → m u) → k x
instance Bind (UContT m) where
  (≫=) ∷ ∀ a b. UContT m a → (a → UContT m b) → UContT m b
  xM ≫= kk = UContT HS.$ \ (k ∷ b → m u) → unUContT xM $ \ (x ∷ a) → unUContT (kk x) k
instance Monad (UContT m)

instance Functor2Iso UContT where
  map2iso ∷ ∀ m₁ m₂. Iso2 m₁ m₂ → ∀ a. UContT m₁ a → UContT m₂ a
  map2iso i xM = UContT HS.$ \ (k ∷ a → m₂ u) → 
    ito2 i $ unUContT xM $ \ (x ∷ a) → 
      ifr2 i $ k x

instance (Monad m) ⇒ MonadUCont (UContT m) where
  ucallCC ∷ ∀ a. (∀ u. (a → UContT m u) → UContT m u) → UContT m a
  ucallCC ff = UContT HS.$ \ (𝓀 ∷ a → m u₁) → 
    evalUContT $ ff $ \ (x ∷ a) → 
      UContT HS.$ \ (𝓀' ∷ u₁ → m u₂) → 
        𝓀' *$ 𝓀 x

  uwithC ∷ ∀ a u. (a → UContT m u) → UContT m a → UContT m u
  uwithC f xM = UContT HS.$ \ (𝓀 ∷ u → m u₁) →
    𝓀 *$ unUContT xM $ \ (x ∷ a) → 
      evalUContT $ f x

instance (∀ a'. Null (m a')) ⇒ Null (UContT m a) where
  null ∷ UContT m a
  null = UContT HS.$ \ (_ ∷ a → m u) → null
instance (∀ a'. Append (m a')) ⇒ Append (UContT m a) where
  (⧺) ∷ UContT m a → UContT m a → UContT m a
  xM₁ ⧺ xM₂ = UContT HS.$ \ (𝓀 ∷ a → m u) → unUContT xM₁ 𝓀 ⧺ unUContT xM₂ 𝓀
instance 
  ( ∀ a'. Null (m a')
  , ∀ a'. Append (m a')
  , ∀ a'. Monoid (m a')
  ) ⇒ Monoid (UContT m a)

instance Transformer UContT where
  lift ∷ ∀ m a. (Monad m) ⇒ m a → UContT m a
  lift xM = UContT HS.$ \ (𝓀 ∷ a → m u) → 𝓀 *$ xM

-----------
-- NoBad --
-----------

newtype NoBad a = NoBad { unNoBad ∷ a }
  deriving 
  (Null,Append,Monoid
  ,Bot,Join,JoinLattice
  ,Top,Meet,MeetLattice
  ,Lattice,Dual,Difference)

instance Functor NoBad where 
  map = mmap
instance Return NoBad where
  return ∷ ∀ a. a → NoBad a
  return = NoBad
instance Bind NoBad where
  (≫=) ∷ ∀ a b. NoBad a → (a → NoBad b) → NoBad b
  x ≫= f = f $ unNoBad x
instance Monad NoBad

instance Extract NoBad where
  extract ∷ ∀ a. NoBad a → a
  extract = unNoBad
instance Cobind NoBad where
  (=≫) ∷ ∀ a b. NoBad a → (NoBad a → b) → NoBad b
  xM =≫ f = NoBad $ f xM
instance Comonad NoBad

instance MonadBad NoBad where
  bad = error "<nobad>"

-- ================= --
-- AUTOMATIC LIFTING --
-- ================= --

------------
-- READER --
------------

instance LiftIO (ReaderT r) where
  liftIO ∷ ∀ m. (Monad m) ⇒ (∀ a. IO a → m a) → (∀ a. IO a → ReaderT r m a)
  liftIO ioM xM = ReaderT $ \ _ → ioM xM

instance LiftReader (ReaderT r) where
  liftAsk ∷ ∀ m r'. (Monad m) ⇒ m r' → ReaderT r m r'
  liftAsk askM = ReaderT $ \ _ → askM

  liftLocal ∷ ∀ m r'. (Monad m) ⇒ (∀ a. r' → m a → m a) → (∀ a. r' → ReaderT r m a → ReaderT r m a)
  liftLocal localM r' xM = ReaderT $ \ r → localM r' $ unReaderT xM r

instance LiftWriter (ReaderT r) where
  liftTell ∷ ∀ m o. (Monad m) ⇒ (o → m ()) → (o → ReaderT r m ())
  liftTell tellM o = ReaderT $ \ _ → tellM o

  liftHijack ∷ ∀ m o. (Monad m) ⇒ (∀ a. m a → m (o ∧ a)) → (∀ a. ReaderT r m a → ReaderT r m (o ∧ a))
  liftHijack hijackM xM = ReaderT $ \ r → hijackM $ unReaderT xM r

instance LiftState (ReaderT r) where
  liftGet ∷ ∀ m s. (Monad m) ⇒ m s → ReaderT r m s
  liftGet getM = ReaderT $ \ _ → getM

  liftPut ∷ ∀ m s. (Monad m) ⇒ (s → m ()) → (s → ReaderT r m ())
  liftPut putM s = ReaderT $ \ _ → putM s

instance LiftFail (ReaderT r) where
  liftAbort ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. ReaderT r m a)
  liftAbort abortM = ReaderT $ \ _ → abortM

  liftTry ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. ReaderT r m a → ReaderT r m a → ReaderT r m a)
  liftTry tryM xM₁ xM₂ = ReaderT $ \ r → tryM (unReaderT xM₁ r) (unReaderT xM₂ r)

instance LiftError (ReaderT r) where
  liftThrow ∷ ∀ m e. (Monad m) ⇒ (∀ a. e → m a) → (∀ a. e → ReaderT r m a)
  liftThrow throwM e = ReaderT $ \ _ → throwM e

  liftCatch ∷ ∀ m e. (Monad m) ⇒ (∀ a. m a → (e → m a) → m a) → (∀ a. ReaderT r m a → (e → ReaderT r m a) → ReaderT r m a)
  liftCatch catchM xM k = ReaderT $ \ r → catchM (unReaderT xM r) (\ e → unReaderT (k e) r)

instance LiftNondet (ReaderT r) where
  liftMzero ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. ReaderT r m a)
  liftMzero mzeroM = ReaderT $ \ _ → mzeroM

  liftMplus ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. ReaderT r m a → ReaderT r m a → ReaderT r m a)
  liftMplus mplusM xM₁ xM₂ = ReaderT $ \ r → mplusM (unReaderT xM₁ r) (unReaderT xM₂ r)
    
instance LiftTop (ReaderT r) where
  liftMtop ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. ReaderT r m a)
  liftMtop mtopM = ReaderT $ \ _ → mtopM

instance LiftCont (ReaderT r) where
  liftCallCC ∷ ∀ m r'. (Monad m) ⇒ (∀ a. ((a → m r') → m r') → m a) → (∀ a. ((a → ReaderT r m r') → ReaderT r m r') → ReaderT r m a)
  liftCallCC callCCM kk = ReaderT $ \ r → 
    callCCM $ \ (k ∷ a → m r') → 
      runReaderT r $ kk $ \ (x ∷ a) → 
        ReaderT $ \ _ → 
          k x
  liftWithC ∷ ∀ m r'. (Monad m) ⇒ (∀ a. (a → m r') → m a → m r') → (∀ a. (a → ReaderT r m r') → ReaderT r m a → ReaderT r m r')
  liftWithC withCM k xM = ReaderT $ \ r →
    flip withCM (unReaderT xM r) $ \ x → runReaderT r $ k x

------------
-- WRITER --
------------

instance (Null o) ⇒ LiftIO (WriterT o) where
  liftIO ∷ ∀ m. (Monad m) ⇒ (∀ a. IO a → m a) → (∀ a. IO a → WriterT o m a)
  liftIO ioM xM = WriterT $ do
    x ← ioM xM
    return (null :* x)

instance (Null o) ⇒ LiftReader (WriterT o) where
  liftAsk ∷ ∀ m r. (Monad m) ⇒ m r → WriterT o m r
  liftAsk askM = WriterT $ do
    r ← askM
    return (null :* r)

  liftLocal ∷ ∀ m r. (Monad m) ⇒ (∀ a. r → m a → m a) → (∀ a. r → WriterT o m a → WriterT o m a)
  liftLocal localM r xM = WriterT $ localM r $ unWriterT xM
    
instance (Null o) ⇒ LiftWriter (WriterT o) where
  liftTell ∷ ∀ m o'. (Monad m) ⇒ (o' → m ()) → (o' → WriterT o m ())
  liftTell tellM o' = WriterT $ do
    tellM o'
    return (null :* ())

  liftHijack ∷ ∀ m o'. (Monad m) ⇒ (∀ a. m a → m (o' ∧ a)) → (∀ a. WriterT o m a → WriterT o m (o' ∧ a))
  liftHijack hijackM xM = WriterT $ do
    (o' :* (o :* a)) ← hijackM $ unWriterT xM
    return (o :* (o' :* a))

instance (Null o) ⇒ LiftState (WriterT o) where
  liftGet ∷ ∀ m s. (Monad m) ⇒ m s → WriterT o m s
  liftGet getM = WriterT $ do
    s ← getM
    return (null :* s)

  liftPut ∷ ∀ m s. (Monad m) ⇒ (s → m ()) → (s → WriterT o m ())
  liftPut putM s = WriterT $ do
    putM s
    return (null :* ())

instance LiftFail (WriterT o) where
  liftAbort ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. WriterT o m a)
  liftAbort abortM = WriterT abortM

  liftTry ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. WriterT o m a → WriterT o m a → WriterT o m a)
  liftTry tryM xM₁ xM₂ = WriterT $ tryM (unWriterT xM₁) (unWriterT xM₂)

instance LiftError (WriterT o) where
  liftThrow ∷ ∀ m e. (Monad m) ⇒ (∀ a. e → m a) → (∀ a. e → WriterT o m a)
  liftThrow throwM e = WriterT $ throwM e

  liftCatch ∷ ∀ m e. (Monad m) ⇒ (∀ a. m a → (e → m a) → m a) → (∀ a. WriterT o m a → (e → WriterT o m a) → WriterT o m a)
  liftCatch catchM xM k = WriterT $ catchM (unWriterT xM) $ \ e → unWriterT $ k e

instance LiftNondet (WriterT o) where
  liftMzero ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. WriterT o m a)
  liftMzero mzeroM = WriterT mzeroM

  liftMplus ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. WriterT o m a → WriterT o m a → WriterT o m a)
  liftMplus mplusM xM₁ xM₂ = WriterT $ mplusM (unWriterT xM₁) (unWriterT xM₂)

instance LiftTop (WriterT o) where
  liftMtop ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. WriterT o m a)
  liftMtop mtopM = WriterT mtopM

instance (Monoid o,Monad m,MonadCont (o ∧ r) m) ⇒ MonadCont r (WriterT o m) where
  callCC ∷ ∀ a. ((a → WriterT o m r) → WriterT o m r) → WriterT o m a
  callCC kk = WriterT $ callCC $ \ (k ∷ (o ∧ a) → m (o ∧ r)) →
    unWriterT $ kk $ \ (x ∷ a) → 
      WriterT $ k $ null :* x

  withC ∷ ∀ a. (a → WriterT o m r) → WriterT o m a → WriterT o m r
  withC k xM = WriterT $ 
    withCOn (unWriterT xM) $ \ (o₁ :* x ∷ o ∧ a) → do 
      o₂ :* r ← unWriterT $ k x
      return $ (o₁ ⧺ o₂) :* r

-----------
-- STATE --
-----------

instance LiftIO (StateT s) where
  liftIO ∷ ∀ m. (Monad m) ⇒ (∀ a. IO a → m a) → (∀ a. IO a → StateT s m a)
  liftIO ioM xM = StateT $ \ s → do
    x ← ioM xM
    return (s :* x)

instance LiftReader (StateT s) where
  liftAsk ∷ ∀ m r. (Monad m) ⇒ m r → StateT s m r
  liftAsk askM = StateT $ \ s → do
    r ← askM
    return (s :* r)

  liftLocal ∷ ∀ m r. (Monad m) ⇒ (∀ a. r → m a → m a) → (∀ a. r → StateT s m a → StateT s m a)
  liftLocal localM r xM = StateT $ \ s → localM r $ unStateT xM s

instance LiftWriter (StateT s) where
  liftTell ∷ ∀ m o. (Monad m) ⇒ (o → m ()) → (o → StateT s m ())
  liftTell tellM o = StateT $ \ s → do
    tellM o
    return (s :* ())

  liftHijack ∷ ∀ m o. (Monad m) ⇒ (∀ a. m a → m (o ∧ a)) → (∀ a. StateT s m a → StateT s m (o ∧ a))
  liftHijack hijackM xM = StateT $ \ s → do
    (o :* (s' :* x)) ← hijackM $ unStateT xM s
    return (s' :* (o :* x))

instance LiftState (StateT s) where
  liftGet ∷ ∀ m s'. (Monad m) ⇒ m s' → StateT s m s'
  liftGet getM = StateT $ \ s → do
    s' ← getM
    return (s :* s')

  liftPut ∷ ∀ m s'. (Monad m) ⇒ (s' → m ()) → s' → StateT s m ()
  liftPut putM s' = StateT $ \ s → do
    putM s'
    return (s :* ())

instance LiftFail (StateT s) where
  liftAbort ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. StateT s m a)
  liftAbort abortM = StateT $ \ _ → abortM

  liftTry ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. StateT s m a → StateT s m a → StateT s m a)
  liftTry tryM xM₁ xM₂ = StateT $ \ s → tryM (unStateT xM₁ s) (unStateT xM₂ s)

instance LiftError (StateT s) where
  liftThrow ∷ ∀ m e. (Monad m) ⇒ (∀ a. e → m a) → (∀ a. e → StateT s m a)
  liftThrow throwM e = StateT $ \ _ → throwM e

  liftCatch ∷ ∀ m e. (Monad m) ⇒ (∀ a. m a → (e → m a) → m a) → (∀ a. StateT s m a → (e → StateT s m a) → StateT s m a)
  liftCatch catchM xM k = StateT $ \ s → catchM (unStateT xM s) (\ e → unStateT (k e) s)

instance LiftNondet (StateT s) where
  liftMzero ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. StateT s m a)
  liftMzero mzeroM = StateT $ \ _ → mzeroM

  liftMplus ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. StateT s m a → StateT s m a → StateT s m a)
  liftMplus mplusM xM₁ xM₂ = StateT $ \ s → mplusM (unStateT xM₁ s) (unStateT xM₂ s)

instance LiftTop (StateT s) where
  liftMtop ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. StateT s m a)
  liftMtop mtopM = StateT $ \ _ → mtopM

instance (Monad m,MonadCont (s ∧ u) m) ⇒ MonadCont u (StateT s m) where
  callCC ∷ ∀ a. ((a → StateT s m u) → StateT s m u) → StateT s m a
  callCC ff = StateT $ \ s₁ → 
    callCC $ \ (𝓀 ∷ (s ∧ a) → m (s ∧ u)) →
      runStateT s₁ $ ff $ \ (x ∷ a) → 
        StateT $ \ s₂ →
          𝓀 $ s₂ :* x

  withC ∷ ∀ a. (a → StateT s m u) → StateT s m a → StateT s m u
  withC f xM = StateT $ \ s₁ →
    withCOn (runStateT s₁ xM) $ \ (s₂ :* x ∷ s ∧ a) → 
      runStateT s₂ $ f x

----------
-- FAIL --
----------

instance LiftIO FailT where
  liftIO ∷ ∀ m. (Monad m) ⇒ (∀ a. IO a → m a) → (∀ a. IO a → FailT m a)
  liftIO ioM xM = FailT $ do
    x ← ioM xM
    return $ Some x

instance LiftReader FailT where
  liftAsk ∷ ∀ m r. (Monad m) ⇒ m r → FailT m r
  liftAsk askM = FailT $ do
    r ← askM
    return $ Some r

  liftLocal ∷ ∀ m r. (Monad m) ⇒ (∀ a. r → m a → m a) → (∀ a. r → FailT m a → FailT m a)
  liftLocal localM r xM = FailT $ localM r $ unFailT xM

instance LiftWriter FailT where
  liftTell ∷ ∀ m o. (Monad m) ⇒ (o → m ()) → (o → FailT m ())
  liftTell tellM o = FailT $ do
    tellM o
    return $ Some ()

  liftHijack ∷ ∀ m o. (Monad m) ⇒ (∀ a. m a → m (o ∧ a)) → (∀ a. FailT m a → FailT m (o ∧ a))
  liftHijack hijackM xM = FailT $ do
    (o :* xO) ← hijackM $ unFailT xM
    case xO of
      None → return None
      Some x → return $ Some (o :* x)

instance LiftState FailT where
  liftGet ∷ ∀ m s. (Monad m) ⇒ m s → FailT m s
  liftGet getM = FailT $ do
    s ← getM
    return $ Some s

  liftPut ∷ ∀ m s. (Monad m) ⇒ (s → m ()) → (s → FailT m ())
  liftPut putM s = FailT $ do
    putM s
    return $ Some ()

instance LiftFail FailT where
  liftAbort ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. FailT m a)
  liftAbort abortM = FailT $ abortM

  liftTry ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. FailT m a → FailT m a → FailT m a)
  liftTry tryM xM₁ xM₂ = FailT $ tryM (unFailT xM₁) (unFailT xM₂)

instance LiftError FailT where
  liftThrow ∷ ∀ e m. (Monad m) ⇒ (∀ a. e → m a) → (∀ a. e → FailT m a)
  liftThrow throwM e = FailT $ throwM e
    
  liftCatch ∷ ∀ e m. (Monad m) ⇒ (∀ a. m a → (e → m a) → m a) → (∀ a. FailT m a → (e → FailT m a) → FailT m a)
  liftCatch catchM xM k = FailT $ catchM (unFailT xM) $ \ e → unFailT $ k e

instance LiftNondet FailT where
  liftMzero ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. FailT m a)
  liftMzero mzeroM = FailT $ mzeroM

  liftMplus ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. FailT m a → FailT m a → FailT m a)
  liftMplus mplusM xM₁ xM₂ = FailT $ mplusM (unFailT xM₁) (unFailT xM₂)

instance LiftTop FailT where
  liftMtop ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. FailT m a)
  liftMtop mtopM = FailT $ mtopM

instance (Monad m,MonadCont (𝑂 r) m) ⇒ MonadCont r (FailT m) where
  callCC ∷ ∀ a. ((a → FailT m r) → FailT m r) → FailT m a
  callCC kk = FailT $
    callCC $ \ (k ∷ 𝑂 a → m (𝑂 r)) →
      unFailT $ kk $ \ (x ∷ a) → 
        FailT $ k $ Some x

  withC ∷ ∀ a. (a → FailT m r) → FailT m a → FailT m r
  withC k xM = FailT $
    withCOn (unFailT xM) $ \ (xO ∷ 𝑂 a) → case xO of
      None → return None
      Some x → unFailT $ k x

-----------
-- Error --
-----------

instance LiftIO (ErrorT e) where
  liftIO ∷ ∀ m. (Monad m) ⇒ (∀ a. IO a → m a) → (∀ a. IO a → ErrorT e m a)
  liftIO ioM xM = ErrorT $ do
    x ← ioM xM
    return $ Inr x

instance LiftReader (ErrorT e) where
  liftAsk ∷ ∀ m r. (Monad m) ⇒ m r → ErrorT e m r
  liftAsk askM = ErrorT $ do
    r ← askM
    return $ Inr r

  liftLocal ∷ ∀ m r. (Monad m) ⇒ (∀ a. r → m a → m a) → (∀ a. r → ErrorT e m a → ErrorT e m a)
  liftLocal localM r xM = ErrorT $ localM r $ unErrorT xM

instance LiftWriter (ErrorT e) where
  liftTell ∷ ∀ m o. (Monad m) ⇒ (o → m ()) → (o → ErrorT e m ())
  liftTell tellM o = ErrorT $ do
    tellM o
    return $ Inr ()

  liftHijack ∷ ∀ m o. (Monad m) ⇒ (∀ a. m a → m (o ∧ a)) → (∀ a. ErrorT e m a → ErrorT e m (o ∧ a))
  liftHijack hijackM xM = ErrorT $ do
    (o :* xE) ← hijackM $ unErrorT xM
    case xE of
      Inl e → return $ Inl e
      Inr x → return $ Inr (o :* x)

instance LiftState (ErrorT e) where
  liftGet ∷ ∀ m s. (Monad m) ⇒ m s → ErrorT e m s
  liftGet getM = ErrorT $ do
    s ← getM
    return $ Inr s

  liftPut ∷ ∀ m s. (Monad m) ⇒ (s → m ()) → (s → ErrorT e m ())
  liftPut putM s = ErrorT $ do
    putM s
    return $ Inr ()

instance LiftFail (ErrorT e) where
  liftAbort ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. ErrorT e m a)
  liftAbort abortM = ErrorT $ abortM

  liftTry ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. ErrorT e m a → ErrorT e m a → ErrorT e m a)
  liftTry tryM xM₁ xM₂ = ErrorT $ tryM (unErrorT xM₁) (unErrorT xM₂)

instance LiftError (ErrorT e) where
  liftThrow ∷ ∀ e' m. (Monad m) ⇒ (∀ a. e' → m a) → (∀ a. e' → ErrorT e m a)
  liftThrow throwM e = ErrorT $ throwM e
    
  liftCatch ∷ ∀ e' m. (Monad m) ⇒ (∀ a. m a → (e' → m a) → m a) → (∀ a. ErrorT e m a → (e' → ErrorT e m a) → ErrorT e m a)
  liftCatch catchM xM k = ErrorT $ catchM (unErrorT xM) $ \ e → unErrorT $ k e

instance LiftNondet (ErrorT e) where
  liftMzero ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. ErrorT e m a)
  liftMzero mzeroM = ErrorT $ mzeroM

  liftMplus ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. ErrorT e m a → ErrorT e m a → ErrorT e m a)
  liftMplus mplusM xM₁ xM₂ = ErrorT $ mplusM (unErrorT xM₁) (unErrorT xM₂)

instance LiftTop (ErrorT e) where
  liftMtop ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. ErrorT e m a)
  liftMtop mtopM = ErrorT $ mtopM

instance (Monad m,MonadCont (e ∨ r) m) ⇒ MonadCont r (ErrorT e m) where
  callCC ∷ ∀ a. ((a → ErrorT e m r) → ErrorT e m r) → ErrorT e m a
  callCC kk = ErrorT $
    callCC $ \ (k ∷ e ∨ a → m (e ∨ r)) →
      unErrorT $ kk $ \ (x ∷ a) → 
        ErrorT $ k (Inr x)

  withC ∷ ∀ a. (a → ErrorT e m r) → ErrorT e m a → ErrorT e m r
  withC k xM = ErrorT $
    withC 
    (\ (ex ∷ e ∨ a) → case ex of
         Inl e → return $ Inl e
         Inr x → unErrorT $ k x)
    (unErrorT xM)

------------
-- NONDET --
------------

instance LiftIO NondetT where
  liftIO ∷ ∀ m. (Monad m) ⇒ (∀ a. IO a → m a) → (∀ a. IO a → NondetT m a)
  liftIO ioM xM = NondetT $ do
    x ← ioM xM
    return $ single x

instance LiftReader NondetT where
  liftAsk ∷ ∀ m r. (Monad m) ⇒ m r → NondetT m r
  liftAsk askM = NondetT $ do
    r ← askM
    return $ single r

  liftLocal ∷ ∀ m r. (Monad m) ⇒ (∀ a. r → m a → m a) → (∀ a. r → NondetT m a → NondetT m a)
  liftLocal localM r xM = NondetT $ localM r $ unNondetT xM
    
instance LiftWriter NondetT where
  liftTell ∷ ∀ m o. (Monad m) ⇒ (o → m ()) → (o → NondetT m ())
  liftTell tellM o = NondetT $ do
    tellM o
    return $ single ()

  liftHijack ∷ ∀ m o. (Monad m) ⇒ (∀ a. m a → m (o ∧ a)) → (∀ a. NondetT m a → NondetT m (o ∧ a))
  liftHijack hijackM xM = NondetT $ do
    (o :* xs) ← hijackM $ unNondetT xM
    return $ map (o :* ) xs

instance LiftState NondetT where
  liftGet ∷ ∀ m s. (Monad m) ⇒ m s → NondetT m s
  liftGet getM = NondetT $ do
    s ← getM
    return $ single s

  liftPut ∷ ∀ m s. (Monad m) ⇒ (s → m ()) → s → NondetT m ()
  liftPut putM s = NondetT $ do
    putM s
    return $ single ()

instance LiftFail NondetT where
  liftAbort ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. NondetT m a)
  liftAbort abortM = NondetT $ abortM

  liftTry ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. NondetT m a → NondetT m a → NondetT m a)
  liftTry tryM xM₁ xM₂ = NondetT $ tryM (unNondetT xM₁) (unNondetT xM₂)

instance LiftError NondetT where
  liftThrow ∷ ∀ m e. (Monad m) ⇒ (∀ a. e → m a) → (∀ a. e → NondetT m a)
  liftThrow throwM e = NondetT $ throwM e

  liftCatch ∷ ∀ m e. (Monad m) ⇒ (∀ a. m a → (e → m a) → m a) → (∀ a. NondetT m a → (e → NondetT m a) → NondetT m a)
  liftCatch catchM xM k = NondetT $ catchM (unNondetT xM) $ \ e → unNondetT $ k e

instance LiftNondet NondetT where
  liftMzero ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. NondetT m a)
  liftMzero mzeroM = NondetT $ mzeroM

  liftMplus ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. NondetT m a → NondetT m a → NondetT m a)
  liftMplus mplusM xM₁ xM₂ = NondetT $ mplusM (unNondetT xM₁) (unNondetT xM₂)

instance LiftTop NondetT where
  liftMtop ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. NondetT m a)
  liftMtop mtopM = NondetT $ mtopM

instance (Monad m,∀ a'. Monoid a' ⇒ Monoid (m a'),MonadCont (𝑄 r) m) ⇒ MonadCont r (NondetT m) where
  callCC ∷ ∀ a. ((a → NondetT m r) → NondetT m r) → NondetT m a
  callCC kk = NondetT $
    callCC $ \ (k ∷ 𝑄 a → m (𝑄 r)) →
      unNondetT $ kk $ \ (x ∷ a) → 
        NondetT $ k (single x)

  withC ∷ ∀ a. (a → NondetT m r) → NondetT m a → NondetT m r
  withC k xM = NondetT $
    withC 
    (\ (xs ∷ 𝑄 a) → unNondetT $ foldr mzero (⊞) $ map k $ iter xs)
    (unNondetT xM)

----------
-- Cont --
----------

instance LiftIO (ContT r) where
  liftIO ∷ ∀ m. (Monad m) ⇒ (∀ a. IO a → m a) → (∀ a. IO a → ContT r m a)
  liftIO ioM xM = ContT $ \ (k ∷ a → m r) → do
    x ← ioM xM
    k x

-- instance (Monad m,MonadReader r' m) ⇒ MonadReader r' (ContT r m) where
--   ask ∷ ContT r m r'
--   ask = ContT $ \ (k ∷ r' → m r) → k *$ ask
-- 
--   local ∷ ∀ a. r' → ContT r m a → ContT r m a
--   local r xM = ContT $ \ (k ∷ a → m r) → local r $ unContT xM k

-- instance (Monad m,Monoid o,MonadWriter o m) ⇒ MonadWriter o (ContT r m) where
--   tell ∷ o → ContT r m ()
--   tell o = ContT $ \ (k ∷ () → m r) → do
--     tell o
--     k ()
-- 
--   hijack ∷ ∀ a. ContT r m a → ContT r m (o ∧ a)
--   hijack xM = ContT $ \ (k ∷ (o ∧ a) → m r) → do
--     o :* r ← hijack $ unContT xM $ \ (x ∷ a) → k $ null :* x
--     tell o
--     return r

instance (Monad m,MonadState s m) ⇒ MonadState s (ContT r m) where
  get ∷ ContT r m s
  get = ContT $ \ (k ∷ s → m r) → do
    s ← get
    k s

  put ∷ s → ContT r m ()
  put s = ContT $ \ (k ∷ () → m r) → do
    put s
    k ()

instance (Monad m,MonadFail m) ⇒ MonadFail (ContT r m) where
  abort ∷ ∀ a. ContT r m a
  abort = ContT $ \ (_ ∷ a → m r) → abort

  (⎅) ∷ ∀ a. ContT r m a → ContT r m a → ContT r m a
  xM₁ ⎅ xM₂ = ContT $ \ (k ∷ a → m r) → do
    runContT k xM₁ ⎅ runContT k xM₂

instance (Monad m,MonadError e m) ⇒ MonadError e (ContT r m) where
  throw ∷ ∀ a. e → ContT r m a
  throw e = ContT $ \ (_ ∷ a → m r) → throw e

  catch ∷ ∀ a. ContT r m a → (e → ContT r m a) → ContT r m a
  catch xM₁ kk = ContT $ \ (k ∷ a → m r) → do
    catch (runContT k xM₁) $ \ e →
      runContT k $ kk e

instance (Monad m,MonadNondet m) ⇒ MonadNondet (ContT r m) where
  mzero ∷ ∀ a. ContT r m a
  mzero = ContT $ \ (_ ∷ a → m r) → mzero

  (⊞) ∷ ∀ a. ContT r m a → ContT r m a → ContT r m a
  xM₁ ⊞ xM₂ = ContT $ \ (k ∷ a → m r) → do
    runContT k xM₁ ⊞ runContT k xM₂

instance (Monad m,MonadTop m) ⇒ MonadTop (ContT r m) where
  mtop ∷ ∀ a. ContT r m a
  mtop = ContT $ \ (_ ∷ a → m r) → mtop

-----------
-- UCont --
-----------

instance LiftIO UContT where
  liftIO ∷ ∀ m. (Monad m) ⇒ (∀ a. IO a → m a) → (∀ a. IO a → UContT m a)
  liftIO ioM xM = UContT HS.$ \ (𝓀 ∷ a → m u) → 𝓀 *$ ioM xM

instance (Monad m,MonadReader r m) ⇒ MonadReader r (UContT m) where
  ask ∷ UContT m r
  ask = UContT HS.$ \ (𝓀 ∷ r → m u) → 𝓀 *$ ask

  local ∷ ∀ a. r → UContT m a → UContT m a
  local r xM = UContT HS.$ \ (𝓀 ∷ a → m u) → 𝓀 *$ local r $ evalUContT xM

instance (Monad m,Monoid o,MonadWriter o m) ⇒ MonadWriter o (UContT m) where
  tell ∷ o → UContT m ()
  tell o = UContT HS.$ \ (𝓀 ∷ () → m u) → 𝓀 *$ tell o

  hijack ∷ ∀ a. UContT m a → UContT m (o ∧ a)
  hijack xM = UContT HS.$ \ (𝓀 ∷ (o ∧ a) → m u) → 𝓀 *$ hijack $ evalUContT xM

instance (Monad m,MonadState s m) ⇒ MonadState s (UContT m) where
  get ∷ UContT m s
  get = UContT HS.$ \ (𝓀 ∷ s → m u) → 𝓀 *$ get

  put ∷ s → UContT m ()
  put s = UContT HS.$ \ (𝓀 ∷ () → m u) → 𝓀 *$ put s

instance (Monad m,MonadFail m) ⇒ MonadFail (UContT m) where
  abort ∷ ∀ a. UContT m a
  abort = UContT HS.$ \ (_ ∷ a → m u) → abort

  (⎅) ∷ ∀ a. UContT m a → UContT m a → UContT m a
  xM₁ ⎅ xM₂ = UContT HS.$ \ (k ∷ a → m u) → do
    runUContT k xM₁ ⎅ runUContT k xM₂

instance (Monad m,MonadError e m) ⇒ MonadError e (UContT m) where
  throw ∷ ∀ a. e → UContT m a
  throw e = UContT HS.$ \ (_ ∷ a → m u) → throw e

  catch ∷ ∀ a. UContT m a → (e → UContT m a) → UContT m a
  catch xM₁ kk = UContT HS.$ \ (k ∷ a → m u) → do
    catch (runUContT k xM₁) $ \ e →
      runUContT k $ kk e

instance (Monad m,MonadNondet m) ⇒ MonadNondet (UContT m) where
  mzero ∷ ∀ a. UContT m a
  mzero = UContT HS.$ \ (_ ∷ a → m u) → mzero

  (⊞) ∷ ∀ a. UContT m a → UContT m a → UContT m a
  xM₁ ⊞ xM₂ = UContT HS.$ \ (k ∷ a → m u) → do
    runUContT k xM₁ ⊞ runUContT k xM₂

instance (Monad m,MonadTop m) ⇒ MonadTop (UContT m) where
  mtop ∷ ∀ a. UContT m a
  mtop = UContT HS.$ \ (_ ∷ a → m u) → mtop

-- ======= --
-- DERIVED --
-- ======= --

----------
-- RWST --
----------

newtype RWST r o s m a = RWST { unRWST ∷ ReaderT r (WriterT o (StateT s m)) a }
  deriving
  (Functor,Return,Bind,Monad
  ,MonadIO
  ,MonadReader r,MonadWriter o,MonadState s
  ,MonadFail,MonadError e
  ,MonadNondet,MonadTop
  )

mkRWST ∷ ∀ r o s m a. (Monad m) ⇒ (r → s → m (s ∧ o ∧ a)) → RWST r o s m a
mkRWST f = RWST $ ReaderT $ \ r → WriterT $ StateT $ \ s → do
  (s' :* o :* a) ← f r s
  return (s' :* (o :* a))

runRWST ∷ ∀ r o s m a. (Monad m) ⇒ r → s → RWST r o s m a → m (s ∧ o ∧ a)
runRWST r s xM = do
  (s' :* (o :* a)) ← unStateT (unWriterT (unReaderT (unRWST xM) r)) s
  return (s' :* o :* a)

evalRWST ∷ ∀ r o s m a. (Monad m) ⇒ r → s → RWST r o s m a → m a
evalRWST r s = map snd ∘ runRWST r s

instance (Monoid o) ⇒ Functor2 (RWST r o s) where
  map2 ∷ ∀ f₁ f₂. (∀ a. f₁ a → f₂ a) → (∀ a. RWST r o s f₁ a → RWST r o s f₂ a)
  map2 f = RWST ∘ map2 (map2 (map2 f)) ∘ unRWST

instance (RWST r o s) ⇄⁼ (ReaderT r ⊡ WriterT o ⊡ StateT s) where
  isoto3 ∷ ∀ f a. RWST r o s f a → (ReaderT r ⊡ WriterT o ⊡ StateT s) f a
  isoto3 = Compose2 ∘ Compose2 ∘ unRWST

  isofr3 ∷ ∀ f a. (ReaderT r ⊡ WriterT o ⊡ StateT s) f a → RWST r o s f a
  isofr3 = RWST ∘ unCompose2 ∘ unCompose2

instance (Monoid o) ⇒ Transformer (RWST r o s) where
  lift = RWST ∘ lift ∘ lift ∘ lift

-- deriving instance (Monoid o,Monad m,MonadCont (s ∧ (o ∧ r')) m) ⇒ MonadCont r' (RWST r o s m)

deriving instance (∀ a'. Null a' ⇒ Null (m a'),Null o,Null s,Null a) ⇒ Null (RWST r o s m a)
deriving instance (∀ a'. Append a' ⇒ Append (m a'),Append o,Append s,Append a) ⇒ Append (RWST r o s m a)
deriving instance 
  (∀ a'. Null a' ⇒ Null (m a')
  ,∀ a'. Append a' ⇒ Append (m a')
  ,∀ a'. Monoid a' ⇒ Monoid (m a')
  ,Monoid o,Monoid s,Monoid a) 
  ⇒ Monoid (RWST r o s m a)

type RWS r o s = RWST r o s ID

mkRWS ∷ ∀ r o s a. (r → s → (s ∧ o ∧ a)) → RWS r o s a
mkRWS f = mkRWST (\ r s → ID $ f r s)

runRWS ∷ ∀ r o s a. r → s → RWS r o s a → s ∧ o ∧ a
runRWS r s xM = unID $ runRWST r s xM

evalRWS ∷ ∀ r o s a. r → s → RWS r o s a → a
evalRWS r s xM = unID $ evalRWST r s xM
