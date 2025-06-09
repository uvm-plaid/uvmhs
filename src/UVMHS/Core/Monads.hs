module UVMHS.Core.Monads where

import UVMHS.Core.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data

import UVMHS.Core.Effects
import UVMHS.Core.Transformers

import qualified Prelude as HS

import qualified Language.Haskell.TH as TH

newtype MU m = MU { unMU ∷ m () }

onMU ∷ (m () → m ()) → MU m → MU m
onMU f = MU ∘ f ∘ unMU

instance (Return m) ⇒ Null (MU m) where null = MU $ return ()
instance (Bind m) ⇒ Append (MU m) where x ⧺ y = MU $ unMU x ≫ unMU y
instance (Monad m) ⇒ Monoid (MU m)

instance MonadIO IO where
  io = id

instance MonadIO QIO where 
  io = TH.runIO
instance MonadQIO QIO where
  qio = id

instance Functor IO where
  map = mmap
instance Return IO where
  return = HS.return
instance Bind IO where
  (≫=) = (HS.>>=)
instance Monad IO

newtype ID a = ID { unID ∷ a }
  deriving
  ( Null,Append,Monoid
  , Bot,Join,JoinLattice
  , Top,Meet,MeetLattice
  , Lattice,Dual,Difference
  )

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
  askL ∷ ∀ r'. r ⟢ r' → ReaderT r m r'
  askL ℓ = ReaderT $ \ r → return $ access ℓ r

  localL ∷ ∀ r' a. r ⟢ r' → r' → ReaderT r m a → ReaderT r m a
  localL ℓ r' xM = ReaderT $ \ r → unReaderT xM $ update ℓ r' r

instance (Func Null m,Null a) ⇒ Null (ReaderT r m a) where
  null ∷ ReaderT r m a
  null = ReaderT $ \ _ → null
instance (Func Append m,Append a) ⇒ Append (ReaderT r m a) where
  (⧺) ∷ ReaderT r m a → ReaderT r m a → ReaderT r m a
  (⧺) xM₁ xM₂ = ReaderT $ \ r → unReaderT xM₁ r ⧺ unReaderT xM₂ r
instance
  ( Func Null m
  , Func Append m
  , Monoid a
  ) ⇒ Monoid (ReaderT r m a)

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

instance (Func Null m,Null o,Null a) ⇒ Null (WriterT o m a) where
  null ∷ WriterT o m a
  null = WriterT null
instance (Func Append m,Append o,Append a) ⇒ Append (WriterT o m a) where
  (⧺) ∷ WriterT o m a → WriterT o m a → WriterT o m a
  xM₁ ⧺ xM₂ = WriterT $ unWriterT xM₁ ⧺ unWriterT xM₂
instance
  ( Func Null m
  , Func Append m
  , Monoid o
  , Monoid a
  )
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

instance (Func Null m,Null s,Null a) ⇒ Null (StateT s m a) where
  null ∷ StateT s m a
  null = StateT $ \ _ → null
instance (Func Append m,Append s,Append a) ⇒ Append (StateT s m a) where
  (⧺) ∷ StateT s m a → StateT s m a → StateT s m a
  xM₁ ⧺ xM₂ = StateT $ \ s → unStateT xM₁ s ⧺ unStateT xM₂ s
instance
  ( Func Null m
  , Func Append m
  , Monoid s,Monoid a
  )
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

instance (Func Null m,Null a) ⇒ Null (FailT m a) where
  null ∷ FailT m a
  null = FailT null
instance (Func Append m,Append a) ⇒ Append (FailT m a) where
  (⧺) ∷ FailT m a → FailT m a → FailT m a
  xM₁ ⧺ xM₂ = FailT $ unFailT xM₁ ⧺ unFailT xM₂
instance
  ( Func Null m
  , Func Append m
  , Monoid a
  )
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

instance (Func Null m,Null a) ⇒ Null (ErrorT e m a) where
  null ∷ ErrorT e m a
  null = ErrorT null
instance (Func Append m,Append e,Append a) ⇒ Append (ErrorT e m a) where
  (⧺) ∷ ErrorT e m a → ErrorT e m a → ErrorT e m a
  xM₁ ⧺ xM₂ = ErrorT $ unErrorT xM₁ ⧺ unErrorT xM₂
instance
  ( Func Null m
  , Func Append m
  , Append e,Monoid a
  )
  ⇒ Monoid (ErrorT e m a)

instance Transformer (ErrorT e) where
  lift ∷ ∀ m a. (Monad m) ⇒ m a → ErrorT e m a
  lift xM = ErrorT $ Inr ^$ xM

-----------
-- DELAY --
-----------

newtype DelayT m a = DelayT { unDelayT ∷ () → m a }

runDelayT ∷ DelayT m a → m a
runDelayT = appto () ∘ unDelayT

instance (Functor m) ⇒ Functor (DelayT m) where map f xM = DelayT $ \ () → map f $ runDelayT xM
instance (Return m) ⇒ Return (DelayT m) where return = DelayT ∘ const ∘ return
instance (Bind m) ⇒ Bind (DelayT m) where xM ≫= f = DelayT $ \ () → runDelayT xM ≫= runDelayT ∘ f
instance (Monad m) ⇒ Monad (DelayT m)
instance Functor2 DelayT where
  map2 ∷ ∀ m₁ m₂. (∀ a. m₁ a → m₂ a) → (∀ a. DelayT m₁ a → DelayT m₂ a)
  map2 f = DelayT ∘ map f ∘ unDelayT


instance MonadDelay (DelayT m) where
  delay xMU = DelayT $ \ () → runDelayT $ xMU ()

instance (Const Null m) ⇒ Null (DelayT m a) where
  null = DelayT $ \ () → null
instance (Const Append m) ⇒ Append (DelayT m a) where
  xM₁ ⧺ xM₂ = DelayT $ \ () → runDelayT xM₁ ⧺ runDelayT xM₂
instance (Const Null m,Const Append m) ⇒ Monoid (DelayT m a)

instance Transformer DelayT where lift xM = DelayT $ \ () → xM

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
instance (Bind m,Func Monoid m) ⇒ Bind (NondetT m) where
  (≫=) ∷ ∀ a b. NondetT m a → (a → NondetT m b) → NondetT m b
  xM ≫= k = NondetT $ do
    xs ← unNondetT xM
    unNondetT $ foldr mzero (⊞) $ map k $ iter xs
instance (Monad m,Func Monoid m) ⇒ Monad (NondetT m)

instance (Func Monoid m) ⇒ MonadNondet (NondetT m) where
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

newtype ContT u m a = ContT { unContT ∷ (a → m u) → m u }

runContT ∷ (a → m u) → ContT u m a → m u
runContT = flip unContT

evalContT ∷ (Return m) ⇒ ContT u m u → m u
evalContT = runContT return

instance Functor (ContT u m) where
  map ∷ ∀ a b. (a → b) → ContT u m a → ContT u m b
  map f xM = ContT $ \ (k ∷ b → m r) → unContT xM $ \ x → k $ f x

instance Return (ContT u m) where
  return ∷ ∀ a. a → ContT u m a
  return x = ContT $ \ (k ∷ a → m r) → k x
instance Bind (ContT u m) where
  (≫=) ∷ ∀ a b. ContT u m a → (a → ContT u m b) → ContT u m b
  xM ≫= kk = ContT $ \ (k ∷ b → m r) → unContT xM $ \ (x ∷ a) → unContT (kk x) k
instance Monad (ContT u m)

instance Functor2Iso (ContT u) where
  map2iso ∷ ∀ m₁ m₂. Iso2 m₁ m₂ → ∀ a. ContT u m₁ a → ContT u m₂ a
  map2iso i xM = ContT $ \ (k ∷ a → m₂ r) →
    ito2 i $ unContT xM $ \ (x ∷ a) →
      ifr2 i $ k x

instance (Monad m) ⇒ MonadCont u (ContT u m) where
  callCC ∷ ∀ a. ((a → ContT u m u) → ContT u m u) → ContT u m a
  callCC kk = ContT $ \ (k ∷ a → m r) →
    runContT return $ kk $ \ (x ∷ a) →
      ContT $ \ (k' ∷ r → m r) →
        k' *$ k x

  withC ∷ ∀ a. (a → ContT u m u) → ContT u m a → ContT u m u
  withC k₁ xM = ContT $ \ (k₂ ∷ u → m u) →
    k₂ *$ unContT xM $ \ (x ∷ a) →
      runContT return $ k₁ x

instance (Func Null m,Null u) ⇒ Null (ContT u m a) where
  null ∷ ContT u m a
  null = ContT $ \ (_ ∷ a → m r) → null
instance (Func Append m,Append u) ⇒ Append (ContT u m a) where
  (⧺) ∷ ContT u m a → ContT u m a → ContT u m a
  xM₁ ⧺ xM₂ = ContT $ \ (k ∷ a → m r) → unContT xM₁ k ⧺ unContT xM₂ k
instance
  ( Func Null m
  , Func Append m
  , Monoid u
  )
  ⇒ Monoid (ContT u m a)

instance Transformer (ContT u) where
  lift ∷ ∀ m a. (Monad m) ⇒ m a → ContT u m a
  lift xM = ContT $ \ (κ ∷ a → m r) → κ *$ xM

-----------
-- UCont --
-----------

newtype UContT m a = UContT { unUContT ∷ ∀ u. (a → m u) → m u }

runUContT ∷ ∀ u m a. (a → m u) → UContT m a → m u
runUContT k xM = unUContT xM k

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

instance (Const Null m) ⇒ Null (UContT m a) where
  null ∷ UContT m a
  null = UContT HS.$ \ (_ ∷ a → m u) → null
instance (Const Append m) ⇒ Append (UContT m a) where
  (⧺) ∷ UContT m a → UContT m a → UContT m a
  xM₁ ⧺ xM₂ = UContT HS.$ \ (𝓀 ∷ a → m u) → unUContT xM₁ 𝓀 ⧺ unUContT xM₂ 𝓀
instance
  ( Const Monoid m
  ) ⇒ Monoid (UContT m a)

instance Transformer UContT where
  lift ∷ ∀ m a. (Monad m) ⇒ m a → UContT m a
  lift xM = UContT HS.$ \ (𝓀 ∷ a → m u) → 𝓀 *$ xM

-- ================= --
-- AUTOMATIC LIFTING --
-- ================= --

------------
-- READER --
------------

instance LiftIO (ReaderT r) where
  liftIO ∷ ∀ m. (Monad m) ⇒ (∀ a. IO a → m a) → (∀ a. IO a → ReaderT r m a)
  liftIO = (∘) lift
instance (Monad m,MonadIO m) ⇒ MonadIO (ReaderT r m) where
  io ∷ ∀ a. IO a → ReaderT r m a
  io = liftIO io

instance LiftQIO (ReaderT r) where
  liftQIO ∷ ∀ m. (Monad m) ⇒ (∀ a. QIO a → m a) → (∀ a. QIO a → ReaderT r m a)
  liftQIO = (∘) lift
instance (Monad m,MonadQIO m) ⇒ MonadQIO (ReaderT r m) where
  qio ∷ ∀ a. QIO a → ReaderT r m a
  qio = liftQIO qio

instance LiftReader (ReaderT r) where
  liftAskL ∷ ∀ m r'. (Monad m) ⇒ (∀ r''. r' ⟢ r'' → m r'') → (∀ r''. r' ⟢ r'' → ReaderT r m r'')
  liftAskL askLM ℓ = ReaderT $ \ _ → askLM ℓ

  liftLocalL ∷ ∀ m r'. (Monad m) ⇒ (∀ r'' a. r' ⟢ r'' → r'' → m a → m a) → (∀ r'' a. r' ⟢ r'' → r'' → ReaderT r m a → ReaderT r m a)
  liftLocalL localLM ℓ r' xM = ReaderT $ \ r → localLM ℓ r' $ unReaderT xM r

instance LiftWriter (ReaderT r) where
  liftTell ∷ ∀ m o. (Monad m) ⇒ (o → m ()) → (o → ReaderT r m ())
  liftTell tellM o = ReaderT $ \ _ → tellM o

  liftHijack ∷ ∀ m o. (Monad m) ⇒ (∀ a. m a → m (o ∧ a)) → (∀ a. ReaderT r m a → ReaderT r m (o ∧ a))
  liftHijack hijackM xM = ReaderT $ \ r → hijackM $ unReaderT xM r
instance (Monad m,MonadWriter o m) ⇒ MonadWriter o (ReaderT r m) where
  tell = liftTell tell
  hijack = liftHijack hijack

instance LiftState (ReaderT r) where
  liftGet ∷ ∀ m s. (Monad m) ⇒ m s → ReaderT r m s
  liftGet getM = ReaderT $ \ _ → getM

  liftPut ∷ ∀ m s. (Monad m) ⇒ (s → m ()) → (s → ReaderT r m ())
  liftPut putM s = ReaderT $ \ _ → putM s
instance (Monad m,MonadState s m) ⇒ MonadState s (ReaderT r m) where
  get = liftGet get
  put = liftPut put

instance LiftFail (ReaderT r) where
  liftAbort ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. ReaderT r m a)
  liftAbort abortM = ReaderT $ \ _ → abortM

  liftTry ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. ReaderT r m a → ReaderT r m a → ReaderT r m a)
  liftTry tryM xM₁ xM₂ = ReaderT $ \ r → tryM (unReaderT xM₁ r) (unReaderT xM₂ r)
instance (Monad m,MonadFail m) ⇒ MonadFail (ReaderT r m) where
  abort = liftAbort abort
  (⎅) = liftTry (⎅)

instance LiftError (ReaderT r) where
  liftThrow ∷ ∀ m e. (Monad m) ⇒ (∀ a. e → m a) → (∀ a. e → ReaderT r m a)
  liftThrow throwM e = ReaderT $ \ _ → throwM e

  liftCatch ∷ ∀ m e. (Monad m) ⇒ (∀ a. m a → (e → m a) → m a) → (∀ a. ReaderT r m a → (e → ReaderT r m a) → ReaderT r m a)
  liftCatch catchM xM k = ReaderT $ \ r → catchM (unReaderT xM r) (\ e → unReaderT (k e) r)
instance (Monad m,MonadError e m) ⇒ MonadError e (ReaderT r m) where
  throw = liftThrow throw
  catch = liftCatch catch

instance LiftDelay (ReaderT r) where
  liftDelay delayM xMU = ReaderT $ \ r → delayM $ \ () → runReaderT r $ xMU ()
instance (Monad m,MonadDelay m) ⇒ MonadDelay (ReaderT r m) where
  delay = liftDelay delay

instance LiftNondet (ReaderT r) where
  liftMzero ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. ReaderT r m a)
  liftMzero mzeroM = ReaderT $ \ _ → mzeroM

  liftMplus ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. ReaderT r m a → ReaderT r m a → ReaderT r m a)
  liftMplus mplusM xM₁ xM₂ = ReaderT $ \ r → mplusM (unReaderT xM₁ r) (unReaderT xM₂ r)
instance (Monad m,MonadNondet m) ⇒ MonadNondet (ReaderT r m) where
  mzero = liftMzero mzero
  (⊞) = liftMplus (⊞)

instance LiftTop (ReaderT r) where
  liftMtop ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. ReaderT r m a)
  liftMtop mtopM = ReaderT $ \ _ → mtopM
instance (Monad m,MonadTop m) ⇒ MonadTop (ReaderT r m) where
  mtop = liftMtop mtop

instance LiftCont (ReaderT r) where
  liftCallCC ∷ ∀ m r'. (Monad m) ⇒ (∀ a. ((a → m r') → m r') → m a) → (∀ a. ((a → ReaderT r m r') → ReaderT r m r') → ReaderT r m a)
  liftCallCC callCCM kk = ReaderT $ \ r →
    callCCM $ \ (k ∷ a → m r') →
      runReaderT r $ kk $ \ (x ∷ a) →
        ReaderT $ \ _ →
          k x
  liftWithC ∷ ∀ m r'. (Monad m) ⇒ (∀ a. (a → m r') → m a → m r') → (∀ a. (a → ReaderT r m r') → ReaderT r m a → ReaderT r m r')
  liftWithC withCM k xM = ReaderT $ \ r →
    flip withCM (runReaderT r xM) $ \ x → runReaderT r $ k x
instance (Monad m,MonadCont r' m) ⇒ MonadCont r' (ReaderT r m) where
  callCC = liftCallCC callCC
  withC = liftWithC withC

------------
-- WRITER --
------------

instance (Null o) ⇒ LiftIO (WriterT o) where
  liftIO ∷ ∀ m. (Monad m) ⇒ (∀ a. IO a → m a) → (∀ a. IO a → WriterT o m a)
  liftIO = (∘) lift
instance (Null o,Monad m,MonadIO m) ⇒ MonadIO (WriterT o m) where
  io ∷ ∀ a. IO a → WriterT o m a
  io = liftIO io

instance (Null o) ⇒ LiftQIO (WriterT o) where
  liftQIO ∷ ∀ m. (Monad m) ⇒ (∀ a. QIO a → m a) → (∀ a. QIO a → WriterT o m a)
  liftQIO = (∘) lift
instance (Null o,Monad m,MonadQIO m) ⇒ MonadQIO (WriterT o m) where
  qio ∷ ∀ a. QIO a → WriterT o m a
  qio = liftQIO qio

instance (Null o) ⇒ LiftReader (WriterT o) where
  liftAskL ∷ ∀ m r. (Monad m) ⇒ (∀ r'. r ⟢ r' → m r') → (∀ r'. r ⟢ r' → WriterT o m r')
  liftAskL askLM ℓ = WriterT $ do
    r ← askLM ℓ
    return $ null :* r

  liftLocalL ∷ ∀ m r. (Monad m) ⇒ (∀ r' a. r ⟢ r' → r' → m a → m a) → (∀ r' a. r ⟢ r' → r' → WriterT o m a → WriterT o m a)
  liftLocalL localLM ℓ r xM = WriterT $ localLM ℓ r $ unWriterT xM
instance (Null o,Monad m,MonadReader r m) ⇒ MonadReader r (WriterT o m) where
  askL = liftAskL askL
  localL = liftLocalL localL

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
instance (Null o,Monad m,MonadState s m) ⇒ MonadState s (WriterT o m) where
  get = liftGet get
  put = liftPut put

instance LiftFail (WriterT o) where
  liftAbort ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. WriterT o m a)
  liftAbort abortM = WriterT abortM

  liftTry ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. WriterT o m a → WriterT o m a → WriterT o m a)
  liftTry tryM xM₁ xM₂ = WriterT $ tryM (unWriterT xM₁) (unWriterT xM₂)
instance (Monad m,MonadFail m) ⇒ MonadFail (WriterT o m) where
  abort = liftAbort abort
  (⎅) = liftTry (⎅)

instance LiftError (WriterT o) where
  liftThrow ∷ ∀ m e. (Monad m) ⇒ (∀ a. e → m a) → (∀ a. e → WriterT o m a)
  liftThrow throwM e = WriterT $ throwM e

  liftCatch ∷ ∀ m e. (Monad m) ⇒ (∀ a. m a → (e → m a) → m a) → (∀ a. WriterT o m a → (e → WriterT o m a) → WriterT o m a)
  liftCatch catchM xM k = WriterT $ catchM (unWriterT xM) $ \ e → unWriterT $ k e
instance (Monad m,MonadError e m) ⇒ MonadError e (WriterT o m) where
  throw = liftThrow throw
  catch = liftCatch catch

instance LiftDelay (WriterT o) where
  liftDelay delayM xMU = WriterT $ delayM $ \ () → unWriterT $ xMU ()
instance (Monad m,MonadDelay m) ⇒ MonadDelay (WriterT o m) where
  delay = liftDelay delay

instance LiftNondet (WriterT o) where
  liftMzero ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. WriterT o m a)
  liftMzero mzeroM = WriterT mzeroM

  liftMplus ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. WriterT o m a → WriterT o m a → WriterT o m a)
  liftMplus mplusM xM₁ xM₂ = WriterT $ mplusM (unWriterT xM₁) (unWriterT xM₂)
instance (Monad m,MonadNondet m) ⇒ MonadNondet (WriterT o m) where
  mzero = liftMzero mzero
  (⊞) = liftMplus (⊞)

instance LiftTop (WriterT o) where
  liftMtop ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. WriterT o m a)
  liftMtop mtopM = WriterT mtopM
instance (Monad m,MonadTop m) ⇒ MonadTop (WriterT o m) where
  mtop = liftMtop mtop

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
  liftIO = (∘) lift
instance (Monad m,MonadIO m) ⇒ MonadIO (StateT s m) where
  io ∷ ∀ a. IO a → StateT s m a
  io = liftIO io

instance LiftQIO (StateT s) where
  liftQIO ∷ ∀ m. (Monad m) ⇒ (∀ a. QIO a → m a) → (∀ a. QIO a → StateT s m a)
  liftQIO = (∘) lift
instance (Monad m,MonadQIO m) ⇒ MonadQIO (StateT s m) where
  qio ∷ ∀ a. QIO a → StateT s m a
  qio = liftQIO qio

instance LiftReader (StateT s) where
  liftAskL ∷ ∀ m r. (Monad m) ⇒ (∀ r'. r ⟢ r' → m r') → (∀ r'. r ⟢ r' → StateT s m r')
  liftAskL askLM ℓ = StateT $ \ s → do
    r ← askLM ℓ
    return $ s :* r

  liftLocalL ∷ ∀ m r. (Monad m) ⇒ (∀ r' a. r ⟢ r' → r' → m a → m a) → (∀ r' a. r ⟢ r' → r' → StateT s m a → StateT s m a)
  liftLocalL localLM ℓ r xM = StateT $ \ s → localLM ℓ r $ unStateT xM s
instance (Monad m,MonadReader r m) ⇒ MonadReader r (StateT s m) where
  askL = liftAskL askL
  localL = liftLocalL localL

instance LiftWriter (StateT s) where
  liftTell ∷ ∀ m o. (Monad m) ⇒ (o → m ()) → (o → StateT s m ())
  liftTell tellM o = StateT $ \ s → do
    tellM o
    return (s :* ())

  liftHijack ∷ ∀ m o. (Monad m) ⇒ (∀ a. m a → m (o ∧ a)) → (∀ a. StateT s m a → StateT s m (o ∧ a))
  liftHijack hijackM xM = StateT $ \ s → do
    (o :* (s' :* x)) ← hijackM $ unStateT xM s
    return (s' :* (o :* x))
instance (Monad m,MonadWriter o m) ⇒ MonadWriter o (StateT s m) where
  tell = liftTell tell
  hijack = liftHijack hijack

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
instance (Monad m,MonadFail m) ⇒ MonadFail (StateT s m) where
  abort = liftAbort abort
  (⎅) = liftTry (⎅)

instance LiftError (StateT s) where
  liftThrow ∷ ∀ m e. (Monad m) ⇒ (∀ a. e → m a) → (∀ a. e → StateT s m a)
  liftThrow throwM e = StateT $ \ _ → throwM e

  liftCatch ∷ ∀ m e. (Monad m) ⇒ (∀ a. m a → (e → m a) → m a) → (∀ a. StateT s m a → (e → StateT s m a) → StateT s m a)
  liftCatch catchM xM k = StateT $ \ s → catchM (unStateT xM s) (\ e → unStateT (k e) s)
instance (Monad m,MonadError e m) ⇒ MonadError e (StateT s m) where
  throw = liftThrow throw
  catch = liftCatch catch

instance LiftDelay (StateT s) where
  liftDelay delayM xMU = StateT $ \ s → delayM $ \ () → runStateT s $ xMU ()
instance (Monad m,MonadDelay m) ⇒ MonadDelay (StateT s m) where
  delay = liftDelay delay

instance LiftNondet (StateT s) where
  liftMzero ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. StateT s m a)
  liftMzero mzeroM = StateT $ \ _ → mzeroM

  liftMplus ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. StateT s m a → StateT s m a → StateT s m a)
  liftMplus mplusM xM₁ xM₂ = StateT $ \ s → mplusM (unStateT xM₁ s) (unStateT xM₂ s)
instance (Monad m,MonadNondet m) ⇒ MonadNondet (StateT s m) where
  mzero = liftMzero mzero
  (⊞) = liftMplus (⊞)

instance LiftTop (StateT s) where
  liftMtop ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. StateT s m a)
  liftMtop mtopM = StateT $ \ _ → mtopM
instance (Monad m,MonadTop m) ⇒ MonadTop (StateT s m) where
  mtop = liftMtop mtop

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
  liftIO = (∘) lift
instance (Monad m,MonadIO m) ⇒ MonadIO (FailT m) where
  io ∷ ∀ a. IO a → FailT m a
  io = liftIO io

instance LiftQIO FailT where
  liftQIO ∷ ∀ m. (Monad m) ⇒ (∀ a. QIO a → m a) → (∀ a. QIO a → FailT m a)
  liftQIO = (∘) lift
instance (Monad m,MonadQIO m) ⇒ MonadQIO (FailT m) where
  qio ∷ ∀ a. QIO a → FailT m a
  qio = liftQIO qio

instance LiftReader FailT where
  liftAskL ∷ ∀ m r. (Monad m) ⇒ (∀ r'. r ⟢ r' → m r') → (∀ r'. r ⟢ r' → FailT m r')
  liftAskL askLM ℓ = FailT $ do
    r ← askLM ℓ
    return $ Some r

  liftLocalL ∷ ∀ m r. (Monad m) ⇒ (∀ r' a. r ⟢ r' → r' → m a → m a) → (∀ r' a. r ⟢ r' → r' → FailT m a → FailT m a)
  liftLocalL localLM ℓ r xM = FailT $ localLM ℓ r $ unFailT xM
instance (Monad m,MonadReader r m) ⇒ MonadReader r (FailT m) where
  askL = liftAskL askL
  localL = liftLocalL localL

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
instance (Monad m,MonadWriter o m) ⇒ MonadWriter o (FailT m) where
  tell = liftTell tell
  hijack = liftHijack hijack

instance LiftState FailT where
  liftGet ∷ ∀ m s. (Monad m) ⇒ m s → FailT m s
  liftGet getM = FailT $ do
    s ← getM
    return $ Some s

  liftPut ∷ ∀ m s. (Monad m) ⇒ (s → m ()) → (s → FailT m ())
  liftPut putM s = FailT $ do
    putM s
    return $ Some ()
instance (Monad m,MonadState s m) ⇒ MonadState s (FailT m) where
  get = liftGet get
  put = liftPut put

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
instance (Monad m,MonadError e m) ⇒ MonadError e (FailT m) where
  throw = liftThrow throw
  catch = liftCatch catch

instance LiftDelay FailT where
  liftDelay delayM xMU = FailT $ delayM $ \ () → unFailT $ xMU ()
instance (Monad m,MonadDelay m) ⇒ MonadDelay (FailT m) where
  delay = liftDelay delay

instance LiftNondet FailT where
  liftMzero ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. FailT m a)
  liftMzero mzeroM = FailT $ mzeroM

  liftMplus ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. FailT m a → FailT m a → FailT m a)
  liftMplus mplusM xM₁ xM₂ = FailT $ mplusM (unFailT xM₁) (unFailT xM₂)
instance (Monad m,MonadNondet m) ⇒ MonadNondet (FailT m) where
  mzero = liftMzero mzero
  (⊞) = liftMplus (⊞)

instance LiftTop FailT where
  liftMtop ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. FailT m a)
  liftMtop mtopM = FailT $ mtopM
instance (Monad m,MonadTop m) ⇒ MonadTop (FailT m) where
  mtop = liftMtop mtop

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
  liftIO = (∘) lift
instance (Monad m,MonadIO m) ⇒ MonadIO (ErrorT e m) where
  io ∷ ∀ a. IO a → ErrorT e m a
  io = liftIO io

instance LiftQIO (ErrorT e) where
  liftQIO ∷ ∀ m. (Monad m) ⇒ (∀ a. QIO a → m a) → (∀ a. QIO a → ErrorT e m a)
  liftQIO = (∘) lift
instance (Monad m,MonadQIO m) ⇒ MonadQIO (ErrorT e m) where
  qio ∷ ∀ a. QIO a → ErrorT e m a
  qio = liftQIO qio

instance LiftReader (ErrorT e) where
  liftAskL ∷ ∀ m r. (Monad m) ⇒ (∀ r'. r ⟢ r' → m r') → (∀ r'. r ⟢ r' → ErrorT e m r')
  liftAskL askLM ℓ = ErrorT $ do
    r ← askLM ℓ
    return $ Inr r

  liftLocalL ∷ ∀ m r. (Monad m) ⇒ (∀ r' a. r ⟢ r' → r' → m a → m a) → (∀ r' a. r ⟢ r' → r' → ErrorT e m a → ErrorT e m a)
  liftLocalL localLM ℓ r xM = ErrorT $ localLM ℓ r $ unErrorT xM
instance (Monad m,MonadReader r m) ⇒ MonadReader r (ErrorT e m) where
  askL = liftAskL askL
  localL = liftLocalL localL

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
instance (Monad m,MonadWriter o m) ⇒ MonadWriter o (ErrorT e m) where
  tell = liftTell tell
  hijack = liftHijack hijack

instance LiftState (ErrorT e) where
  liftGet ∷ ∀ m s. (Monad m) ⇒ m s → ErrorT e m s
  liftGet getM = ErrorT $ do
    s ← getM
    return $ Inr s

  liftPut ∷ ∀ m s. (Monad m) ⇒ (s → m ()) → (s → ErrorT e m ())
  liftPut putM s = ErrorT $ do
    putM s
    return $ Inr ()
instance (Monad m,MonadState s m) ⇒ MonadState s (ErrorT e m) where
  get = liftGet get
  put = liftPut put

instance LiftFail (ErrorT e) where
  liftAbort ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. ErrorT e m a)
  liftAbort abortM = ErrorT $ abortM

  liftTry ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. ErrorT e m a → ErrorT e m a → ErrorT e m a)
  liftTry tryM xM₁ xM₂ = ErrorT $ tryM (unErrorT xM₁) (unErrorT xM₂)
instance (Monad m,MonadFail m) ⇒ MonadFail (ErrorT e m) where
  abort = liftAbort abort
  (⎅) = liftTry (⎅)

instance LiftError (ErrorT e) where
  liftThrow ∷ ∀ e' m. (Monad m) ⇒ (∀ a. e' → m a) → (∀ a. e' → ErrorT e m a)
  liftThrow throwM e = ErrorT $ throwM e

  liftCatch ∷ ∀ e' m. (Monad m) ⇒ (∀ a. m a → (e' → m a) → m a) → (∀ a. ErrorT e m a → (e' → ErrorT e m a) → ErrorT e m a)
  liftCatch catchM xM k = ErrorT $ catchM (unErrorT xM) $ \ e → unErrorT $ k e

instance LiftDelay (ErrorT e) where
  liftDelay delayM xMU = ErrorT $ delayM $ \ () → unErrorT $ xMU ()
instance (Monad m,MonadDelay m) ⇒ MonadDelay (ErrorT e m) where
  delay = liftDelay delay

instance LiftNondet (ErrorT e) where
  liftMzero ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. ErrorT e m a)
  liftMzero mzeroM = ErrorT $ mzeroM

  liftMplus ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. ErrorT e m a → ErrorT e m a → ErrorT e m a)
  liftMplus mplusM xM₁ xM₂ = ErrorT $ mplusM (unErrorT xM₁) (unErrorT xM₂)
instance (Monad m,MonadNondet m) ⇒ MonadNondet (ErrorT e m) where
  mzero = liftMzero mzero
  (⊞) = liftMplus (⊞)

instance LiftTop (ErrorT e) where
  liftMtop ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. ErrorT e m a)
  liftMtop mtopM = ErrorT $ mtopM
instance (Monad m,MonadTop m) ⇒ MonadTop (ErrorT e m) where
  mtop = liftMtop mtop

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

-----------
-- DELAY --
-----------

instance LiftIO DelayT where
  liftIO ∷ ∀ m. (Monad m) ⇒ (∀ a. IO a → m a) → ∀ a. IO a → DelayT m a
  liftIO = (∘) lift
instance (Monad m,MonadIO m) ⇒ MonadIO (DelayT m) where
  io ∷ ∀ a. IO a → DelayT m a
  io = liftIO io

instance LiftQIO DelayT where
  liftQIO ∷ ∀ m. (Monad m) ⇒ (∀ a. QIO a → m a) → ∀ a. QIO a → DelayT m a
  liftQIO = (∘) lift
instance (Monad m,MonadQIO m) ⇒ MonadQIO (DelayT m) where
  qio ∷ ∀ a. QIO a → DelayT m a
  qio = liftQIO qio

instance LiftReader DelayT where
  liftAskL askLM ℓ = DelayT $ \ () → askLM ℓ
  liftLocalL localLM ℓ r xM = DelayT $ \ () → localLM ℓ r $ runDelayT xM
instance (Monad m,MonadReader r m) ⇒ MonadReader r (DelayT m) where
  askL = liftAskL askL
  localL = liftLocalL localL
instance LiftWriter DelayT where
  liftTell tellM o = DelayT $ \ () → tellM o
  liftHijack hijackM xM = DelayT $ \ () → hijackM $ runDelayT xM
instance (Monad m,MonadWriter o m) ⇒ MonadWriter o (DelayT m) where
  tell = liftTell tell
  hijack = liftHijack hijack
instance LiftState DelayT where
  liftGet getM = DelayT $ \ () → getM
  liftPut putM s = DelayT $ \ () → putM s
instance (Monad m,MonadState s m) ⇒ MonadState s (DelayT m) where
  get = liftGet get
  put = liftPut put
instance LiftFail DelayT where
  liftAbort abortM = DelayT $ \ () → abortM
  liftTry tryM xM₁ xM₂ = DelayT $ \ () → tryM (runDelayT xM₁) $ runDelayT xM₂
instance (Monad m,MonadFail m) ⇒ MonadFail (DelayT m) where
  abort = liftAbort abort
  (⎅) = liftTry (⎅)
instance LiftError DelayT where
  liftThrow throwM e = DelayT $ \ () → throwM e
  liftCatch catchM xM f = DelayT $ \ () → catchM (runDelayT xM) $ runDelayT ∘ f
instance (Monad m,MonadError e m) ⇒ MonadError e (DelayT m) where
  throw = liftThrow throw
  catch = liftCatch catch
instance LiftDelay DelayT where
  liftDelay ∷ ∀ m. (Monad m) ⇒ (∀ a. (() → m a) → m a) → (∀ a. (() → DelayT m a) → DelayT m a)
  liftDelay delayM xMU = DelayT $ \ () → delayM $ \ () → runDelayT $ xMU ()
instance LiftNondet DelayT where
  liftMzero ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. DelayT m a)
  liftMzero mzeroM = DelayT $ const mzeroM

  liftMplus ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. DelayT m a → DelayT m a → DelayT m a)
  liftMplus mplusM xM yM = DelayT $ \ () → mplusM (runDelayT xM) $ runDelayT yM
instance (Monad m,MonadNondet m) ⇒ MonadNondet (DelayT m) where
  mzero = liftMzero mzero
  (⊞) = liftMplus (⊞)
instance LiftTop DelayT where
  liftMtop ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. DelayT m a)
  liftMtop mtopM = DelayT $ const mtopM
instance (Monad m,MonadTop m) ⇒ MonadTop (DelayT m) where
  mtop = liftMtop mtop
instance (MonadCont r m) ⇒ MonadCont r (DelayT m) where
  callCC 𝓀𝓀 = DelayT $ \ () →
    callCC $ \ 𝓀 → runDelayT $ 𝓀𝓀 $ \ x → DelayT $ \ () → 𝓀 x
  withC 𝓀 xM = DelayT $ \ () → withC (runDelayT ∘ 𝓀) $ runDelayT xM
instance (MonadUCont m) ⇒ MonadUCont (DelayT m) where
  ucallCC 𝓀𝓀 = DelayT $ \ () →
    ucallCC (\ 𝓀 → runDelayT $ 𝓀𝓀 $ \ x → DelayT $ \ () → 𝓀 x)
  uwithC 𝓀 xM = DelayT $ \ () → uwithC (runDelayT ∘ 𝓀) $ runDelayT xM

------------
-- NONDET --
------------

instance LiftIO NondetT where
  liftIO ∷ ∀ m. (Monad m) ⇒ (∀ a. IO a → m a) → (∀ a. IO a → NondetT m a)
  liftIO = (∘) lift
instance (Monad m,MonadIO m) ⇒ MonadIO (NondetT m) where
  io ∷ ∀ a. IO a → NondetT m a
  io = liftIO io

instance LiftQIO NondetT where
  liftQIO ∷ ∀ m. (Monad m) ⇒ (∀ a. QIO a → m a) → (∀ a. QIO a → NondetT m a)
  liftQIO = (∘) lift
instance (Monad m,MonadQIO m) ⇒ MonadQIO (NondetT m) where
  qio ∷ ∀ a. QIO a → NondetT m a
  qio = liftQIO qio

instance LiftReader NondetT where
  liftAskL ∷ ∀ m r. (Monad m) ⇒ (∀ r'. r ⟢ r' → m r') → (∀ r'. r ⟢ r' → NondetT m r')
  liftAskL askLM ℓ = NondetT $ do
    r ← askLM ℓ
    return $ single r

  liftLocalL ∷ ∀ m r. (Monad m) ⇒ (∀ r' a. r ⟢ r' → r' → m a → m a) → (∀ r' a. r ⟢ r' → r' → NondetT m a → NondetT m a)
  liftLocalL localLM ℓ r xM = NondetT $ localLM ℓ r $ unNondetT xM
instance (Monad m,MonadReader r m) ⇒ MonadReader r (NondetT m) where
  askL = liftAskL askL
  localL = liftLocalL localL

instance LiftWriter NondetT where
  liftTell ∷ ∀ m o. (Monad m) ⇒ (o → m ()) → (o → NondetT m ())
  liftTell tellM o = NondetT $ do
    tellM o
    return $ single ()

  liftHijack ∷ ∀ m o. (Monad m) ⇒ (∀ a. m a → m (o ∧ a)) → (∀ a. NondetT m a → NondetT m (o ∧ a))
  liftHijack hijackM xM = NondetT $ do
    (o :* xs) ← hijackM $ unNondetT xM
    return $ map (o :* ) xs
instance (Monad m,MonadWriter o m) ⇒ MonadWriter o (NondetT m) where
  tell = liftTell tell
  hijack = liftHijack hijack

instance LiftState NondetT where
  liftGet ∷ ∀ m s. (Monad m) ⇒ m s → NondetT m s
  liftGet getM = NondetT $ do
    s ← getM
    return $ single s

  liftPut ∷ ∀ m s. (Monad m) ⇒ (s → m ()) → s → NondetT m ()
  liftPut putM s = NondetT $ do
    putM s
    return $ single ()
instance (Monad m,MonadState s m) ⇒ MonadState s (NondetT m) where
  get = liftGet get
  put = liftPut put

instance LiftFail NondetT where
  liftAbort ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. NondetT m a)
  liftAbort abortM = NondetT $ abortM

  liftTry ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. NondetT m a → NondetT m a → NondetT m a)
  liftTry tryM xM₁ xM₂ = NondetT $ tryM (unNondetT xM₁) (unNondetT xM₂)
instance (Monad m,MonadFail m) ⇒ MonadFail (NondetT m) where
  abort = liftAbort abort
  (⎅) = liftTry (⎅)

instance LiftError NondetT where
  liftThrow ∷ ∀ m e. (Monad m) ⇒ (∀ a. e → m a) → (∀ a. e → NondetT m a)
  liftThrow throwM e = NondetT $ throwM e

  liftCatch ∷ ∀ m e. (Monad m) ⇒ (∀ a. m a → (e → m a) → m a) → (∀ a. NondetT m a → (e → NondetT m a) → NondetT m a)
  liftCatch catchM xM k = NondetT $ catchM (unNondetT xM) $ \ e → unNondetT $ k e
instance (Monad m,MonadError e m) ⇒ MonadError e (NondetT m) where
  throw = liftThrow throw
  catch = liftCatch catch

instance LiftDelay NondetT where
  liftDelay delayM xMU = NondetT $ delayM $ \ () → unNondetT $ xMU ()
instance (Monad m,MonadDelay m) ⇒ MonadDelay (NondetT m) where
  delay = liftDelay delay

instance LiftNondet NondetT where
  liftMzero ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. NondetT m a)
  liftMzero mzeroM = NondetT $ mzeroM

  liftMplus ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. NondetT m a → NondetT m a → NondetT m a)
  liftMplus mplusM xM₁ xM₂ = NondetT $ mplusM (unNondetT xM₁) (unNondetT xM₂)

instance LiftTop NondetT where
  liftMtop ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. NondetT m a)
  liftMtop mtopM = NondetT $ mtopM
instance (Monad m,MonadTop m) ⇒ MonadTop (NondetT m) where
  mtop = liftMtop mtop

instance (Monad m,Func Monoid m,MonadCont (𝑄 r) m) ⇒ MonadCont r (NondetT m) where
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

instance LiftIO (ContT u) where
  liftIO ∷ ∀ m. (Monad m) ⇒ (∀ a. IO a → m a) → (∀ a. IO a → ContT u m a)
  liftIO = (∘) lift
instance (Monad m,MonadIO m) ⇒ MonadIO (ContT u m) where
  io ∷ ∀ a. IO a → ContT u m a
  io = liftIO io

instance LiftQIO (ContT u) where
  liftQIO ∷ ∀ m. (Monad m) ⇒ (∀ a. QIO a → m a) → (∀ a. QIO a → ContT u m a)
  liftQIO = (∘) lift
instance (Monad m,MonadQIO m) ⇒ MonadQIO (ContT u m) where
  qio ∷ ∀ a. QIO a → ContT u m a
  qio = liftQIO qio

instance (Monad m,MonadReader r m) ⇒ MonadReader r (ContT u m) where
  askL ∷ ∀ r'. r ⟢ r' → ContT u m r'
  askL ℓ = ContT $ \ (k ∷ r' → m u) → k *$ askL ℓ

  localL ∷ ∀ r' a. r ⟢ r' → r' → ContT u m a → ContT u m a
  localL ℓ r xM = ContT $ \ (k ∷ a → m u) → do
    r' ← askL ℓ
    localL ℓ r $ unContT xM $ \ x → do
      localL ℓ r' $ k x

instance (Monad m,Monoid o,MonadWriter o m) ⇒ MonadWriter o (ContT (o ∧ r) m) where
  tell ∷ o → ContT (o ∧ r) m ()
  tell o = ContT $ \ (k ∷ () → m (o ∧ r)) → do
    tell o
    k ()

  hijack ∷ ∀ a. ContT (o ∧ r) m a → ContT (o ∧ r) m (o ∧ a)
  hijack xM = ContT $ \ (k ∷ (o ∧ a) → m (o ∧ r)) → do
    o :* ox ← hijack $ unContT xM $ \ (x ∷ a) → do
      o₁ :* (o₂ :* r) ← hijack $ k $ null :* x
      return $ (o₁ ⧺ o₂) :* r
    tell o
    return ox

instance (Monad m,MonadState s m) ⇒ MonadState s (ContT u m) where
  get ∷ ContT u m s
  get = ContT $ \ (k ∷ s → m r) → do
    s ← get
    k s

  put ∷ s → ContT u m ()
  put s = ContT $ \ (k ∷ () → m r) → do
    put s
    k ()

instance (Monad m,MonadFail m) ⇒ MonadFail (ContT u m) where
  abort ∷ ∀ a. ContT u m a
  abort = ContT $ \ (_ ∷ a → m r) → abort

  (⎅) ∷ ∀ a. ContT u m a → ContT u m a → ContT u m a
  xM₁ ⎅ xM₂ = ContT $ \ (k ∷ a → m r) → do
    runContT k xM₁ ⎅ runContT k xM₂

instance (Monad m,MonadError e m) ⇒ MonadError e (ContT u m) where
  throw ∷ ∀ a. e → ContT u m a
  throw e = ContT $ \ (_ ∷ a → m r) → throw e

  catch ∷ ∀ a. ContT u m a → (e → ContT u m a) → ContT u m a
  catch xM₁ kk = ContT $ \ (k ∷ a → m r) → do
    catch (runContT k xM₁) $ \ e →
      runContT k $ kk e

instance LiftDelay (ContT u) where
  liftDelay delayM xMU = ContT $ \ 𝓀 → delayM $ \ () → runContT 𝓀 $ xMU ()
instance (Monad m,MonadDelay m) ⇒ MonadDelay (ContT u m) where
  delay = liftDelay delay

instance (Monad m,MonadNondet m) ⇒ MonadNondet (ContT u m) where
  mzero ∷ ∀ a. ContT u m a
  mzero = ContT $ \ (_ ∷ a → m r) → mzero

  (⊞) ∷ ∀ a. ContT u m a → ContT u m a → ContT u m a
  xM₁ ⊞ xM₂ = ContT $ \ (k ∷ a → m r) → do
    runContT k xM₁ ⊞ runContT k xM₂

instance (Monad m,MonadTop m) ⇒ MonadTop (ContT u m) where
  mtop ∷ ∀ a. ContT u m a
  mtop = ContT $ \ (_ ∷ a → m r) → mtop

-----------
-- UCont --
-----------

instance LiftIO UContT where
  liftIO ∷ ∀ m. (Monad m) ⇒ (∀ a. IO a → m a) → (∀ a. IO a → UContT m a)
  liftIO ioM xM = UContT HS.$ \ (𝓀 ∷ a → m u) → 𝓀 *$ ioM xM
instance (Monad m,MonadIO m) ⇒ MonadIO (UContT m) where
  io ∷ ∀ a. IO a → UContT m a
  io = liftIO io

instance LiftQIO UContT where
  liftQIO ∷ ∀ m. (Monad m) ⇒ (∀ a. QIO a → m a) → (∀ a. QIO a → UContT m a)
  liftQIO ioM xM = UContT HS.$ \ (𝓀 ∷ a → m u) → 𝓀 *$ ioM xM
instance (Monad m,MonadQIO m) ⇒ MonadQIO (UContT m) where
  qio ∷ ∀ a. QIO a → UContT m a
  qio = liftQIO qio

instance (Monad m,MonadReader r m) ⇒ MonadReader r (UContT m) where
  askL ∷ ∀ r'. r ⟢ r' → UContT m r'
  askL ℓ = UContT HS.$ \ (𝓀 ∷ r' → m u) → 𝓀 *$ askL ℓ

  localL ∷ ∀ r' a. r ⟢ r' → r' → UContT m a → UContT m a
  localL ℓ r xM = UContT HS.$ \ (𝓀 ∷ a → m u) → do
    r' ← askL ℓ
    localL ℓ r $ unUContT xM $ \ x → do
      localL ℓ r' $ 𝓀 x

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

instance LiftDelay UContT where
  liftDelay delayM xMU = UContT (\ 𝓀 → delayM $ \ () → runUContT 𝓀 $ xMU ())
instance (Monad m,MonadDelay m) ⇒ MonadDelay (UContT m) where
  delay = liftDelay delay

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
  ( Functor,Return,Bind,Monad
  , MonadIO,MonadQIO
  , MonadReader r,MonadWriter o,MonadState s
  , MonadFail,MonadError e
  , MonadDelay
  , MonadNondet,MonadTop
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

deriving instance (Func Null m,Null o,Null s,Null a) ⇒ Null (RWST r o s m a)
deriving instance (Func Append m,Append o,Append s,Append a) ⇒ Append (RWST r o s m a)
deriving instance
  ( Func Null m
  , Func Append m
  , Monoid o,Monoid s,Monoid a
  )
  ⇒ Monoid (RWST r o s m a)

type RWS r o s = RWST r o s ID

mkRWS ∷ ∀ r o s a. (r → s → (s ∧ o ∧ a)) → RWS r o s a
mkRWS f = mkRWST (\ r s → ID $ f r s)

runRWS ∷ ∀ r o s a. r → s → RWS r o s a → s ∧ o ∧ a
runRWS r s xM = unID $ runRWST r s xM

evalRWS ∷ ∀ r o s a. r → s → RWS r o s a → a
evalRWS r s xM = unID $ evalRWST r s xM
