module UVMHS.Core.Transformers where

import UVMHS.Core.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data

import UVMHS.Core.Effects

import qualified Prelude as HS

-------------
-- COMPOSE --
-------------

infixl 7 ⊡

newtype (⊡) (t₁ ∷ (★ → ★) → (★ → ★)) (t₂ ∷ (★ → ★) → (★ → ★)) m a = Compose2 { unCompose2 ∷ t₁ (t₂ m) a }

instance (∀ m'. Monad m' ⇒ Monad (t₁ m'),∀ m'. Monad m' ⇒ Monad (t₂ m'),Monad m) ⇒ Functor ((t₁ ⊡ t₂) m) where
  map = mmap
instance (∀ m'. Monad m' ⇒ Monad (t₁ m'),∀ m'. Monad m' ⇒ Monad (t₂ m'),Monad m) ⇒ Return ((t₁ ⊡ t₂) m) where
  return ∷ ∀ a. a → (t₁ ⊡ t₂) m a
  return x = Compose2 $ return x
instance (∀ m'. Monad m' ⇒ Monad (t₁ m'),∀ m'. Monad m' ⇒ Monad (t₂ m'),Monad m) ⇒ Bind ((t₁ ⊡ t₂) m) where
  (≫=) ∷ ∀ a b. (t₁ ⊡ t₂) m a → (a → (t₁ ⊡ t₂) m b) → (t₁ ⊡ t₂) m b
  xM ≫= k = Compose2 $ do
    x ← unCompose2 xM
    unCompose2 $ k x
instance (∀ m'. Monad m' ⇒ Monad (t₁ m'),∀ m'. Monad m' ⇒ Monad (t₂ m'),Monad m) ⇒ Monad ((t₁ ⊡ t₂) m)

instance (Functor2 t₁,Functor2 t₂) ⇒ Functor2 (t₁ ⊡ t₂) where
  map2 ∷ ∀ m₁ m₂. (∀ a. m₁ a → m₂ a) → (∀ a. (t₁ ⊡ t₂) m₁ a → (t₁ ⊡ t₂) m₂ a)
  map2 f = Compose2 ∘ map2 (map2 f) ∘ unCompose2

instance (∀ m'. Monad m' ⇒ Monad (t₂ m'),LiftIO t₁,LiftIO t₂) ⇒ LiftIO (t₁ ⊡ t₂) where
  liftIO ∷ ∀ m. (Monad m) ⇒ (∀ a. IO a → m a) → (∀ a. IO a → (t₁ ⊡ t₂) m a)
  liftIO ioM xM = Compose2 $ liftIO (liftIO ioM) xM

instance (∀ m'. Monad m' ⇒ Monad (t₂ m'),LiftReader t₁,LiftReader t₂) ⇒ LiftReader (t₁ ⊡ t₂) where
  liftAskL ∷ ∀ m r. (Monad m) ⇒ (∀ r'. r ⟢ r' → m r') → ∀ r'. r ⟢ r' → (t₁ ⊡ t₂) m r'
  liftAskL askLM = Compose2 ∘ liftAskL (liftAskL askLM)

  liftLocalL ∷ ∀ m r. (Monad m) ⇒ (∀ r' a. r ⟢ r' → r' → m a → m a) → (∀ r' a. r ⟢ r' → r' → (t₁ ⊡ t₂) m a → (t₁ ⊡ t₂) m a)
  liftLocalL localLM ℓ r = Compose2 ∘ (liftLocalL HS.$ liftLocalL localLM) ℓ r ∘ unCompose2

instance (∀ m'. Monad m' ⇒ Monad (t₂ m'),LiftWriter t₁,LiftWriter t₂) ⇒ LiftWriter (t₁ ⊡ t₂) where
  liftTell ∷ ∀ m o. (Monad m) ⇒ (o → m ()) → (o → (t₁ ⊡ t₂) m ())
  liftTell tellM = Compose2 ∘ (liftTell $ liftTell tellM)

  liftHijack ∷ ∀ m o. (Monad m) ⇒ (∀ a. m a → m (o ∧ a)) → (∀ a. (t₁ ⊡ t₂) m a → (t₁ ⊡ t₂) m (o ∧ a))
  liftHijack hijackM = Compose2 ∘ (liftHijack HS.$ liftHijack hijackM) ∘ unCompose2

instance (∀ m'. Monad m' ⇒ Monad (t₂ m'),LiftState t₁,LiftState t₂) ⇒ LiftState (t₁ ⊡ t₂) where
  liftGet ∷ ∀ m s. (Monad m) ⇒ m s → (t₁ ⊡ t₂) m s
  liftGet getM = Compose2 $ liftGet $ liftGet getM

  liftPut ∷ ∀ m s. (Monad m) ⇒ (s → m ()) → (s → (t₁ ⊡ t₂) m ())
  liftPut putM = Compose2 ∘ (liftPut $ liftPut putM)

instance (∀ m'. Monad m' ⇒ Monad (t₂ m'),LiftFail t₁,LiftFail t₂) ⇒ LiftFail (t₁ ⊡ t₂) where
  liftAbort ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. (t₁ ⊡ t₂) m a)
  liftAbort abortM = Compose2 $ liftAbort HS.$ liftAbort abortM

  liftTry ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. (t₁ ⊡ t₂) m a → (t₁ ⊡ t₂) m a → (t₁ ⊡ t₂) m a)
  liftTry tryM xM₁ xM₂ = Compose2 $ (liftTry HS.$ liftTry tryM) (unCompose2 xM₁) (unCompose2 xM₂)

instance (∀ m'. Monad m' ⇒ Monad (t₂ m'),LiftError t₁,LiftError t₂) ⇒ LiftError (t₁ ⊡ t₂) where
  liftThrow ∷ ∀ m e. (Monad m) ⇒ (∀ a. e → m a) → (∀ a. e → (t₁ ⊡ t₂) m a)
  liftThrow throwM = Compose2 ∘ (liftThrow HS.$ liftThrow throwM)

  liftCatch ∷ ∀ m e. (Monad m) ⇒ (∀ a. m a → (e → m a) → m a) → (∀ a. (t₁ ⊡ t₂) m a → (e → (t₁ ⊡ t₂) m a) → (t₁ ⊡ t₂) m a)
  liftCatch catchM xM k = Compose2 $ (liftCatch HS.$ liftCatch catchM) (unCompose2 xM) (unCompose2 ∘ k)

instance (∀ m'. Monad m' ⇒ Monad (t₂ m'),LiftDelay t₁,LiftDelay t₂) ⇒ LiftDelay (t₁ ⊡ t₂) where
  liftDelay ∷ ∀ m. (Monad m) ⇒ (∀ a. (() → m a) → m a) → (∀ a. (() → (t₁ ⊡ t₂) m a) → (t₁ ⊡ t₂) m a)
  liftDelay delayM xMU = Compose2 $ (liftDelay HS.$ liftDelay delayM) (unCompose2 ∘ xMU)

instance (∀ m'. Monad m' ⇒ Monad (t₂ m'),LiftNondet t₁,LiftNondet t₂) ⇒ LiftNondet (t₁ ⊡ t₂) where
  liftMzero ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. (t₁ ⊡ t₂) m a)
  liftMzero mzeroM = Compose2 $ liftMzero HS.$ liftMzero mzeroM

  liftMplus ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. (t₁ ⊡ t₂) m a → (t₁ ⊡ t₂) m a → (t₁ ⊡ t₂) m a)
  liftMplus mplusM xM₁ xM₂ = Compose2 $ (liftMplus HS.$ liftMplus mplusM) (unCompose2 xM₁) (unCompose2 xM₂)

instance (∀ m'. Monad m' ⇒ Monad (t₂ m'),LiftTop t₁,LiftTop t₂) ⇒ LiftTop (t₁ ⊡ t₂) where
  liftMtop ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. (t₁ ⊡ t₂) m a)
  liftMtop mtopM = Compose2 $ liftMtop HS.$ liftMtop mtopM

instance (∀ m'. Monad m' ⇒ Monad (t₂ m'),LiftCont t₁,LiftCont t₂) ⇒ LiftCont (t₁ ⊡ t₂) where
  liftCallCC ∷ ∀ m r. (Monad m) ⇒ (∀ a. ((a → m r) → m r) → m a) → (∀ a. ((a → (t₁ ⊡ t₂) m r) → (t₁ ⊡ t₂) m r) → (t₁ ⊡ t₂) m a)
  liftCallCC callCCM kk = Compose2 $ (liftCallCC HS.$ liftCallCC callCCM) $ \ (k ∷ a → t₁ (t₂ m) r) → unCompose2 $ kk (Compose2 ∘ k)

  liftWithC ∷ ∀ m r. (Monad m) ⇒ (∀ a. (a → m r) → m a → m r) →  (∀ a. (a → (t₁ ⊡ t₂) m r) → (t₁ ⊡ t₂) m a → (t₁ ⊡ t₂) m r)
  liftWithC withCM k xM = Compose2 $ (liftWithC HS.$ liftWithC withCM) (unCompose2 ∘ k) (unCompose2 xM)

-------------
-- LIFTING --
-------------

-- instance {-# OVERLAPPABLE #-} (Monad m,MonadIO m,LiftIO t) ⇒ MonadIO (t m) where
--   io = liftIO io
-- instance {-# OVERLAPPABLE #-} (Monad m,MonadReader r m,LiftReader t) ⇒ MonadReader r (t m) where
--   askL = liftAskL askL
--   localL = liftLocalL localL
-- instance {-# OVERLAPPABLE #-} (Monad m,MonadWriter o m,LiftWriter t) ⇒ MonadWriter o (t m) where
--   tell = liftTell tell
--   hijack = liftHijack hijack
-- instance {-# OVERLAPPABLE #-} (Monad m,MonadState s m,LiftState t) ⇒ MonadState s (t m) where
--   get = liftGet get
--   put = liftPut put
-- instance {-# OVERLAPPABLE #-} (Monad m,MonadFail m,LiftFail t) ⇒ MonadFail (t m) where
--   abort = liftAbort abort
--   (⎅) = liftTry (⎅)
-- instance {-# OVERLAPPABLE #-} (Monad m,MonadError e m,LiftError t) ⇒ MonadError e (t m) where
--   throw = liftThrow throw
--   catch = liftCatch catch
-- instance {-# OVERLAPPABLE #-} (Monad m,MonadDelay m,LiftDelay t) ⇒ MonadDelay (t m) where
--   delay = liftDelay delay
-- instance {-# OVERLAPPABLE #-} (Monad m,MonadNondet m,LiftNondet t) ⇒ MonadNondet (t m) where
--   mzero = liftMzero mzero
--   (⊞) = liftMplus (⊞)
-- instance {-# OVERLAPPABLE #-} (Monad m,MonadTop m,LiftTop t) ⇒ MonadTop (t m) where
--   mtop = liftMtop mtop
-- instance {-# OVERLAPPABLE #-} (Monad m,MonadCont r m,LiftCont t) ⇒ MonadCont r (t m) where
--   callCC = liftCallCC callCC
--   withC = liftWithC withC

--------------
-- DERIVING --
--------------

deriveLiftIO ∷ ∀ t₁ t₂ m. (Monad m,t₁ ⇄⁼ t₂,LiftIO t₂) ⇒ (∀ a. IO a → m a) → (∀ a. IO a → t₁ m a)
deriveLiftIO ioM xM = isofr3 $ liftIO ioM xM

deriveLiftAskL ∷ ∀ t₁ t₂ m r. (Monad m,t₁ ⇄⁼ t₂,LiftReader t₂) ⇒ (∀ r'. r ⟢ r' → m r') → (∀ r'. r ⟢ r' → t₁ m r')
deriveLiftAskL askLM = isofr3 ∘ liftAskL askLM

deriveLiftLocalL ∷ ∀ t₁ t₂ m r. (Monad m,t₁ ⇄⁼ t₂,LiftReader t₂) ⇒ (∀ r' a. r ⟢ r' → r' → m a → m a) → (∀ r' a. r ⟢ r' → r' → t₁ m a → t₁ m a)
deriveLiftLocalL localLM ℓ r = isofr3 ∘ liftLocalL localLM ℓ r ∘ isoto3

deriveLiftTell ∷ ∀ t₁ t₂ m o. (Monad m,t₁ ⇄⁼ t₂,LiftWriter t₂) ⇒ (o → m ()) → (o → t₁ m ())
deriveLiftTell tellM = isofr3 ∘ liftTell tellM

deriveLiftHijack ∷ ∀ t₁ t₂ m o. (Monad m,t₁ ⇄⁼ t₂,LiftWriter t₂) ⇒ (∀ a. m a → m (o ∧ a)) → (∀ a. t₁ m a → t₁ m (o ∧ a))
deriveLiftHijack hijackM = isofr3 ∘ liftHijack hijackM ∘ isoto3

deriveLiftGet ∷ ∀ t₁ t₂ m s. (Monad m,t₁ ⇄⁼ t₂,LiftState t₂) ⇒ m s → t₁ m s
deriveLiftGet getM = isofr3 $ liftGet getM

deriveLiftPut ∷ ∀ t₁ t₂ m s. (Monad m,t₁ ⇄⁼ t₂,LiftState t₂) ⇒ (s → m ()) → (s → t₁ m ())
deriveLiftPut putM = isofr3 ∘ liftPut putM

deriveLiftAbort ∷ ∀ t₁ t₂ m. (Monad m,t₁ ⇄⁼ t₂,LiftFail t₂) ⇒ (∀ a. m a) → (∀ a. t₁ m a)
deriveLiftAbort abortM = isofr3 $ liftAbort abortM

deriveLiftTry ∷ ∀ t₁ t₂ m. (Monad m,t₁ ⇄⁼ t₂,LiftFail t₂) ⇒ (∀ a. m a → m a → m a) → (∀ a. t₁ m a → t₁ m a → t₁ m a)
deriveLiftTry tryM xM₁ xM₂ = isofr3 $ liftTry tryM (isoto3 xM₁) (isoto3 xM₂)

deriveLiftThrow ∷ ∀ t₁ t₂ m e. (Monad m,t₁ ⇄⁼ t₂,LiftError t₂) ⇒ (∀ a. e → m a) → (∀ a. e → t₁ m a)
deriveLiftThrow throwM e = isofr3 $ liftThrow throwM e

deriveLiftCatch ∷ ∀ t₁ t₂ m e. (Monad m,t₁ ⇄⁼ t₂,LiftError t₂) ⇒ (∀ a. m a → (e → m a) → m a) → (∀ a. t₁ m a → (e → t₁ m a) → t₁ m a)
deriveLiftCatch catchM xM k = isofr3 $ liftCatch catchM (isoto3 xM) (isoto3 ∘ k)

deriveLiftDelay ∷ ∀ t₁ t₂ m. (Monad m,t₁ ⇄⁼ t₂,LiftDelay t₂) ⇒ (∀ a. (() → m a) → m a) → (∀ a. (() → t₁ m a) → t₁ m a)
deriveLiftDelay delayM xMU = isofr3 $ liftDelay delayM $ isoto3 ∘ xMU

deriveLiftMzero ∷ ∀ t₁ t₂ m. (Monad m,t₁ ⇄⁼ t₂,LiftNondet t₂) ⇒ (∀ a. m a) → (∀ a. t₁ m a)
deriveLiftMzero mzeroM = isofr3 $ liftMzero mzeroM

deriveLiftMplus ∷ ∀ t₁ t₂ m. (Monad m,t₁ ⇄⁼ t₂,LiftNondet t₂) ⇒ (∀ a. m a → m a → m a) → (∀ a. t₁ m a → t₁ m a → t₁ m a)
deriveLiftMplus mplusM xM₁ xM₂ = isofr3 $ liftMplus mplusM (isoto3 xM₁) (isoto3 xM₂)

deriveLiftMtop ∷ ∀ t₁ t₂ m. (Monad m,t₁ ⇄⁼ t₂,LiftTop t₂) ⇒ (∀ a. m a) → (∀ a. t₁ m a)
deriveLiftMtop mtopM = isofr3 $ liftMtop mtopM

deriveLiftCallCC ∷ ∀ t₁ t₂ m r. (Monad m,t₁ ⇄⁼ t₂,LiftCont t₂) ⇒ (∀ a. ((a → m r) → m r) → m a) → (∀ a. ((a → t₁ m r) → t₁ m r) → t₁ m a)
deriveLiftCallCC callCCM kk = isofr3 $ liftCallCC callCCM $ \ (k ∷ a → t₂ m r) → isoto3 $ kk (isofr3 ∘ k)

deriveLiftWithC ∷ ∀ t₁ t₂ m r. (Monad m,t₁ ⇄⁼ t₂,LiftCont t₂) ⇒ (∀ a. (a → m r) → m a → m r) → (∀ a. (a → t₁ m r) → t₁ m a → t₁ m r)
deriveLiftWithC withCM k xM = isofr3 $ liftWithC withCM (isoto3 ∘ k) (isoto3 xM)

-- instance {-# OVERLAPPABLE #-} (t₁ ⇄⁼ t₂,LiftIO t₂) ⇒ LiftIO t₁ where
--   liftIO = deriveLiftIO
-- instance {-# OVERLAPPABLE #-} (t₁ ⇄⁼ t₂,LiftReader t₂) ⇒ LiftReader t₁ where
--   liftAskL = deriveLiftAskL
--   liftLocalL = deriveLiftLocalL
-- instance {-# OVERLAPPABLE #-} (t₁ ⇄⁼ t₂,LiftWriter t₂) ⇒ LiftWriter t₁ where
--   liftTell = deriveLiftTell
--   liftHijack = deriveLiftHijack
-- instance {-# OVERLAPPABLE #-} (t₁ ⇄⁼ t₂,LiftState t₂) ⇒ LiftState t₁ where
--   liftGet = deriveLiftGet
--   liftPut = deriveLiftPut
-- instance {-# OVERLAPPABLE #-} (t₁ ⇄⁼ t₂,LiftFail t₂) ⇒ LiftFail t₁ where
--   liftAbort = deriveLiftAbort
--   liftTry = deriveLiftTry
-- instance {-# OVERLAPPABLE #-} (t₁ ⇄⁼ t₂,LiftError t₂) ⇒ LiftError t₁ where
--   liftThrow = deriveLiftThrow
--   liftCatch = deriveLiftCatch
-- instance {-# OVERLAPPABLE #-} (t₁ ⇄⁼ t₂,LiftDelay t₂) ⇒ LiftDelay t₁ where
--   liftDelay = deriveLiftDelay
-- instance {-# OVERLAPPABLE #-} (t₁ ⇄⁼ t₂,LiftNondet t₂) ⇒ LiftNondet t₁ where
--   liftMzero = deriveLiftMzero
--   liftMplus = deriveLiftMplus
-- instance {-# OVERLAPPABLE #-} (t₁ ⇄⁼ t₂,LiftTop t₂) ⇒ LiftTop t₁ where
--   liftMtop = deriveLiftMtop
-- instance {-# OVERLAPPABLE #-} (t₁ ⇄⁼ t₂,LiftCont t₂) ⇒ LiftCont t₁ where
--   liftCallCC = deriveLiftCallCC
--   liftWithC = deriveLiftWithC
