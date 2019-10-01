module UVMHS.Core.Transformers where

import UVMHS.Core.Init
import UVMHS.Core.Classes

import UVMHS.Core.Effects

-------------
-- COMPOSE --
-------------

infixr 6 ⊡

newtype (⊡) (t₁ ∷ (★ → ★) → (★ → ★)) (t₂ ∷ (★ → ★) → (★ → ★)) m a = Compose2 { unCompose2 ∷ t₁ (t₂ m) a }

instance (∀ m'. Monad m' ⇒ Monad (t₁ m'),∀ m'. Monad m' ⇒ Monad (t₂ m'),Monad m) ⇒ Functor ((t₁ ⊡ t₂) m) where 
  {-# INLINE map #-}
  map = mmap
instance (∀ m'. Monad m' ⇒ Monad (t₁ m'),∀ m'. Monad m' ⇒ Monad (t₂ m'),Monad m) ⇒ Return ((t₁ ⊡ t₂) m) where
  {-# INLINE return #-}
  return ∷ ∀ a. a → (t₁ ⊡ t₂) m a
  return x = Compose2 $ return x
instance (∀ m'. Monad m' ⇒ Monad (t₁ m'),∀ m'. Monad m' ⇒ Monad (t₂ m'),Monad m) ⇒ Bind ((t₁ ⊡ t₂) m) where
  {-# INLINE (≫=) #-}
  (≫=) ∷ ∀ a b. (t₁ ⊡ t₂) m a → (a → (t₁ ⊡ t₂) m b) → (t₁ ⊡ t₂) m b
  xM ≫= k = Compose2 $ do
    x ← unCompose2 xM
    unCompose2 $ k x
instance (∀ m'. Monad m' ⇒ Monad (t₁ m'),∀ m'. Monad m' ⇒ Monad (t₂ m'),Monad m) ⇒ Monad ((t₁ ⊡ t₂) m)

instance (Functor2 t₁,Functor2 t₂) ⇒ Functor2 (t₁ ⊡ t₂) where
  {-# INLINE map2 #-}
  map2 ∷ ∀ m₁ m₂. (∀ a. m₁ a → m₂ a) → (∀ a. (t₁ ⊡ t₂) m₁ a → (t₁ ⊡ t₂) m₂ a)
  map2 f = Compose2 ∘ map2 (map2 f) ∘ unCompose2

instance (∀ m'. Monad m' ⇒ Monad (t₂ m'),LiftIO t₁,LiftIO t₂) ⇒ LiftIO (t₁ ⊡ t₂) where
  {-# INLINE liftIO #-}
  liftIO ∷ ∀ m. (Monad m) ⇒ (∀ a. IO a → m a) → (∀ a. IO a → (t₁ ⊡ t₂) m a)
  liftIO ioM xM = Compose2 $ liftIO (liftIO ioM) xM

instance (∀ m'. Monad m' ⇒ Monad (t₂ m'),LiftReader t₁,LiftReader t₂) ⇒ LiftReader (t₁ ⊡ t₂) where
  {-# INLINE liftAsk #-}
  liftAsk ∷ ∀ m r. (Monad m) ⇒ m r → (t₁ ⊡ t₂) m r
  liftAsk askM = Compose2 $ liftAsk $ liftAsk askM

  {-# INLINE liftLocal #-}
  liftLocal ∷ ∀ m r. (Monad m) ⇒ (∀ a. r → m a → m a) → (∀ a. r → (t₁ ⊡ t₂) m a → (t₁ ⊡ t₂) m a)
  liftLocal localM r = Compose2 ∘ (liftLocal $ liftLocal localM) r ∘ unCompose2

instance (∀ m'. Monad m' ⇒ Monad (t₂ m'),LiftWriter t₁,LiftWriter t₂) ⇒ LiftWriter (t₁ ⊡ t₂) where
  {-# INLINE liftTell #-}
  liftTell ∷ ∀ m o. (Monad m) ⇒ (o → m ()) → (o → (t₁ ⊡ t₂) m ())
  liftTell tellM = Compose2 ∘ (liftTell $ liftTell tellM)

  {-# INLINE liftHijack #-}
  liftHijack ∷ ∀ m o. (Monad m) ⇒ (∀ a. m a → m (o ∧ a)) → (∀ a. (t₁ ⊡ t₂) m a → (t₁ ⊡ t₂) m (o ∧ a))
  liftHijack hijackM = Compose2 ∘ (liftHijack $ liftHijack hijackM) ∘ unCompose2

instance (∀ m'. Monad m' ⇒ Monad (t₂ m'),LiftState t₁,LiftState t₂) ⇒ LiftState (t₁ ⊡ t₂) where
  {-# INLINE liftGet #-}
  liftGet ∷ ∀ m s. (Monad m) ⇒ m s → (t₁ ⊡ t₂) m s
  liftGet getM = Compose2 $ liftGet $ liftGet getM

  {-# INLINE liftPut #-}
  liftPut ∷ ∀ m s. (Monad m) ⇒ (s → m ()) → (s → (t₁ ⊡ t₂) m ())
  liftPut putM = Compose2 ∘ (liftPut $ liftPut putM)

instance (∀ m'. Monad m' ⇒ Monad (t₂ m'),LiftFail t₁,LiftFail t₂) ⇒ LiftFail (t₁ ⊡ t₂) where
  {-# INLINE liftAbort #-}
  liftAbort ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. (t₁ ⊡ t₂) m a)
  liftAbort abortM = Compose2 $ liftAbort $ liftAbort abortM

  {-# INLINE liftTry #-}
  liftTry ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. (t₁ ⊡ t₂) m a → (t₁ ⊡ t₂) m a → (t₁ ⊡ t₂) m a)
  liftTry tryM xM₁ xM₂ = Compose2 $ (liftTry $ liftTry tryM) (unCompose2 xM₁) (unCompose2 xM₂)

instance (∀ m'. Monad m' ⇒ Monad (t₂ m'),LiftError t₁,LiftError t₂) ⇒ LiftError (t₁ ⊡ t₂) where
  {-# INLINE liftThrow #-}
  liftThrow ∷ ∀ m e. (Monad m) ⇒ (∀ a. e → m a) → (∀ a. e → (t₁ ⊡ t₂) m a)
  liftThrow throwM = Compose2 ∘ (liftThrow $ liftThrow throwM)

  {-# INLINE liftCatch #-}
  liftCatch ∷ ∀ m e. (Monad m) ⇒ (∀ a. m a → (e → m a) → m a) → (∀ a. (t₁ ⊡ t₂) m a → (e → (t₁ ⊡ t₂) m a) → (t₁ ⊡ t₂) m a)
  liftCatch catchM xM k = Compose2 $ (liftCatch $ liftCatch catchM) (unCompose2 xM) (unCompose2 ∘ k)

instance (∀ m'. Monad m' ⇒ Monad (t₂ m'),LiftNondet t₁,LiftNondet t₂) ⇒ LiftNondet (t₁ ⊡ t₂) where
  {-# INLINE liftMzero #-}
  liftMzero ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. (t₁ ⊡ t₂) m a)
  liftMzero mzeroM = Compose2 $ liftMzero $ liftMzero mzeroM

  {-# INLINE liftMplus #-}
  liftMplus ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. (t₁ ⊡ t₂) m a → (t₁ ⊡ t₂) m a → (t₁ ⊡ t₂) m a)
  liftMplus mplusM xM₁ xM₂ = Compose2 $ (liftMplus $ liftMplus mplusM) (unCompose2 xM₁) (unCompose2 xM₂)

instance (∀ m'. Monad m' ⇒ Monad (t₂ m'),LiftTop t₁,LiftTop t₂) ⇒ LiftTop (t₁ ⊡ t₂) where
  {-# INLINE liftMtop #-}
  liftMtop ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. (t₁ ⊡ t₂) m a)
  liftMtop mtopM = Compose2 $ liftMtop $ liftMtop mtopM

instance (∀ m'. Monad m' ⇒ Monad (t₂ m'),LiftCont t₁,LiftCont t₂) ⇒ LiftCont (t₁ ⊡ t₂) where
  {-# INLINE liftCallCC #-}
  liftCallCC ∷ ∀ m r. (Monad m) ⇒ (∀ a. ((a → m r) → m r) → m a) → (∀ a. ((a → (t₁ ⊡ t₂) m r) → (t₁ ⊡ t₂) m r) → (t₁ ⊡ t₂) m a)
  liftCallCC callCCM kk = Compose2 $ (liftCallCC $ liftCallCC callCCM) $ \ (k ∷ a → t₁ (t₂ m) r) → unCompose2 $ kk (Compose2 ∘ k)

  {-# INLINE liftWithC #-}
  liftWithC ∷ ∀ m r. (Monad m) ⇒ (∀ a. (a → m r) → m a → m r) →  (∀ a. (a → (t₁ ⊡ t₂) m r) → (t₁ ⊡ t₂) m a → (t₁ ⊡ t₂) m r)
  liftWithC withCM k xM = Compose2 $ (liftWithC $ liftWithC withCM) (unCompose2 ∘ k) (unCompose2 xM)

-------------
-- LIFTING --
-------------

instance {-# OVERLAPPABLE #-} (Monad m,MonadIO m,LiftIO t) ⇒ MonadIO (t m) where
  {-# INLINE io #-}
  io = liftIO io
instance {-# OVERLAPPABLE #-} (Monad m,MonadReader r m,LiftReader t) ⇒ MonadReader r (t m) where
  {-# INLINE ask #-}
  ask = liftAsk ask
  {-# INLINE local #-}
  local = liftLocal local
instance {-# OVERLAPPABLE #-} (Monad m,MonadWriter o m,LiftWriter t) ⇒ MonadWriter o (t m) where
  {-# INLINE tell #-}
  tell = liftTell tell
  {-# INLINE hijack #-}
  hijack = liftHijack hijack
instance {-# OVERLAPPABLE #-} (Monad m,MonadState s m,LiftState t) ⇒ MonadState s (t m) where
  {-# INLINE get #-}
  get = liftGet get
  {-# INLINE put #-}
  put = liftPut put
instance {-# OVERLAPPABLE #-} (Monad m,MonadFail m,LiftFail t) ⇒ MonadFail (t m) where
  {-# INLINE abort #-}
  abort = liftAbort abort
  {-# INLINE (⎅) #-}
  (⎅) = liftTry (⎅)
instance {-# OVERLAPPABLE #-} (Monad m,MonadError e m,LiftError t) ⇒ MonadError e (t m) where
  {-# INLINE throw #-}
  throw = liftThrow throw
  {-# INLINE catch #-}
  catch = liftCatch catch
instance {-# OVERLAPPABLE #-} (Monad m,MonadNondet m,LiftNondet t) ⇒ MonadNondet (t m) where
  {-# INLINE mzero #-}
  mzero = liftMzero mzero
  {-# INLINE (⊞) #-}
  (⊞) = liftMplus (⊞)
instance {-# OVERLAPPABLE #-} (Monad m,MonadTop m,LiftTop t) ⇒ MonadTop (t m) where
  {-# INLINE mtop #-}
  mtop = liftMtop mtop
instance {-# OVERLAPPABLE #-} (Monad m,MonadCont r m,LiftCont t) ⇒ MonadCont r (t m) where
  {-# INLINE callCC #-}
  callCC = liftCallCC callCC
  {-# INLINE withC #-}
  withC = liftWithC withC

--------------
-- DERIVING --
--------------

{-# INLINE deriveLiftIO #-}
deriveLiftIO ∷ ∀ t₁ t₂ m. (Monad m,t₁ ⇄⁼ t₂,LiftIO t₂) ⇒ (∀ a. IO a → m a) → (∀ a. IO a → t₁ m a)
deriveLiftIO ioM xM = isofr3 $ liftIO ioM xM

{-# INLINE deriveLiftAsk #-}
deriveLiftAsk ∷ ∀ t₁ t₂ m r. (Monad m,t₁ ⇄⁼ t₂,LiftReader t₂) ⇒ m r → t₁ m r
deriveLiftAsk askM = isofr3 $ liftAsk askM

{-# INLINE deriveLiftLocal #-}
deriveLiftLocal ∷ ∀ t₁ t₂ m r. (Monad m,t₁ ⇄⁼ t₂,LiftReader t₂) ⇒ (∀ a. r → m a → m a) → (∀ a. r → t₁ m a → t₁ m a)
deriveLiftLocal localM r = isofr3 ∘ liftLocal localM r ∘ isoto3

{-# INLINE deriveLiftTell #-}
deriveLiftTell ∷ ∀ t₁ t₂ m o. (Monad m,t₁ ⇄⁼ t₂,LiftWriter t₂) ⇒ (o → m ()) → (o → t₁ m ())
deriveLiftTell tellM = isofr3 ∘ liftTell tellM

{-# INLINE deriveLiftHijack #-}
deriveLiftHijack ∷ ∀ t₁ t₂ m o. (Monad m,t₁ ⇄⁼ t₂,LiftWriter t₂) ⇒ (∀ a. m a → m (o ∧ a)) → (∀ a. t₁ m a → t₁ m (o ∧ a))
deriveLiftHijack hijackM = isofr3 ∘ liftHijack hijackM ∘ isoto3

{-# INLINE deriveLiftGet #-}
deriveLiftGet ∷ ∀ t₁ t₂ m s. (Monad m,t₁ ⇄⁼ t₂,LiftState t₂) ⇒ m s → t₁ m s
deriveLiftGet getM = isofr3 $ liftGet getM

{-# INLINE deriveLiftPut #-}
deriveLiftPut ∷ ∀ t₁ t₂ m s. (Monad m,t₁ ⇄⁼ t₂,LiftState t₂) ⇒ (s → m ()) → (s → t₁ m ())
deriveLiftPut putM = isofr3 ∘ liftPut putM

{-# INLINE deriveLiftAbort #-}
deriveLiftAbort ∷ ∀ t₁ t₂ m. (Monad m,t₁ ⇄⁼ t₂,LiftFail t₂) ⇒ (∀ a. m a) → (∀ a. t₁ m a)
deriveLiftAbort abortM = isofr3 $ liftAbort abortM

{-# INLINE deriveLiftTry #-}
deriveLiftTry ∷ ∀ t₁ t₂ m. (Monad m,t₁ ⇄⁼ t₂,LiftFail t₂) ⇒ (∀ a. m a → m a → m a) → (∀ a. t₁ m a → t₁ m a → t₁ m a)
deriveLiftTry tryM xM₁ xM₂ = isofr3 $ liftTry tryM (isoto3 xM₁) (isoto3 xM₂)

{-# INLINE deriveLiftThrow #-}
deriveLiftThrow ∷ ∀ t₁ t₂ m e. (Monad m,t₁ ⇄⁼ t₂,LiftError t₂) ⇒ (∀ a. e → m a) → (∀ a. e → t₁ m a)
deriveLiftThrow throwM e = isofr3 $ liftThrow throwM e

{-# INLINE deriveLiftCatch #-}
deriveLiftCatch ∷ ∀ t₁ t₂ m e. (Monad m,t₁ ⇄⁼ t₂,LiftError t₂) ⇒ (∀ a. m a → (e → m a) → m a) → (∀ a. t₁ m a → (e → t₁ m a) → t₁ m a)
deriveLiftCatch catchM xM k = isofr3 $ liftCatch catchM (isoto3 xM) (isoto3 ∘ k)

{-# INLINE deriveLiftMzero #-}
deriveLiftMzero ∷ ∀ t₁ t₂ m. (Monad m,t₁ ⇄⁼ t₂,LiftNondet t₂) ⇒ (∀ a. m a) → (∀ a. t₁ m a)
deriveLiftMzero mzeroM = isofr3 $ liftMzero mzeroM

{-# INLINE deriveLiftMplus #-}
deriveLiftMplus ∷ ∀ t₁ t₂ m. (Monad m,t₁ ⇄⁼ t₂,LiftNondet t₂) ⇒ (∀ a. m a → m a → m a) → (∀ a. t₁ m a → t₁ m a → t₁ m a)
deriveLiftMplus mplusM xM₁ xM₂ = isofr3 $ liftMplus mplusM (isoto3 xM₁) (isoto3 xM₂)

{-# INLINE deriveLiftMtop #-}
deriveLiftMtop ∷ ∀ t₁ t₂ m. (Monad m,t₁ ⇄⁼ t₂,LiftTop t₂) ⇒ (∀ a. m a) → (∀ a. t₁ m a)
deriveLiftMtop mtopM = isofr3 $ liftMtop mtopM

{-# INLINE deriveLiftCallCC #-}
deriveLiftCallCC ∷ ∀ t₁ t₂ m r. (Monad m,t₁ ⇄⁼ t₂,LiftCont t₂) ⇒ (∀ a. ((a → m r) → m r) → m a) → (∀ a. ((a → t₁ m r) → t₁ m r) → t₁ m a)
deriveLiftCallCC callCCM kk = isofr3 $ liftCallCC callCCM $ \ (k ∷ a → t₂ m r) → isoto3 $ kk (isofr3 ∘ k)

{-# INLINE deriveLiftWithC #-}
deriveLiftWithC ∷ ∀ t₁ t₂ m r. (Monad m,t₁ ⇄⁼ t₂,LiftCont t₂) ⇒ (∀ a. (a → m r) → m a → m r) → (∀ a. (a → t₁ m r) → t₁ m a → t₁ m r)
deriveLiftWithC withCM k xM = isofr3 $ liftWithC withCM (isoto3 ∘ k) (isoto3 xM)

instance {-# OVERLAPPABLE #-} (t₁ ⇄⁼ t₂,LiftIO t₂) ⇒ LiftIO t₁ where
  {-# INLINE liftIO #-}
  liftIO = deriveLiftIO
instance {-# OVERLAPPABLE #-} (t₁ ⇄⁼ t₂,LiftReader t₂) ⇒ LiftReader t₁ where
  {-# INLINE liftAsk #-}
  liftAsk = deriveLiftAsk
  {-# INLINE liftLocal #-}
  liftLocal = deriveLiftLocal
instance {-# OVERLAPPABLE #-} (t₁ ⇄⁼ t₂,LiftWriter t₂) ⇒ LiftWriter t₁ where
  {-# INLINE liftTell #-}
  liftTell = deriveLiftTell
  {-# INLINE liftHijack #-}
  liftHijack = deriveLiftHijack
instance {-# OVERLAPPABLE #-} (t₁ ⇄⁼ t₂,LiftState t₂) ⇒ LiftState t₁ where
  {-# INLINE liftGet #-}
  liftGet = deriveLiftGet
  {-# INLINE liftPut #-}
  liftPut = deriveLiftPut
instance {-# OVERLAPPABLE #-} (t₁ ⇄⁼ t₂,LiftFail t₂) ⇒ LiftFail t₁ where
  {-# INLINE liftAbort #-}
  liftAbort = deriveLiftAbort
  {-# INLINE liftTry #-}
  liftTry = deriveLiftTry
instance {-# OVERLAPPABLE #-} (t₁ ⇄⁼ t₂,LiftError t₂) ⇒ LiftError t₁ where
  {-# INLINE liftThrow #-}
  liftThrow = deriveLiftThrow
  {-# INLINE liftCatch #-}
  liftCatch = deriveLiftCatch
instance {-# OVERLAPPABLE #-} (t₁ ⇄⁼ t₂,LiftNondet t₂) ⇒ LiftNondet t₁ where
  {-# INLINE liftMzero #-}
  liftMzero = deriveLiftMzero
  {-# INLINE liftMplus #-}
  liftMplus = deriveLiftMplus
instance {-# OVERLAPPABLE #-} (t₁ ⇄⁼ t₂,LiftTop t₂) ⇒ LiftTop t₁ where
  {-# INLINE liftMtop #-}
  liftMtop = deriveLiftMtop
instance {-# OVERLAPPABLE #-} (t₁ ⇄⁼ t₂,LiftCont t₂) ⇒ LiftCont t₁ where
  {-# INLINE liftCallCC #-}
  liftCallCC = deriveLiftCallCC
  {-# INLINE liftWithC #-}
  liftWithC = deriveLiftWithC

