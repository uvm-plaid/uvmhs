module UVMHS.Core.Effects where

import UVMHS.Core.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data

import UVMHS.Core.Lens

infixl 5 ⊞,⎅

class MonadIO (m ∷ ★ → ★) where io ∷ IO a → m a

class LiftIO t where
  liftIO ∷ ∀ m. (Monad m) ⇒ (∀ a. IO a → m a) → (∀ a. IO a → t m a)

class MonadReader r m | m → r where
  ask ∷ m r
  local ∷ ∀ a. r → m a → m a

class LiftReader t where
  liftAsk ∷ ∀ m r. (Monad m) ⇒ m r → t m r
  liftLocal ∷ ∀ m r. (Monad m) ⇒ (∀ a. r → m a → m a) → (∀ a. r → t m a → t m a)

class MonadWriter o m | m → o where
  tell ∷ o → m ()
  hijack ∷ ∀ a. m a → m (o ∧ a)

class LiftWriter t where
  liftTell ∷ ∀ m o. (Monad m) ⇒ (o → m ()) → (o → t m ())
  liftHijack ∷ ∀ m o. (Monad m) ⇒ (∀ a. m a → m (o ∧ a)) → (∀ a. t m a → t m (o ∧ a))

class MonadState s m | m → s where
  get ∷ m s
  put ∷ s → m ()

class LiftState t where
  liftGet ∷ ∀ m s. (Monad m) ⇒ m s → t m s
  liftPut ∷ ∀ m s. (Monad m) ⇒ (s → m ()) → (s → t m ())

class MonadFail m where
  abort ∷ ∀ a. m a
  (⎅) ∷ ∀ a. m a → m a → m a

class LiftFail t where
  liftAbort ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. t m a)
  liftTry ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. t m a → t m a → t m a)

class MonadError e m | m → e where
  throw ∷ ∀ a. e → m a
  catch ∷ ∀ a. m a → (e → m a) → m a

class LiftError t where
  liftThrow ∷ ∀ m e. (Monad m) ⇒ (∀ a. e → m a) → (∀ a. e → t m a)
  liftCatch ∷ ∀ m e. (Monad m) ⇒ (∀ a. m a → (e → m a) → m a) → (∀ a. t m a → (e → t m a) → t m a)

class MonadNondet m where
  mzero ∷ ∀ a. m a
  (⊞) ∷ ∀ a. m a → m a → m a

class LiftNondet t where
  liftMzero ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. t m a)
  liftMplus ∷ ∀ m. (Monad m) ⇒ (∀ a. m a → m a → m a) → (∀ a. t m a → t m a → t m a)

class MonadTop m where
  mtop ∷ ∀ a. m a

class LiftTop t where
  liftMtop ∷ ∀ m. (Monad m) ⇒ (∀ a. m a) → (∀ a. t m a)

class MonadCont r m | m → r where
  callCC ∷ ∀ a. ((a → m r) → m r) → m a 
  withC ∷ ∀ a. (a → m r) → m a → m r 

class LiftCont t where
  liftCallCC ∷ ∀ m r. (Monad m) ⇒ (∀ a. ((a → m r) → m r) → m a) → (∀ a. ((a → t m r) → t m r) → t m a)
  liftWithC ∷ ∀ m r. (Monad m) ⇒ (∀ a. (a → m r) → m a → m r) → (∀ a. (a → t m r) → t m a → t m r)

------------------------
-- STANDARD INSTANCES --
------------------------

instance MonadReader r ((→) r) where
  {-# INLINE ask #-}
  ask ∷ r → r
  ask = id

  {-# INLINE local #-}
  local ∷ ∀ a. r → (r → a) → (r → a)
  local r f = const $ f r

instance (Null o) ⇒ MonadWriter o ((∧) o) where
  {-# INLINE tell #-}
  tell ∷ o → (o ∧ ())
  tell o = (o :* ())

  {-# INLINE hijack #-}
  hijack ∷ ∀ a. o ∧ a → o ∧ (o ∧ a)
  hijack ox = null :* ox

instance MonadFail 𝑂 where
  {-# INLINE abort #-}
  abort ∷ ∀ a. 𝑂 a
  abort = None

  {-# INLINE (⎅) #-}
  (⎅) ∷ ∀ a. 𝑂 a → 𝑂 a → 𝑂 a
  None ⎅ xM = xM
  Some x ⎅ _ = Some x

instance MonadError e ((∨) e) where
  {-# INLINE throw #-}
  throw ∷ ∀ a. e → e ∨ a
  throw = Inl

  {-# INLINE catch #-}
  catch ∷ ∀ a. e ∨ a → (e → e ∨ a) → e ∨ a
  catch (Inl e) k = k e
  catch (Inr x) _ = Inr x

instance MonadNondet 𝐼 where
  {-# INLINE mzero #-}
  mzero ∷ ∀ a. 𝐼 a
  mzero = null

  {-# INLINE (⊞) #-}
  (⊞) ∷ ∀ a. 𝐼 a → 𝐼 a → 𝐼 a
  (⊞) = (⧺)

instance MonadNondet 𝐿 where
  {-# INLINE mzero #-}
  mzero ∷ ∀ a. 𝐿 a
  mzero = null

  {-# INLINE (⊞) #-}
  (⊞) ∷ ∀ a. 𝐿 a → 𝐿 a → 𝐿 a
  (⊞) = (⧺)

instance MonadNondet 𝑄 where
  {-# INLINE mzero #-}
  mzero ∷ ∀ a. 𝑄 a
  mzero = null

  {-# INLINE (⊞) #-}
  (⊞) ∷ ∀ a. 𝑄 a → 𝑄 a → 𝑄 a
  (⊞) = (⧺)

----------------
-- OPERATIONS --
----------------

-- Reader

{-# INLINE askL #-}
askL ∷ (Monad m,MonadReader r m) ⇒ r ⟢ a → m a 
askL l = access l ^$ ask

{-# INLINE mapEnv #-}
mapEnv ∷ (Monad m,MonadReader r m) ⇒ (r → r) → m a → m a 
mapEnv f aM = do
  r ← ask
  local (f r) aM

{-# INLINE localL #-}
localL ∷ (Monad m,MonadReader r₁ m) ⇒ (r₁ ⟢ r₂) → r₂ → m a → m a
localL 𝓁 r = mapEnv $ update 𝓁 r

{-# INLINE mapEnvL #-}
mapEnvL ∷ (Monad m,MonadReader r₁ m) ⇒ (r₁ ⟢ r₂) → (r₂ → r₂) → m a → m a
mapEnvL 𝓁 f = mapEnv $ alter 𝓁 f

-- Writer

{-# INLINE tellL #-}
tellL ∷ (Monoid o₁,Monad m,MonadWriter o₁ m) ⇒ o₁ ⟢ o₂ → o₂ → m ()
tellL l o = tell $ update l o null

{-# INLINE hijackL #-}
hijackL ∷ (Monad m,MonadWriter o₁ m,Monoid o₂) ⇒ o₁ ⟢ o₂ → m a → m (o₂ ∧ a)
hijackL l aM = do
  (o₁ :* a) ← hijack aM
  tell $ update l null o₁
  return (access l o₁ :* a)

{-# INLINE mapOut #-}
mapOut ∷ (Monad m,MonadWriter o m) ⇒ (o → o) → m a → m a
mapOut f aM = do
  (o :* a) ← hijack aM
  tell $ f o
  return a

{-# INLINE retOut #-}
retOut ∷ ∀ o m a. (Monad m,MonadWriter o m) ⇒ m a → m o
retOut xM = do
  (o :* _) ← hijack xM
  return o

-- # State

{-# INLINE getL #-}
getL ∷ (Monad m,MonadState s m) ⇒ s ⟢ a → m a 
getL l = map (access l) get

{-# INLINE putL #-}
putL ∷ (Monad m,MonadState s m) ⇒ s ⟢ a → a → m () 
putL 𝓁 = modify ∘ update 𝓁

{-# INLINE modify #-}
modify ∷ (Monad m,MonadState s m) ⇒ (s → s) → m () 
modify f = do
  s ← get
  put $ f s

{-# INLINE modifyM #-}
modifyM ∷ (Monad m,MonadState s m) ⇒ (s → m s) → m () 
modifyM f = do
  s ← get
  put *$ f s

{-# INLINE modifyL #-}
modifyL ∷ (Monad m,MonadState s m) ⇒ s ⟢ a → (a → a) → m () 
modifyL 𝓁 = modify ∘ alter 𝓁

{-# INLINE modifyML #-}
modifyML ∷ (Monad m,MonadState s m) ⇒ s ⟢ a → (a → m a) → m () 
modifyML 𝓁 = modifyM ∘ alterM 𝓁

{-# INLINE getput #-}
getput ∷ (Monad m,MonadState s m) ⇒ s → m s
getput s = do
  s' ← get
  put s
  return s'

{-# INLINE getputL #-}
getputL ∷ (Monad m,MonadState s₁ m) ⇒ s₁ ⟢ s₂ → s₂ → m s₂
getputL 𝓁 x = do
  x' ← getL 𝓁
  putL 𝓁 x
  return x'

{-# INLINE next #-}
next ∷ (Monad m,MonadState s m,Multiplicative s) ⇒ m s
next = do
  i ← get
  put $ succ i
  return i

{-# INLINE nextL #-}
nextL ∷ (Monad m,MonadState s m,Multiplicative a) ⇒ s ⟢ a → m a
nextL l = do
  i ← getL l
  putL l $ succ i
  return i

{-# INLINE bump #-}
bump ∷ (Monad m,MonadState s m,Multiplicative s) ⇒ m ()
bump = modify succ

{-# INLINE bumpL #-}
bumpL ∷ (Monad m,MonadState s m,Multiplicative a) ⇒ s ⟢ a → m ()
bumpL l = modifyL l succ

{-# INLINE localize #-}
localize ∷ (Monad m,MonadState s m) ⇒ s → m a → m (s ∧ a)
localize s xM = do
  s' ← getput s
  x ← xM
  s'' ← getput s'
  return (s'' :* x)

{-# INLINE localizeL #-}
localizeL ∷ (Monad m,MonadState s₁ m) ⇒ s₁ ⟢ s₂ → s₂ → m a → m (s₂ ∧ a)
localizeL 𝓁 s₂ aM = do
  s₂' ← getputL 𝓁 s₂
  x ← aM
  s₂'' ← getputL 𝓁 s₂'
  return (s₂'' :* x)

{-# INLINE localState #-}
localState ∷ (Monad m,MonadState s m) ⇒ s → m a → m a
localState s = map snd ∘ localize s

{-# INLINE localStateL #-}
localStateL ∷ (Monad m,MonadState s₁ m) ⇒ s₁ ⟢ s₂ → s₂ → m a → m a
localStateL 𝓁 s = map snd ∘ localizeL 𝓁 s

{-# INLINE retState #-}
retState ∷ ∀ s m a. (Monad m,MonadState s m) ⇒ m a → m s
retState xM = do
  _ ← xM
  get

{-# INLINE tellStateL #-}
tellStateL ∷ (Monad m,MonadState o₁ m,Append o₂) ⇒ o₁ ⟢ o₂ → o₂ → m ()
tellStateL 𝓁 o = modifyL 𝓁 $ (⧺) o

{-# INLINE hijackStateL #-}
hijackStateL ∷ (Monad m,MonadState o₁ m,Null o₂) ⇒ o₁ ⟢ o₂ → m a → m (o₂ ∧ a)
hijackStateL 𝓁 aM = localizeL 𝓁 null aM

localMapStateL ∷ (Monad m,MonadState s₁ m) ⇒ s₁ ⟢ s₂ → (s₂ → s₂) → m a → m a
localMapStateL ℓ f xM = do
  s ← getL ℓ
  snd ^$ localizeL ℓ (f s) xM

localStateEffectsL ∷ (Monad m,MonadState s₁ m) ⇒ s₁ ⟢ s₂ → m a → m a
localStateEffectsL ℓ xM = do
  s ← getL ℓ
  localStateL ℓ s xM

-- Fail

{-# INLINE abort𝑂 #-}
abort𝑂 ∷ (Monad m,MonadFail m) ⇒ 𝑂 a → m a
abort𝑂 = elim𝑂 abort return

{-# INLINE tries #-}
tries ∷ (Monad m,MonadFail m,ToIter (m a) t) ⇒ t → m a
tries = foldr abort (⎅)

{-# INLINE guard #-}
guard ∷ (Monad m,MonadFail m) ⇒ 𝔹 → m ()
guard = \case
  True → return ()
  False → abort

oneOrMoreSplit ∷ (Monad m,MonadFail m) ⇒ m a → m (a ∧ 𝐿 a)
oneOrMoreSplit aM = do
  x ← aM
  xs ← many aM
  return $ x :* xs

oneOrMore ∷ (Monad m,MonadFail m) ⇒ m a → m (𝐿 a)
oneOrMore xM = do
  (x :* xs) ← oneOrMoreSplit xM
  return $ x :& xs

many ∷ (Monad m,MonadFail m) ⇒ m a → m (𝐿 a)
many aM = tries
  [ oneOrMore aM
  , return null
  ]

-- Error

{-# INLINE throw𝑂 #-}
throw𝑂 ∷ (Monad m,MonadError e m) ⇒ e → 𝑂 a → m a 
throw𝑂 e = elim𝑂 (throw e) return

-- # Nondet

{-# INLINE mconcat #-}
mconcat ∷ (MonadNondet m,ToIter (m a) t) ⇒ t → m a
mconcat = foldr mzero (⊞)

{-# INLINE from #-}
from ∷ (Monad m,MonadNondet m,ToIter a t) ⇒ t → m a
from = mconcat ∘ map return ∘ iter

oneOrMoreSplitNT ∷ (Monad m,MonadNondet m) ⇒ m a → m (a ∧ 𝐿 a)
oneOrMoreSplitNT aM = do
  x ← aM
  xs ← manyNT aM
  return $ x :* xs

oneOrMoreNT ∷ (Monad m,MonadNondet m) ⇒ m a → m (𝐿 a)
oneOrMoreNT xM = do
  (x :* xs) ← oneOrMoreSplitNT xM
  return $ x :& xs

manyNT ∷ (Monad m,MonadNondet m) ⇒ m a → m (𝐿 a)
manyNT aM = mconcat
  [ oneOrMoreNT aM
  , return null
  ]

twoOrMoreSplitNT ∷ (Monad m,MonadNondet m) ⇒ m a → m (a ∧ a ∧ 𝐿 a)
twoOrMoreSplitNT aM = do
  x₁ ← aM
  (x₂ :* xs) ← oneOrMoreSplitNT aM
  return (x₁ :* x₂ :* xs)

manySepBy ∷ (Monad m,MonadNondet m) ⇒ m () → m a → m (𝐿 a)
manySepBy uM xM = mconcat
  [ do
      x ← xM
      xs ← manyPrefBy uM xM
      return $ x :& xs
  , return null
  ]

manyPrefBy ∷ (Monad m,MonadNondet m) ⇒ m () → m a → m (𝐿 a)
manyPrefBy uM xM = mconcat
  [ do
      uM
      x ← xM
      xs ← manyPrefBy uM xM
      return $ x :& xs
  , return null
  ]

mzero𝑂 ∷ (Monad m,MonadNondet m) ⇒ 𝑂 a → m a
mzero𝑂 = elim𝑂 mzero return

return𝑃 ∷ ∀ m a. (Monad m,MonadNondet m) ⇒ 𝑃 a → m a
return𝑃 = fold mzero (\ x xM → xM ⊞ return x)

-- Cont

reset ∷ (Monad m,MonadCont r m) ⇒ m r → m r 
reset aM = callCC $ \ k → k *$ withC return aM

modifyC ∷ (Monad m,MonadCont r m) ⇒ (r → m r) → m ()
modifyC f = callCC $ \ k → f *$ k ()

withCOn ∷ (Monad m,MonadCont r m) ⇒ m a → (a → m r) → m r
withCOn = flip withC

-- this doesn't do anything???
-- delimit ∷ (Monad m,MonadCont r m) ⇒ m a → m a
-- delimit xM = callCC $ \ (𝓀 ∷ a → m r) → 𝓀 *$ xM

putEnvL ∷ (Monad m,MonadReader r m,MonadCont kr m) ⇒ r ⟢ r' → r' → m ()
putEnvL l x = callCC $ \ 𝓀 → localL l x $ 𝓀 ()

modifyEnvL ∷ (Monad m,MonadReader r m,MonadCont kr m) ⇒ r ⟢ r' → (r' → r') → m ()
modifyEnvL l f = callCC $ \ 𝓀 → mapEnvL l f $ 𝓀 ()

--------------
-- DERIVING --
--------------

{-# INLINE deriveAsk #-}
deriveAsk ∷ ∀ m₁ m₂ r. (m₁ ⇄⁻ m₂,MonadReader r m₂) ⇒ m₁ r
deriveAsk = isofr2 ask

{-# INLINE deriveLocal #-}
deriveLocal ∷ ∀ m₁ m₂ r a. (m₁ ⇄⁻ m₂,MonadReader r m₂) ⇒ r → m₁ a → m₁ a
deriveLocal r = isofr2 ∘ local r ∘ isoto2

{-# INLINE deriveTell #-}
deriveTell ∷ ∀ m₁ m₂ o. (m₁ ⇄⁻ m₂,MonadWriter o m₂) ⇒ o → m₁ ()
deriveTell = isofr2 ∘ tell

{-# INLINE deriveHijack #-}
deriveHijack ∷ ∀ m₁ m₂ o a. (m₁ ⇄⁻ m₂,MonadWriter o m₂) ⇒ m₁ a → m₁ (o ∧ a)
deriveHijack = isofr2 ∘ hijack ∘ isoto2

{-# INLINE deriveGet #-}
deriveGet ∷ ∀ m₁ m₂ s. (m₁ ⇄⁻ m₂,MonadState s m₂) ⇒ m₁ s
deriveGet = isofr2 get

{-# INLINE derivePut #-}
derivePut ∷ ∀ m₁ m₂ s. (m₁ ⇄⁻ m₂,MonadState s m₂) ⇒ s → m₁ ()
derivePut = isofr2 ∘ put

{-# INLINE deriveAbort #-}
deriveAbort ∷ ∀ m₁ m₂ a. (m₁ ⇄⁻ m₂,MonadFail m₂) ⇒ m₁ a
deriveAbort = isofr2 abort

{-# INLINE deriveTry #-}
deriveTry ∷ ∀ m₁ m₂ a. (m₁ ⇄⁻ m₂,MonadFail m₂) ⇒ m₁ a → m₁ a → m₁ a
deriveTry xM₁ xM₂ = isofr2 $ isoto2 xM₁ ⎅ isoto2 xM₂

{-# INLINE deriveThrow #-}
deriveThrow ∷ ∀ m₁ m₂ e a. (m₁ ⇄⁻ m₂,MonadError e m₂) ⇒ e → m₁ a
deriveThrow e = isofr2 $ throw e

{-# INLINE deriveCatch #-}
deriveCatch ∷ ∀ m₁ m₂ e a. (m₁ ⇄⁻ m₂,MonadError e m₂) ⇒ m₁ a → (e → m₁ a) → m₁ a
deriveCatch xM k = isofr2 $ catch (isoto2 xM) (isoto2 ∘ k)

{-# INLINE deriveMzero #-}
deriveMzero ∷ ∀ m₁ m₂ a. (m₁ ⇄⁻ m₂,MonadNondet m₂) ⇒ m₁ a
deriveMzero = isofr2 mzero

{-# INLINE deriveMplus #-}
deriveMplus ∷ ∀ m₁ m₂ a. (m₁ ⇄⁻ m₂,MonadNondet m₂) ⇒ m₁ a → m₁ a → m₁ a
deriveMplus xM₁ xM₂ = isofr2 $ isoto2 xM₁ ⊞ isoto2 xM₂

{-# INLINE deriveMtop #-}
deriveMtop ∷ ∀ m₁ m₂ a. (m₁ ⇄⁻ m₂,MonadTop m₂) ⇒ m₁ a
deriveMtop = isofr2 mtop

{-# INLINE deriveCallCC #-}
deriveCallCC ∷ ∀ m₁ m₂ r a. (m₁ ⇄⁻ m₂,MonadCont r m₂) ⇒ ((a → m₁ r) → m₁ r) → m₁ a
deriveCallCC ff = isofr2 $ callCC $ \ k → isoto2 $ ff (isofr2 ∘ k)

{-# INLINE deriveWithC #-}
deriveWithC ∷ ∀ m₁ m₂ r a. (m₁ ⇄⁻ m₂,MonadCont r m₂) ⇒ (a → m₁ r) → m₁ a → m₁ r
deriveWithC k xM = isofr2 $ withC (isoto2 ∘ k) (isoto2 xM)

