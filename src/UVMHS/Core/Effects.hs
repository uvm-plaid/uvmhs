module UVMHS.Core.Effects where

import UVMHS.Core.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data

import qualified Prelude as HS

infixl 5 ⊞,⎅

class MonadIO (m ∷ ★ → ★) where io ∷ IO a → m a
class MonadQIO (m ∷ ★ → ★) where qio ∷ QIO a → m a

class LiftIO t where
  liftIO ∷ ∀ m. (Monad m) ⇒ (∀ a. IO a → m a) → (∀ a. IO a → t m a)

class LiftQIO t where
  liftQIO ∷ ∀ m. (Monad m) ⇒ (∀ a. QIO a → m a) → (∀ a. QIO a → t m a)

class MonadReader r m | m → r where
  askL ∷ r ⟢ r' → m r'
  localL ∷ ∀ a r'. r ⟢ r' → r' → m a → m a

class LiftReader t where
  liftAskL ∷ ∀ m r. (Monad m) ⇒ (∀ r'. r ⟢ r' → m r') → (∀ r'. r ⟢ r' → t m r')
  liftLocalL ∷ ∀ m r. (Monad m) ⇒ (∀ r' a. r ⟢ r' → r' → m a → m a) → (∀ r' a. r ⟢ r' → r' → t m a → t m a)

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

class MonadDelay m where
  delay ∷ (() → m a) → m a

class LiftDelay t where
  liftDelay ∷ ∀ m. (Monad m) ⇒ (∀ a. (() → m a) → m a) → (∀ a. (() → t m a) → t m a)

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
  liftCallCC ∷
    ∀ m r. (Monad m)
    ⇒ (∀ a. ((a → m r) → m r) → m a)
    → (∀ a. ((a → t m r) → t m r) → t m a)
  liftWithC ∷
    ∀ m r. (Monad m)
    ⇒ (∀ a. (a → m r) → m a → m r)
    → (∀ a. (a → t m r) → t m a → t m r)

class MonadUCont m where
  ucallCC ∷ ∀ a. (∀ u. (a → m u) → m u) → m a
  uwithC ∷ ∀ a u. (a → m u) → m a → m u

class LiftUCont t where
  liftUCallCC ∷
    ∀ m. (Monad m)
    ⇒ (∀ a. (∀ u. (a → m u) → m u) → m a)
    → (∀ a. (∀ u. (a → t m u) → t m u) → t m a)
  liftUWithC ∷
    ∀ m. (Monad m)
    ⇒ (∀ a u. (a → m u) → m a → m u)
    → (∀ a u. (a → t m u) → t m a → t m u)

class MonadBad m where
  bad ∷ ∀ a. m a

------------------------
-- STANDARD INSTANCES --
------------------------

instance MonadReader r ((→) r) where
  askL ∷ r ⟢ r' → r → r'
  askL = access

  localL ∷ ∀ a r'. r ⟢ r' → r' → (r → a) → (r → a)
  localL ℓ r' f = f ∘ update ℓ r'

instance (Null o) ⇒ MonadWriter o ((∧) o) where
  tell ∷ o → (o ∧ ())
  tell o = (o :* ())

  hijack ∷ ∀ a. o ∧ a → o ∧ (o ∧ a)
  hijack ox = null :* ox

instance MonadFail 𝑂 where
  abort ∷ ∀ a. 𝑂 a
  abort = None

  (⎅) ∷ ∀ a. 𝑂 a → 𝑂 a → 𝑂 a
  None ⎅ xM = xM
  Some x ⎅ _ = Some x

instance MonadError e ((∨) e) where
  throw ∷ ∀ a. e → e ∨ a
  throw = Inl

  catch ∷ ∀ a. e ∨ a → (e → e ∨ a) → e ∨ a
  catch (Inl e) k = k e
  catch (Inr x) _ = Inr x

instance MonadNondet 𝐼 where
  mzero ∷ ∀ a. 𝐼 a
  mzero = null

  (⊞) ∷ ∀ a. 𝐼 a → 𝐼 a → 𝐼 a
  (⊞) = (⧺)

instance MonadNondet 𝐿 where
  mzero ∷ ∀ a. 𝐿 a
  mzero = null

  (⊞) ∷ ∀ a. 𝐿 a → 𝐿 a → 𝐿 a
  (⊞) = (⧺)

instance MonadNondet 𝑄 where
  mzero ∷ ∀ a. 𝑄 a
  mzero = null

  (⊞) ∷ ∀ a. 𝑄 a → 𝑄 a → 𝑄 a
  (⊞) = (⧺)

----------------
-- OPERATIONS --
----------------

-- Reader

mapEnvL ∷ (Monad m,MonadReader r₁ m) ⇒ (r₁ ⟢ r₂) → (r₂ → r₂) → m a → m a
mapEnvL ℓ f xM = do
  r ← askL ℓ
  localL ℓ (f r) xM

ask ∷ (Monad m,MonadReader r m) ⇒ m r
ask = askL refl

local ∷ (Monad m,MonadReader r m) ⇒ r → m a → m a
local = localL refl

mapEnv ∷ (Monad m,MonadReader r m) ⇒ (r → r) → m a → m a
mapEnv = mapEnvL refl

-- Writer

tellL ∷ (Monoid o₁,Monad m,MonadWriter o₁ m) ⇒ o₁ ⟢ o₂ → o₂ → m ()
tellL l o = tell $ update l o null

hijackL ∷ (Monad m,MonadWriter o₁ m,Null o₂) ⇒ o₁ ⟢ o₂ → m a → m (o₂ ∧ a)
hijackL ℓ xM = do
  o₁ :* a ← hijack xM
  tell $ update ℓ null o₁
  return $ access ℓ o₁ :* a

mapOut ∷ (Monad m,MonadWriter o m) ⇒ (o → o) → m a → m a
mapOut f aM = do
  o :* a ← hijack aM
  tell $ f o
  return a

listen ∷ (Monad m,MonadWriter o m) ⇒ m a → m (o ∧ a)
listen xM = do
  o :* x ← hijack xM
  tell o
  return $ o :* x

listenL ∷ (Monad m,MonadWriter o₁ m) ⇒ (o₁ ⟢ o₂) → m a → m (o₂ ∧ a)
listenL ℓ xM = do
  o₁ :* x ← hijack xM
  tell o₁
  return $ access ℓ o₁ :* x

-- returns the output, and replaces the output with null
retOut ∷ ∀ o m a. (Monad m,MonadWriter o m) ⇒ m a → m o
retOut xM = do
  o :* _ ← hijack xM
  return o

retWithOut ∷ ∀ o m a. (Monad m,MonadWriter o m) ⇒ m a → m (o ∧ a)
retWithOut = hijack

-- # State

getL ∷ (Monad m,MonadState s m) ⇒ s ⟢ a → m a
getL l = map (access l) get

putL ∷ (Monad m,MonadState s m) ⇒ s ⟢ a → a → m ()
putL ℓ = modify ∘ update ℓ

modify ∷ (Monad m,MonadState s m) ⇒ (s → s) → m ()
modify f = do
  s ← get
  put $ f s

modifyM ∷ (Monad m,MonadState s m) ⇒ (s → m s) → m ()
modifyM f = do
  s ← get
  put *$ f s

modifyL ∷ (Monad m,MonadState s m) ⇒ s ⟢ a → (a → a) → m ()
modifyL ℓ = modify ∘ alter ℓ

modifyML ∷ (Monad m,MonadState s m) ⇒ s ⟢ a → (a → m a) → m ()
modifyML ℓ = modifyM ∘ alterM ℓ

getput ∷ (Monad m,MonadState s m) ⇒ s → m s
getput s = do
  s' ← get
  put s
  return s'

getputL ∷ (Monad m,MonadState s₁ m) ⇒ s₁ ⟢ s₂ → s₂ → m s₂
getputL ℓ x = do
  x' ← getL ℓ
  putL ℓ x
  return x'

next ∷ (Monad m,MonadState s m,Multiplicative s) ⇒ m s
next = do
  i ← get
  put $ succ i
  return i

nextL ∷ (Monad m,MonadState s m,Multiplicative a) ⇒ s ⟢ a → m a
nextL l = do
  i ← getL l
  putL l $ succ i
  return i

bump ∷ (Monad m,MonadState s m,Multiplicative s) ⇒ m ()
bump = modify succ

bumpL ∷ (Monad m,MonadState s m,Multiplicative a) ⇒ s ⟢ a → m ()
bumpL l = modifyL l succ

localizeState ∷ (Monad m,MonadState s m) ⇒ s → m a → m (s ∧ a)
localizeState s xM = do
  s' ← getput s
  x ← xM
  s'' ← getput s'
  return (s'' :* x)

localizeStateL ∷ (Monad m,MonadState s₁ m) ⇒ s₁ ⟢ s₂ → s₂ → m a → m (s₂ ∧ a)
localizeStateL ℓ s₂ aM = do
  s₂' ← getputL ℓ s₂
  x ← aM
  s₂'' ← getputL ℓ s₂'
  return (s₂'' :* x)

localize ∷ (Monad m,MonadState s m) ⇒ m a → m a
localize xM = do
  s ← get
  snd ^$ localizeState s xM

localizeL ∷ (Monad m,MonadState s₁ m) ⇒ s₁ ⟢ s₂ → m a → m a
localizeL ℓ xM = do
  s ← getL ℓ
  snd ^$ localizeStateL ℓ s xM

retState ∷ (Monad m,MonadState s m) ⇒ m a → m s
retState xM = do
  _ ← xM
  get

retStateOut ∷ (Monad m,MonadState s m,MonadWriter o m) ⇒ m a → m (s ∧ o)
retStateOut xM = do
  o :* _ ← hijack xM
  s ← get
  return $ s :* o

tellStateL ∷ (Monad m,MonadState o₁ m,Append o₂) ⇒ o₁ ⟢ o₂ → o₂ → m ()
tellStateL ℓ o = modifyL ℓ $ (⧺) o

hijackStateL ∷ (Monad m,MonadState o₁ m,Null o₂) ⇒ o₁ ⟢ o₂ → m a → m (o₂ ∧ a)
hijackStateL ℓ aM = localizeStateL ℓ null aM

localMapStateL ∷ (Monad m,MonadState s₁ m) ⇒ s₁ ⟢ s₂ → (s₂ → s₂) → m a → m a
localMapStateL ℓ f xM = do
  s ← getL ℓ
  snd ^$ localizeStateL ℓ (f s) xM

-- Fail

failEff ∷ (Monad m,MonadFail m) ⇒ 𝑂 a → m a
failEff = elim𝑂 (const abort) return

failObs ∷ (Monad m,MonadFail m) ⇒ m a → m (𝑂 a)
failObs xM = tries
  [ Some ^$ xM
  , return None
  ]

abort𝑂 ∷ (Monad m,MonadFail m) ⇒ 𝑂 a → m a
abort𝑂 = elim𝑂 (const abort) return

tries ∷ (Monad m,MonadFail m,ToIter (m a) t) ⇒ t → m a
tries = foldr abort (⎅)

guard ∷ (Monad m,MonadFail m) ⇒ 𝔹 → m ()
guard b = if b then skip else abort

optional ∷ (Monad m,MonadFail m) ⇒ m a → m (𝑂 a)
optional p = tries [map Some p,return None]

many ∷ (Monad m,MonadFail m) ⇒ m a → m (𝐿 a)
many aM = tries
  [ oneOrMore aM
  , return Nil
  ]

oneOrMore ∷ (Monad m,MonadFail m) ⇒ m a → m (𝐿 a)
oneOrMore xM = do
  x :* xs ← oneOrMoreSplit xM
  return $ x :& xs

oneOrMoreSplit ∷ (Monad m,MonadFail m) ⇒ m a → m (a ∧ 𝐿 a)
oneOrMoreSplit aM = do
  x ← aM
  xs ← many aM
  return $ x :* xs

manySepBy ∷ (Monad m,MonadFail m) ⇒ m () → m a → m (𝐿 a)
manySepBy sepM xM = tries
  [ oneOrMoreSepBy sepM xM
  , return Nil
  ]

oneOrMoreSepBy ∷ (Monad m,MonadFail m) ⇒ m () → m a → m (𝐿 a)
oneOrMoreSepBy sepM xM = do
  x :* xs ← oneOrMoreSepBySplit sepM xM
  return $ x :& xs

oneOrMoreSepBySplit ∷ (Monad m,MonadFail m) ⇒ m () → m a → m (a ∧ 𝐿 a)
oneOrMoreSepBySplit sepM xM = do
  x ← xM
  xs ← many $ do sepM ; xM
  return $ x :* xs
  

-- Error --

throwEff ∷ (Monad m,MonadError e m) ⇒ m (e ∨ a) → m a
throwEff = extend $ elimChoice throw return

throwObs ∷ (Monad m,MonadError e m) ⇒ m a → m (e ∨ a)
throwObs xM = catch (map Inr xM) $ return ∘ Inl

throw𝑂 ∷ (Monad m,MonadError e m) ⇒ e → 𝑂 a → m a
throw𝑂 e = elim𝑂 (const $ throw e) return

-- Nondet --

mzeroIfNot ∷ (Monad m,MonadNondet m) ⇒ 𝔹 → m ()
mzeroIfNot b = if b then skip else mzero

mconcat ∷ (MonadNondet m,ToIter (m a) t) ⇒ t → m a
mconcat = foldr mzero (⊞)

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

-- twoOrMoreSplitNT ∷ (Monad m,MonadNondet m) ⇒ m a → m (a ∧ a ∧ 𝐿 a)
-- twoOrMoreSplitNT aM = do
--   x₁ ← aM
--   (x₂ :* xs) ← oneOrMoreSplitNT aM
--   return (x₁ :* x₂ :* xs)

-- manySepBy ∷ (Monad m,MonadNondet m) ⇒ m () → m a → m (𝐿 a)
-- manySepBy uM xM = mconcat
--   [ do
--       x ← xM
--       xs ← manyPrefBy uM xM
--       return $ x :& xs
--   , return null
--   ]
-- 
-- manyPrefBy ∷ (Monad m,MonadNondet m) ⇒ m () → m a → m (𝐿 a)
-- manyPrefBy uM xM = mconcat
--   [ do
--       uM
--       x ← xM
--       xs ← manyPrefBy uM xM
--       return $ x :& xs
--   , return null
--   ]

mzero𝑂 ∷ (Monad m,MonadNondet m) ⇒ 𝑂 a → m a
mzero𝑂 = elim𝑂 (const mzero) return

return𝑃 ∷ ∀ m a. (Monad m,MonadNondet m) ⇒ 𝑃 a → m a
return𝑃 = fold mzero (\ x xM → xM ⊞ return x)

-- Cont --

reset ∷ (Monad m,MonadCont u m) ⇒ m u → m u
reset aM = callCC $ \ k → k *$ withC return aM

modifyC ∷ (Monad m,MonadCont u m) ⇒ (u → m u) → m ()
modifyC f = callCC $ \ k → f *$ k ()

withCOn ∷ (Monad m,MonadCont u m) ⇒ m a → (a → m u) → m u
withCOn = flip withC

putEnv ∷ (Monad m,MonadReader r m,MonadCont u m) ⇒ r → m ()
putEnv r = callCC $ \ 𝓀 → local r $ 𝓀 ()

putEnvL ∷ (Monad m,MonadReader r m,MonadCont u m) ⇒ r ⟢ r' → r' → m ()
putEnvL ℓ r = callCC $ \ 𝓀 → localL ℓ r $ 𝓀 ()

modifyEnv ∷ (Monad m,MonadReader r m,MonadCont u m) ⇒ (r → r) → m ()
modifyEnv f = do
  r ← ask
  putEnv $ f r

modifyEnvL ∷ (Monad m,MonadReader r m,MonadCont u m) ⇒ r ⟢ r' → (r' → r') → m ()
modifyEnvL ℓ f = do
  r ← askL ℓ
  putEnvL ℓ $ f r

-- UCont --

ureset ∷ (Monad m,MonadUCont m) ⇒ m a → m a
ureset aM = ucallCC HS.$ \ k → k *$ uwithC return aM

umodifyC ∷ (Monad m,MonadUCont m) ⇒ (∀ u. u → m u) → m ()
umodifyC f = ucallCC HS.$ \ k → f *$ k ()

uwithCOn ∷ (Monad m,MonadUCont m) ⇒ m a → (a → m u) → m u
uwithCOn = flip uwithC

uputEnv ∷ (Monad m,MonadReader r m,MonadUCont m) ⇒ r → m ()
uputEnv r = ucallCC HS.$ \ 𝓀 → local r $ 𝓀 ()

uputEnvL ∷ (Monad m,MonadReader r m,MonadUCont m) ⇒ r ⟢ r' → r' → m ()
uputEnvL ℓ r = ucallCC HS.$ \ 𝓀 → localL ℓ r $ 𝓀 ()

umodifyEnv ∷ (Monad m,MonadReader r m,MonadUCont m) ⇒ (r → r) → m ()
umodifyEnv f = do
  r ← ask
  uputEnv $ f r

umodifyEnvL ∷ (Monad m,MonadReader r m,MonadUCont m) ⇒ r ⟢ r' → (r' → r') → m ()
umodifyEnvL ℓ f = do
  r ← askL ℓ
  uputEnvL ℓ $ f r

unextEnvL ∷ (Monad m,MonadReader r m,MonadUCont m,Zero a,One a,Plus a) ⇒ r ⟢ a → m a
unextEnvL ℓ = do
  x ← askL ℓ
  uputEnvL ℓ $ succ x
  return x

--------------
-- DERIVING --
--------------

deriveAskL ∷ ∀ m₁ m₂ r r'. (m₁ ⇄⁻ m₂,MonadReader r m₂) ⇒ r ⟢ r' → m₁ r'
deriveAskL = isofr2 ∘ askL

deriveLocal ∷ ∀ m₁ m₂ r r' a. (m₁ ⇄⁻ m₂,MonadReader r m₂) ⇒ r ⟢ r' → r' → m₁ a → m₁ a
deriveLocal ℓ r = isofr2 ∘ localL ℓ r ∘ isoto2

deriveTell ∷ ∀ m₁ m₂ o. (m₁ ⇄⁻ m₂,MonadWriter o m₂) ⇒ o → m₁ ()
deriveTell = isofr2 ∘ tell

deriveHijack ∷ ∀ m₁ m₂ o a. (m₁ ⇄⁻ m₂,MonadWriter o m₂) ⇒ m₁ a → m₁ (o ∧ a)
deriveHijack = isofr2 ∘ hijack ∘ isoto2

deriveGet ∷ ∀ m₁ m₂ s. (m₁ ⇄⁻ m₂,MonadState s m₂) ⇒ m₁ s
deriveGet = isofr2 get

derivePut ∷ ∀ m₁ m₂ s. (m₁ ⇄⁻ m₂,MonadState s m₂) ⇒ s → m₁ ()
derivePut = isofr2 ∘ put

deriveAbort ∷ ∀ m₁ m₂ a. (m₁ ⇄⁻ m₂,MonadFail m₂) ⇒ m₁ a
deriveAbort = isofr2 abort

deriveTry ∷ ∀ m₁ m₂ a. (m₁ ⇄⁻ m₂,MonadFail m₂) ⇒ m₁ a → m₁ a → m₁ a
deriveTry xM₁ xM₂ = isofr2 $ isoto2 xM₁ ⎅ isoto2 xM₂

deriveThrow ∷ ∀ m₁ m₂ e a. (m₁ ⇄⁻ m₂,MonadError e m₂) ⇒ e → m₁ a
deriveThrow e = isofr2 $ throw e

deriveCatch ∷ ∀ m₁ m₂ e a. (m₁ ⇄⁻ m₂,MonadError e m₂) ⇒ m₁ a → (e → m₁ a) → m₁ a
deriveCatch xM k = isofr2 $ catch (isoto2 xM) (isoto2 ∘ k)

deriveMzero ∷ ∀ m₁ m₂ a. (m₁ ⇄⁻ m₂,MonadNondet m₂) ⇒ m₁ a
deriveMzero = isofr2 mzero

deriveMplus ∷ ∀ m₁ m₂ a. (m₁ ⇄⁻ m₂,MonadNondet m₂) ⇒ m₁ a → m₁ a → m₁ a
deriveMplus xM₁ xM₂ = isofr2 $ isoto2 xM₁ ⊞ isoto2 xM₂

deriveMtop ∷ ∀ m₁ m₂ a. (m₁ ⇄⁻ m₂,MonadTop m₂) ⇒ m₁ a
deriveMtop = isofr2 mtop

deriveCallCC ∷ ∀ m₁ m₂ r a. (m₁ ⇄⁻ m₂,MonadCont r m₂) ⇒ ((a → m₁ r) → m₁ r) → m₁ a
deriveCallCC ff = isofr2 $ callCC $ \ k → isoto2 $ ff (isofr2 ∘ k)

deriveWithC ∷ ∀ m₁ m₂ r a. (m₁ ⇄⁻ m₂,MonadCont r m₂) ⇒ (a → m₁ r) → m₁ a → m₁ r
deriveWithC k xM = isofr2 $ withC (isoto2 ∘ k) (isoto2 xM)
