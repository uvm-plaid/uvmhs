module UVMHS.Core.Classes.Functors where

import UVMHS.Core.Init
import UVMHS.Core.Classes.Morphism

import qualified Prelude as HS

infixr 0 ^$, ^^$
infixl 7 ^∘, ^^∘

infixr 0 *$
infixl 1 ≫=, ≫
infixl 6 ⧆
infixl 7 *∘

newtype NoCostID (a ∷ ★) = NoCostID a
instance Functor NoCostID where
  map ∷ ∀ a b. (a → b) → NoCostID a → NoCostID b
  map = coerce @((a → b) → a → b) id
instance Return NoCostID where
  return ∷ ∀ a. a → NoCostID a
  return = coerce @a
instance Bind NoCostID where
  (≫=) ∷ ∀ a b. NoCostID a → (a → NoCostID b) → NoCostID b
  (≫=) = coerce @(a → (a → b) → b) appto
instance Monad NoCostID

--------------
-- FunctorM --
--------------

class FunctorM (t ∷ ★ → ★) where
  mapM ∷ ∀ m a b. (Monad m) ⇒ (a → m b) → t a → m (t b)
  -- DEFAULTS --
  default mapM ∷ (OFunctorM t,Monad m) ⇒ (a → m b) → t a → m (t b)
  mapM f = omapM $ map Some ∘ f
class OFunctorM (t ∷ ★ → ★) where
  omapM ∷ ∀ m a b. (Monad m) ⇒ (a → m (𝑂 b)) → t a → m (t b)
class KFunctorM (k ∷ ★) (t ∷ ★ → ★) | t → k where
  kmapM ∷ ∀ m a b. (Monad m) ⇒ (k → a → m b) → t a → m (t b)
  kmapAtM ∷ ∀ m a. (Monad m) ⇒ k → (a → m a) → t a → m (t a)
  -- DEFAULTS --
  default kmapM ∷ (OKFunctorM k t,Monad m) ⇒ (k → a → m b) → t a → m (t b)
  kmapM f = okmapM $ map Some ∘∘ f
  default kmapAtM ∷ (OKFunctorM k t,Monad m) ⇒ k → (a → m a) → t a → m (t a)
  kmapAtM k f = okmapAtM k $ \case {None→return None;Some x → Some ^$ f x}
class OKFunctorM (k ∷ ★) (t ∷ ★ → ★) | t → k where
  okmapM ∷ ∀ m a b. (Monad m) ⇒ (k → a → m (𝑂 b)) → t a → m (t b)
  okmapAtM ∷ ∀ m a. (Monad m) ⇒ k → (𝑂 a → m (𝑂 a)) → t a → m (t a)

----------------
-- BiFunctorM --
----------------

class BiFunctorM (t ∷ ★ → ★) where
  bimapM ∷ ∀ m a b c. (Monad m) ⇒ (a → m c) → (b → m c) → (a → b → m c) → t a → t b → m (t c)
  -- DEFAULTS --
  default bimapM ∷ (OBiFunctorM t,Monad m) ⇒ (a → m c) → (b → m c) → (a → b → m c) → t a → t b → m (t c)
  bimapM f₁ f₂ f₃ = obimapM (map Some ∘ f₁) (map Some ∘ f₂) $ map Some ∘∘ f₃
class OBiFunctorM (t ∷ ★ → ★) where
  obimapM ∷ ∀ m a b c. (Monad m) ⇒ (a → m (𝑂 c)) → (b → m (𝑂 c)) → (a → b → m (𝑂 c)) → t a → t b → m (t c)
class KBiFunctorM (k ∷ ★) (t ∷ ★ → ★) | t → k where
  kbimapM ∷ ∀ m a b c. (Monad m) ⇒ (k → a → m c) → (k → b → m c) → (k → a → b → m c) → t a → t b → m (t c)
  -- DEFAULTS --
  default kbimapM ∷ (OKBiFunctorM k t,Monad m) ⇒ (k → a → m c) → (k → b → m c) → (k → a → b → m c) → t a → t b → m (t c)
  kbimapM f₁ f₂ f₃ = okbimapM (map Some ∘∘ f₁) (map Some ∘∘ f₂) $ map Some ∘∘∘ f₃
class OKBiFunctorM (k ∷ ★) (t ∷ ★ → ★) | t → k where
  okbimapM ∷ ∀ m a b c. (Monad m) ⇒ (k → a → m (𝑂 c)) → (k → b → m (𝑂 c)) → (k → a → b → m (𝑂 c)) → t a → t b → m (t c)

-------------
-- Functor --
-------------

class Functor (t ∷ ★ → ★) where
  map ∷ (a → b) → t a → t b
  -- DEFAULTS --
  default map ∷ ∀ a b. (FunctorM t) ⇒ (a → b) → t a → t b
  map f = coerce $ mapM @t @NoCostID @a @b $ coerce f
class OFunctor (t ∷ ★ → ★) where
  omap ∷ (a → 𝑂 b) → t a → t b
  -- DEFAULTS --
  default omap ∷ ∀ a b. (OFunctorM t) ⇒ (a → 𝑂 b) → t a → t b
  omap f = coerce $ omapM @t @NoCostID @a @b $ coerce f
class KFunctor (k ∷ ★) (t ∷ ★ → ★) | t → k where
  kmap ∷ (k → a → b) → t a → t b
  kmapAt ∷ k → (a → a) → t a → t a
  -- DEFAULTS --
  default kmap ∷ ∀ a b. (KFunctorM k t) ⇒ (k → a → b) → t a → t b
  kmap f = coerce $ kmapM @k @t @NoCostID @a @b $ coerce f
  default kmapAt ∷ ∀ a. (KFunctorM k t) ⇒ k → (a → a) → t a → t a
  kmapAt k f = coerce $ kmapAtM @k @t @NoCostID @a k $ coerce f
class OKFunctor (k ∷ ★) (t ∷ ★ → ★) | t → k where
  okmap ∷ (k → a → 𝑂 b) → t a → t b
  okmapAt ∷ k → (𝑂 a → 𝑂 a) → t a → t a
  -- DEFAULTS --
  default okmap ∷ ∀ a b. (OKFunctorM k t) ⇒ (k → a → 𝑂 b) → t a → t b
  okmap f = coerce $ okmapM @k @t @NoCostID @a @b $ coerce f
  default okmapAt ∷ ∀ a. (OKFunctorM k t) ⇒ k → (𝑂 a → 𝑂 a) → t a → t a
  okmapAt k f = coerce $ okmapAtM @k @t @NoCostID @a k $ coerce f

---------------
-- BiFunctor --
---------------

class BiFunctor (t ∷ ★ → ★) where
  bimap ∷ (a → c) → (b → c) → (a → b → c) → t a → t b → t c
  -- DEFAULTS --
  default bimap ∷ ∀ a b c. (BiFunctorM t) ⇒ (a → c) → (b → c) → (a → b → c) → t a → t b → t c
  bimap f₁ f₂ f₃ = coerce $ bimapM @t @NoCostID @a @b @c (coerce f₁) (coerce f₂) $ coerce f₃
class OBiFunctor (t ∷ ★ → ★) where
  obimap ∷ (a → 𝑂 c) → (b → 𝑂 c) → (a → b → 𝑂 c) → t a → t b → t c
  -- DEFAULTS --
  default obimap ∷ ∀ a b c. (OBiFunctorM t) ⇒ (a → 𝑂 c) → (b → 𝑂 c) → (a → b → 𝑂 c) → t a → t b → t c
  obimap f₁ f₂ f₃ = coerce $ obimapM @t @NoCostID @a @b @c (coerce f₁) (coerce f₂) $ coerce f₃
class KBiFunctor (k ∷ ★) (t ∷ ★ → ★) where
  kbimap ∷ (k → a → c) → (k → b → c) → (k → a → b → c) → t a → t b → t c
  -- DEFAULTS --
  default kbimap ∷ ∀ a b c. (KBiFunctorM k t) ⇒ (k → a → c) → (k → b → c) → (k → a → b → c) → t a → t b → t c
  kbimap f₁ f₂ f₃ = coerce $ kbimapM @k @t @NoCostID @a @b @c (coerce f₁) (coerce f₂) $ coerce f₃
class OKBiFunctor (k ∷ ★) (t ∷ ★ → ★) where
  okbimap ∷ (k → a → 𝑂 c) → (k → b → 𝑂 c) → (k → a → b → 𝑂 c) → t a → t b → t c
  -- DEFAULTS --
  default okbimap ∷ ∀ a b c. (OKBiFunctorM k t) ⇒ (k → a → 𝑂 c) → (k → b → 𝑂 c) → (k → a → b → 𝑂 c) → t a → t b → t c
  okbimap f₁ f₂ f₃ = coerce $ okbimapM @k @t @NoCostID @a @b @c (coerce f₁) (coerce f₂) $ coerce f₃

-------------
-- DERIVED --
-------------

mapMOn ∷ (FunctorM t,Monad m) ⇒ t a → (a → m b) → m (t b)
mapMOn = flip mapM

exchange ∷ (FunctorM t,Monad m) ⇒ t (m a) → m (t a)
exchange = mapM id

omapMOn ∷ (OFunctorM t,Monad m) ⇒ t a → (a → m (𝑂 b)) → m (t b)
omapMOn = flip omapM

kmapMOn ∷ (KFunctorM k t,Monad m) ⇒ t a → (k → a → m b) → m (t b)
kmapMOn = flip kmapM

kmapAtMOn ∷ (KFunctorM k t,Monad m) ⇒ k → t a → (a → m a) → m (t a)
kmapAtMOn = flip ∘ kmapAtM

okmapMOn ∷ (OKFunctorM k t,Monad m) ⇒ t a → (k → a → m (𝑂 b)) → m (t b)
okmapMOn = flip okmapM

okmapAtMOn ∷ (OKFunctorM k t,Monad m) ⇒ k → t a → (𝑂 a → m (𝑂 a)) → m (t a)
okmapAtMOn = flip ∘ okmapAtM

bimapMOn ∷ (BiFunctorM t,Monad m) ⇒ t a → t b → (a → m c) → (b → m c) → (a → b → m c) → m (t c)
bimapMOn = \ xM yM f₁ f₂ f₃ → bimapM f₁ f₂ f₃ xM yM

obimapMOn ∷ (OBiFunctorM t,Monad m) ⇒ t a → t b → (a → m (𝑂 c)) → (b → m (𝑂 c)) → (a → b → m (𝑂 c)) → m (t c)
obimapMOn = \ xM yM f₁ f₂ f₃ → obimapM f₁ f₂ f₃ xM yM

kbimapMOn ∷ (KBiFunctorM k t,Monad m) ⇒ t a → t b → (k → a → m c) → (k → b → m c) → (k → a → b → m c) → m (t c)
kbimapMOn = \ xM yM f₁ f₂ f₃ → kbimapM f₁ f₂ f₃ xM yM

okbimapMOn ∷ (OKBiFunctorM k t,Monad m) ⇒ t a → t b → (k → a → m (𝑂 c)) → (k → b → m (𝑂 c)) → (k → a → b → m (𝑂 c)) → m (t c)
okbimapMOn = \ xM yM f₁ f₂ f₃ → okbimapM f₁ f₂ f₃ xM yM

mapOn ∷ (Functor t) ⇒ t a → (a → b) → t b
mapOn = flip map

mapp ∷ (Functor t,Functor u) ⇒ (a → b) → t (u a) → t (u b)
mapp = map ∘ map

mappOn ∷ (Functor t,Functor u) ⇒ t (u a) → (a → b) → t (u b)
mappOn = flip mapp

mappp ∷ (Functor t,Functor u,Functor v) ⇒ (a → b) → t (u (v a)) → t (u (v b))
mappp = mapp ∘ map

mapppOn ∷ (Functor t,Functor u,Functor v) ⇒ t (u (v a)) → (a → b) → t (u (v b))
mapppOn = flip mappp

(^$) ∷ (Functor t) ⇒ (a → b) → t a → t b
(^$) = map

(^^$) ∷ (Functor t,Functor u) ⇒ (a → b) → t (u a) → t (u b)
(^^$) = mapp

(^∘) ∷ (Functor t) ⇒ (b → c) → (a → t b) → a → t c
(^∘) = \ g f → map g ∘ f

(^^∘) ∷ (Functor t,Functor u) ⇒ (b → c) → (a → t (u b)) → a → t (u c)
(^^∘) = \ g f → mapp g ∘ f

omapOn ∷ (OFunctor t) ⇒ t a → (a → 𝑂 b) → t b
omapOn = flip omap

kmapOn ∷ (KFunctor k t) ⇒ t a → (k → a → b) → t b
kmapOn = flip kmap

kmapAtOn ∷ (KFunctor k t) ⇒ k → t a → (a → a) → t a
kmapAtOn = flip ∘ kmapAt

okmapOn ∷ (OKFunctor k t) ⇒ t a → (k → a → 𝑂 b) → t b
okmapOn = flip okmap

okmapAtOn ∷ (OKFunctor k t) ⇒ k → t a → (𝑂 a → 𝑂 a) → t a
okmapAtOn = flip ∘ okmapAt

bimapOn ∷ (BiFunctor t) ⇒ t a → t b → (a → c) → (b → c) → (a → b → c) → t c
bimapOn = \ x y f₁ f₂ f₃ → bimap f₁ f₂ f₃ x y

obimapOn ∷ (OBiFunctor t) ⇒ t a → t b → (a → 𝑂 c) → (b → 𝑂 c) → (a → b → 𝑂 c) → t c
obimapOn = \ x y f₁ f₂ f₃ → obimap f₁ f₂ f₃ x y

kbimapOn ∷ (KBiFunctor k t) ⇒ t a → t b → (k → a → c) → (k → b → c) → (k → a → b → c) → t c
kbimapOn = \ x y f₁ f₂ f₃ → kbimap f₁ f₂ f₃ x y

okbimapOn ∷ (OKBiFunctor k t) ⇒ t a → t b → (k → a → 𝑂 c) → (k → b → 𝑂 c) → (k → a → b → 𝑂 c) → t c
okbimapOn = \ x y f₁ f₂ f₃ → okbimap f₁ f₂ f₃ x y

class Functor2 (w ∷ (★ → ★) → (★ → ★)) where map2 ∷ (t →⁻ u) → w t →⁻ w u
class Functor2Iso (w ∷ (★ → ★) → (★ → ★)) where map2iso ∷ Iso2 t u → w t →⁻ w u

-----------
-- Monad --
-----------

(>>=) ∷ (Bind m) ⇒ m a → (a → m b) → m b
(>>=) = (≫=)

(>>) ∷ (Bind m) ⇒ m a → m b → m b
(>>) = \ xM ~yM → xM ≫= \ _ → id yM

class Return (m ∷ ★ → ★) where return ∷ a → m a
class Bind (m ∷ ★ → ★) where (≫=) ∷ m a → (a → m b) → m b
class (Functor m,Return m,Bind m) ⇒ Monad m
class Transformer (t ∷ (★ → ★) → (★ → ★)) where lift ∷ ∀ m a. (Monad m) ⇒ m a → t m a

(*⋅) ∷ (Bind m) ⇒ (a → m b) → (m a → m b)
(*⋅) = extend

(*$) ∷ (Bind m) ⇒ (a → m b) → (m a → m b)
(*$) = extend

(*∘) ∷ (Bind m) ⇒ (b → m c) → (a → m b) → (a → m c)
(*∘) = \ g f → extend g ∘ f

kreturn ∷ (Return m) ⇒ (a → b) → (a → m b)
kreturn = \ f → return ∘ f

extend ∷ (Bind m) ⇒ (a → m b) → (m a → m b)
extend = \ f xM → xM ≫= f

(≫) ∷ (Bind m) ⇒ m a → m b → m b
(≫) = \ xM ~yM → xM ≫= \ _ → id yM

void ∷ (Functor m) ⇒ m a → m ()
void = map $ const ()

mjoin ∷ (Bind m) ⇒ m (m a) → m a
mjoin = extend id

mmap ∷ (Monad m) ⇒ (a → b) → m a → m b
mmap = \ f xM → do x ← xM ; return $ f x

(⧆) ∷ (Monad m) ⇒ m a → m b → m (a ∧ b)
(⧆) = \ xM yM → do x ← xM ; y ← yM ; return $ x :* y

(⊡) ∷ (Monad m) ⇒ m (a → b) → m a → m b
(⊡) = \ fM xM → do f ← fM ; x ← xM ; return $ f x

skip ∷ (Return m) ⇒ m ()
skip = return ()

when ∷ (Return m) ⇒ 𝔹 → (() → m ()) → m ()
when = \ b f → if b then f () else skip

whenM ∷ (Monad m) ⇒ m 𝔹 → (() → m ()) → m ()
whenM = \ bM xM → do b ← bM ; when b xM

-- Compat --

newtype ToHSM (m ∷ ★ → ★) (a ∷ ★) = ToHSM { unToHSM ∷ m a }

instance (Functor t) ⇒ HS.Functor (ToHSM t) where
  fmap f = ToHSM ∘ map f ∘ unToHSM

instance (Monad m) ⇒ HS.Applicative (ToHSM m) where
  pure = ToHSM ∘ return
  fM <*> xM = ToHSM $ unToHSM fM ⊡ unToHSM xM

instance (Monad m) ⇒ HS.Monad (ToHSM m) where
  xM >>= f = ToHSM $ do
    x ← unToHSM xM
    unToHSM $ f x

tohsMonad ∷ ∀ m. (Monad m) ⇒ W (HS.Monad m)
tohsMonad = coerce_UNSAFE (W ∷ W (HS.Monad (ToHSM m)))

newtype FrHSM (m ∷ ★ → ★) (a ∷ ★) = FrHSM { unFrHSM ∷ m a }

instance (HS.Functor t) ⇒ Functor (FrHSM t) where
  map f = FrHSM ∘ HS.fmap f ∘ unFrHSM

instance (HS.Monad m) ⇒ Return (FrHSM m) where
  return = FrHSM ∘ HS.return
instance (HS.Monad m) ⇒ Bind (FrHSM m) where
  xM ≫= f = FrHSM $ unFrHSM xM HS.>>= \ x → unFrHSM $ f x
instance (HS.Monad m) ⇒ Monad (FrHSM m)

frhsMonad ∷ ∀ m. (HS.Monad m) ⇒ W (Monad m)
frhsMonad = coerce_UNSAFE (W ∷ W (Monad (FrHSM m)))
