module UVMHS.Core.Classes.Monad where

import UVMHS.Core.Init
import UVMHS.Core.Classes.Functor

import qualified Prelude as HS

infixr 0 *$
infixr 1 ≫=, ≫
infixl 6 ⧆
infixl 7 *∘

(>>=) ∷ (Bind m) ⇒ m a → (a → m b) → m b
(>>=) = (≫=)

(>>) ∷ (Bind m) ⇒ m a → m b → m b
xM >> ~yM = xM ≫= \ _ → let yM' = yM in yM'

class Return (m ∷ ★ → ★) where return ∷ a → m a
class Bind (m ∷ ★ → ★) where (≫=) ∷ m a → (a → m b) → m b
class (Functor m,Return m,Bind m) ⇒ Monad m
class Transformer (t ∷ (★ → ★) → (★ → ★)) where lift ∷ ∀ m a. (Monad m) ⇒ m a → t m a

(*⋅) ∷ (Bind m) ⇒ (a → m b) → (m a → m b)
(*⋅) = extend

(*$) ∷ (Bind m) ⇒ (a → m b) → (m a → m b)
(*$) = extend

(*∘) ∷ (Bind m) ⇒ (b → m c) → (a → m b) → (a → m c)
g *∘ f = extend g ∘ f

kreturn ∷ (Return m) ⇒ (a → b) → (a → m b)
kreturn f = return ∘ f

extend ∷ (Bind m) ⇒ (a → m b) → (m a → m b)
extend f xM = xM ≫= f

(≫) ∷ (Bind m) ⇒ m a → m b → m b
xM ≫ ~yM = xM ≫= \ _ → let yM' = yM in yM'

void ∷ (Functor m) ⇒ m a → m ()
void = map $ const ()

mjoin ∷ (Bind m) ⇒ m (m a) → m a
mjoin = extend id

mmap ∷ (Monad m) ⇒ (a → b) → m a → m b
mmap f xM = do {x ← xM;return $ f x}

(⧆) ∷ (Monad m) ⇒ m a → m b → m (a ∧ b)
xM ⧆ yM = do {x ← xM;y ← yM;return (x :* y)}

(⊡) ∷ (Monad m) ⇒ m (a → b) → m a → m b
fM ⊡ xM = do {f ← fM;x ← xM;return $ f x}

skip ∷ (Return m) ⇒ m ()
skip = return ()

when ∷ (Return m) ⇒ 𝔹 → m () → m ()
when b xM
  | b = xM
  | otherwise = skip

whenZ ∷ (Return m) ⇒ 𝔹 → m () → m ()
whenZ b ~xM
  | b = xM
  | otherwise = skip

whenM ∷ (Monad m) ⇒ m 𝔹 → m () → m ()
whenM bM xM = do b ← bM ; when b xM

whenMZ ∷ (Monad m) ⇒ m 𝔹 → m () → m ()
whenMZ bM ~xM = do b ← bM ; whenZ b xM

when𝑂 ∷ (Return m) ⇒ 𝑂 a → (a → m ()) → m ()
when𝑂 aO f = case aO of {None → skip;Some x → f x}

-- Compat --

newtype ToHSM (m ∷ ★ → ★) (a ∷ ★) = ToHSM { unToHSM ∷ m a }

instance (Functor t) ⇒ HS.Functor (ToHSM t) where
  fmap f = ToHSM ∘ map f ∘ unToHSM

instance (Monad m) ⇒ HS.Applicative (ToHSM m) where
  pure = ToHSM ∘ return
  fM <*> xM = ToHSM $ unToHSM fM ⊡ unToHSM xM

instance (Monad m) ⇒ HS.Monad (ToHSM m) where
  return = ToHSM ∘ return
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
