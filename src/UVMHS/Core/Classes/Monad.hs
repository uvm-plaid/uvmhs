module UVMHS.Core.Classes.Monad where

import UVMHS.Core.Init
import UVMHS.Core.Classes.Functor

infixr 0 *$
infixr 1 ≫=, ≫
infixl 6 ⧆
infixl 7 *∘

{-# INLINE (>>=) #-}
(>>=) ∷ (Bind m) ⇒ m a → (a → m b) → m b
(>>=) = (≫=)

{-# INLINE (>>) #-}
(>>) ∷ (Bind m) ⇒ m a → m b → m b
xM >> ~yM = xM ≫= \ _ → let yM' = yM in yM'

class Return (m ∷ ★ → ★) where return ∷ a → m a
class Bind (m ∷ ★ → ★) where (≫=) ∷ m a → (a → m b) → m b
class (Functor m,Return m,Bind m) ⇒ Monad m
class Transformer (t ∷ (★ → ★) → (★ → ★)) where lift ∷ ∀ m a. (Monad m) ⇒ m a → t m a

{-# INLINE (*⋅) #-}
(*⋅) ∷ (Bind m) ⇒ (a → m b) → (m a → m b)
(*⋅) = extend

{-# INLINE (*$) #-}
(*$) ∷ (Bind m) ⇒ (a → m b) → (m a → m b)
(*$) = extend

{-# INLINE (*∘) #-}
(*∘) ∷ (Bind m) ⇒ (b → m c) → (a → m b) → (a → m c)
g *∘ f = extend g ∘ f

{-# INLINE kreturn #-}
kreturn ∷ (Return m) ⇒ (a → b) → (a → m b)
kreturn f = return ∘ f

{-# INLINE extend #-}
extend ∷ (Bind m) ⇒ (a → m b) → (m a → m b)
extend f xM = xM ≫= f

{-# INLINE (≫) #-}
(≫) ∷ (Bind m) ⇒ m a → m b → m b
xM ≫ ~yM = xM ≫= \ _ → yM

{-# INLINE void #-}
void ∷ (Functor m) ⇒ m a → m ()
void = map $ const ()

{-# INLINE mjoin #-}
mjoin ∷ (Bind m) ⇒ m (m a) → m a
mjoin = extend id

{-# INLINE mmap #-}
mmap ∷ (Monad m) ⇒ (a → b) → m a → m b
mmap f xM = do {x ← xM;return $ f x}

{-# INLINE (⧆) #-}
(⧆) ∷ (Monad m) ⇒ m a → m b → m (a ∧ b)
xM ⧆ yM = do {x ← xM;y ← yM;return (x :* y)}

{-# INLINE (⊡) #-}
(⊡) ∷ (Monad m) ⇒ m (a → b) → m a → m b
fM ⊡ xM = do {f ← fM;x ← xM;return $ f x}

{-# INLINE skip #-}
skip ∷ (Return m) ⇒ m ()
skip = return ()

when ∷ (Return m) ⇒ 𝔹 → m () → m ()
when b ~xM
  | b = xM
  | otherwise = skip

whenM ∷ (Monad m) ⇒ m 𝔹 → m () → m ()
whenM bM ~xM = do b ← bM ; when b xM

when𝑂 ∷ (Return m) ⇒ 𝑂 a → (a → m ()) → m ()
when𝑂 aO f = case aO of {None → skip;Some x → f x}
