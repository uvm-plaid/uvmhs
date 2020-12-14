module UVMHS.Core.Data.Option where

import UVMHS.Core.Init
import UVMHS.Core.Classes

instance Functor 𝑂 where 
  {-# INLINE map #-}
  map = mmap
instance Return 𝑂 where 
  {-# INLINE return #-}
  return = Some
instance Bind 𝑂 where 
  {-# INLINE (≫=) #-}
  xO ≫= k = case xO of {None → None;Some x → k x}
instance Monad 𝑂
instance FunctorM 𝑂 where 
  {-# INLINE mapM #-}
  mapM f = \case
    None → return None
    Some x → Some ^$ f x
instance (Null a) ⇒ Null (𝑂 a) where 
  {-# INLINE null #-}
  null = Some null
instance (Append a) ⇒ Append (𝑂 a) where
  {-# INLINE (⧺) #-}
  None ⧺ _ = None
  _ ⧺ None = None
  Some x ⧺ Some y = Some $ x ⧺ y
instance (Monoid a) ⇒ Monoid (𝑂 a)

instance ToStream a (𝑂 a) where 
  {-# INLINE stream #-}
  stream xM = 𝑆 xM $ map (:*None)
instance ToIter a (𝑂 a) where 
  {-# INLINE iter #-}
  iter = iter𝑆 ∘ stream

instance 𝑂 a ⇄ (() ∨ a) where
  {-# INLINE isoto #-}
  isoto = \case
    None → Inl ()
    Some x → Inr x
  {-# INLINE isofr #-}
  isofr = \case
    Inl () → None
    Inr x → Some x

{-# INLINE elim𝑂 #-}
elim𝑂 ∷ b → (a → b) → 𝑂 a → b
elim𝑂 n s = \case
  None → n
  Some x → s x

{-# INLINE isNone #-}
isNone ∷ 𝑂 a → 𝔹
isNone = \case
  None → True
  Some _ → False

{-# INLINE isSome #-}
isSome ∷ 𝑂 a → 𝔹
isSome = \case
  None → False
  Some _ → True

{-# INLINE ifNone #-}
ifNone ∷ a → 𝑂 a → a
ifNone i = \case
  None → i
  Some x → x

{-# INLINE ifNoneM #-}
ifNoneM ∷ (Return m) ⇒ m a → 𝑂 a → m a
ifNoneM i = \case
  None → i
  Some x → return $ x

{-# INLINE first #-}
first ∷ 𝑂 a → 𝑂 a → 𝑂 a
first = \case
  None → id
  Some x → const $ Some x

{-# INLINE last #-}
last ∷ 𝑂 a → 𝑂 a → 𝑂 a
last = flip first
