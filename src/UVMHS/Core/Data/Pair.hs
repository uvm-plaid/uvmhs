module UVMHS.Core.Data.Pair where

import UVMHS.Core.Init
import UVMHS.Core.Classes

import UVMHS.Core.Data.Arithmetic ()

instance (POrd a,POrd b) ⇒ POrd (a ∧ b) where 
  {-# INLINE (⊑) #-}
  (x₁ :* y₁) ⊑ (x₂ :* y₂) = (x₁ ⊑ x₂) ⩓ (y₁ ⊑ y₂)
instance (Bot a,Bot b) ⇒ Bot (a ∧ b) where 
  {-# INLINE bot #-}
  bot = bot :* bot
instance (Join a,Join b) ⇒ Join (a ∧ b) where 
  {-# INLINE (⊔) #-}
  (a₁ :* b₁) ⊔ (a₂ :* b₂) = (a₁ ⊔ a₂) :* (b₁ ⊔ b₂)
instance (Top a,Top b) ⇒ Top (a ∧ b) where 
  {-# INLINE top #-}
  top = top :* top
instance (Meet a,Meet b) ⇒ Meet (a ∧ b) where 
  {-# INLINE (⊓) #-}
  (a₁ :* b₁) ⊓ (a₂ :* b₂) = (a₁ ⊓ a₂) :* (b₁ ⊓ b₂)
instance (Dual a,Dual b) ⇒ Dual (a ∧ b) where 
  {-# INLINE dual #-}
  dual (a :* b) = dual a :* dual b
instance (Difference a,Difference b) ⇒ Difference (a ∧ b) where 
  {-# INLINE (⊟) #-}
  (a₁ :* b₁) ⊟ (a₂ :* b₂) = (a₁ ⊟ a₂) :* (b₁ ⊟ b₂)
instance (JoinLattice a,JoinLattice b) ⇒ JoinLattice (a ∧ b)
instance (MeetLattice a,MeetLattice b) ⇒ MeetLattice (a ∧ b)
instance (Lattice a,Lattice b) ⇒ Lattice (a ∧ b)

instance (Null a,Null b) ⇒ Null (a ∧ b) where 
  {-# INLINE null #-}
  null = (null :* null)
instance (Append a,Append b) ⇒ Append (a ∧ b) where 
  {-# INLINE (⧺) #-}
  (x₁ :* y₁) ⧺ (x₂ :* y₂) = (x₁ ⧺ x₂) :* (y₁ ⧺ y₂)
instance (Monoid a,Monoid b) ⇒ Monoid (a ∧ b)

instance Functor ((∧) a) where 
  {-# INLINE map #-}
  map f (x :* y) = x :* f y
instance (Null a) ⇒ Return ((∧) a) where 
  {-# INLINE return #-}
  return = (:*) null
instance (Append a) ⇒ Bind ((∧) a) where 
  {-# INLINE (≫=) #-}
  (a :* b) ≫= f = let (a' :* c) = f b in (a ⧺ a') :* c
instance (Monoid a) ⇒ Monad ((∧) a)

instance FunctorM ((∧) a) where 
  {-# INLINE mapM #-}
  mapM f (x :* y) = map ((:*) x) $ f y

instance (Sized a,Sized b) ⇒ Sized (a ∧ b) where size (x :* y)  = size x + size y

{-# INLINE fst #-}
fst ∷ a ∧ b → a
fst (x :* _) = x

{-# INLINE snd #-}
snd ∷ a ∧ b → b
snd (_ :* y) = y

{-# INLINE mapPair #-}
mapPair ∷ (a₁ → a₂) → (b₁ → b₂) → a₁ ∧ b₁ → a₂ ∧ b₂
mapPair f g (x :* y) = f x :* g y

{-# INLINE mapFst #-}
mapFst ∷ (a₁ → a₂) → a₁ ∧ b → a₂ ∧ b
mapFst f = mapPair f id

{-# INLINE mapSnd #-}
mapSnd ∷ (b₁ → b₂) → a ∧ b₁ → a ∧ b₂
mapSnd f = mapPair id f

mapMPair ∷ (Monad m) ⇒ (a → m a') → (b → m b') → a ∧ b → m (a' ∧ b')
mapMPair f g (x :* y) = do
  x' ← f x
  y' ← g y
  return $ x' :* y'

mapMFst ∷ (Monad m) ⇒ (a → m a') → a ∧ b → m (a' ∧ b)
mapMFst = flip mapMPair return

mapMSnd ∷ (Monad m) ⇒ (b → m b') → a ∧ b → m (a ∧ b')
mapMSnd = mapMPair return

