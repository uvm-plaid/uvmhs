module UVMHS.Core.Lens where

import UVMHS.Core.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data

infixr 2 ⟢
infixr 2 ⌲

newtype a ⟢ b = Lens {runLens ∷ a → b ∧ (b → a)}
data a ⌲ b = Prism {construct ∷ b → a,view ∷ a → 𝑂 b}

-- # Cursors (common for Lens and Prism)

class Alter (t ∷ ★ → ★ → ★) where alter ∷ t a b → (b → b) → a → a
class AlterM (t ∷ ★ → ★ → ★) where alterM ∷ (Monad m) ⇒ t a b → (b → m b) → a → m a

{-# INLINE update #-}
update ∷ (Alter t) ⇒ t a b → b → a → a
update l x = alter l $ const x

{-# INLINE updateM #-}
updateM ∷ (AlterM t,Monad m) ⇒ t a b → m b → a → m a
updateM l xM = alterM l $ const xM

-- ## Lens

instance Category (⟢) where
  {-# INLINE refl #-}
  refl = isoLens id id
  {-# INLINE (⊚) #-}
  Lens g ⊚ Lens f = Lens $ \ a →
    let (b :* ba) = f a
        (c :* cb) = g b
    in (c :* (ba ∘ cb))
instance Alter (⟢) where
  {-# INLINE alter #-}
  alter l f a = let (b :* ba) = runLens l a in ba $ f b
instance AlterM (⟢) where
  {-# INLINE alterM #-}
  alterM l f a = let (b :* ba) = runLens l a in map ba $ f b

{-# INLINE lens #-}
lens ∷ (a → b) → (a → b → a) → a ⟢ b
lens getter setter = Lens $ \ s → (getter s :* setter s)

{-# INLINE isoLens #-}
isoLens ∷ (a → b) → (b → a) → a ⟢ b
isoLens to from = lens to $ const from

{-# INLINE access #-}
access ∷ a ⟢ b → a → b
access l = fst ∘ runLens l

-- ## Prism

instance Category (⌲) where
  {-# INLINE refl #-}
  refl = isoPrism id id
  {-# INLINE (⊚) #-}
  g ⊚ f = Prism
    { view = view g *∘ view f
    , construct = construct f ∘ construct g
    }
instance Alter (⌲) where
  {-# INLINE alter #-}
  alter p f a = elim𝑂 a (construct p ∘ f) $ view p a

{-# INLINE prism #-}
prism ∷ (b → a) → (a → 𝑂 b) → a ⌲ b
prism = Prism

{-# INLINE isoPrism #-}
isoPrism ∷ (b → a) → (a → b) → a ⌲ b
isoPrism from to = prism from $ Some ∘ to

unsafeView ∷ a ⌲ b → a → b
unsafeView p = elim𝑂 (error "unsafeView") id ∘ view p

{-# INLINE shape #-}
shape ∷ a ⌲ b → a → 𝔹
shape p = elim𝑂 False (const True) ∘ view p

{-# INLINE leftL #-}
leftL ∷ a ∨ b ⌲ a
leftL = Prism Inl $ elimChoice Some $ const None

{-# INLINE rightL #-}
rightL ∷ a ∨ b ⌲ b
rightL = Prism Inr $ elimChoice (const None) Some

{-# INLINE fstL #-}
fstL ∷ a ∧ b ⟢ a
fstL = lens fst $ \ (_ :* b) → ( :* b)

{-# INLINE sndL #-}
sndL ∷ a ∧ b ⟢ b
sndL = lens snd $ \ (a :* _) → (a :* )

nothingL ∷ 𝑂 a ⌲ ()
nothingL = prism (const None) $ elim𝑂 (Some ()) $ const None

justL ∷ 𝑂 a ⌲ a
justL = Prism Some id

singleL ∷ 𝐿 a ⌲ a
singleL = Prism single $ \case
  x :& Nil → Some x
  _ → None

unconsL ∷ 𝐿 a ⌲ (a ∧ 𝐿 a)
unconsL = Prism (curry (:&)) $ \case { x:&xs → Some (x:*xs) ; _ → None}

--------------------------
-- HasPrism and HasLens --
--------------------------

class HasPrism a b where hasPrism ∷ a ⌲ b
class HasLens a b where hasLens ∷ a ⟢ b

instance HasPrism a a where 
  {-# INLINE hasPrism #-}
  hasPrism = refl
instance HasLens a a where 
  {-# INLINE hasLens #-}
  hasLens = refl

{-# INLINE ι #-}
ι ∷ (HasPrism a b) ⇒ b → a
ι = construct hasPrism

{-# INLINE ιview #-}
ιview ∷ ∀ b a. (HasPrism a b) ⇒ a → 𝑂 b
ιview = view hasPrism

{-# INLINE π #-}
π ∷ (HasLens a b) ⇒ a → b
π = access hasLens
