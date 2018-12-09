module UVMHS.Core.Lens where

import UVMHS.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data

infixr 2 ⟢
infixr 2 ⌲

newtype a ⟢ b = Lens {runLens ∷ a → b ∧ (b → a)}
data a ⌲ b = Prism {construct ∷ b → a,view ∷ a → 𝑂 b}

-- # Cursors (common for Lens and Prism)

class Alter (t ∷ ★ → ★ → ★) where alter ∷ t a b → (b → b) → a → a
class AlterM (t ∷ ★ → ★ → ★) where alterM ∷ (Monad m) ⇒ t a b → (b → m b) → a → m a

update ∷ (Alter t) ⇒ t a b → b → a → a
update l x = alter l $ const x

updateM ∷ (AlterM t,Monad m) ⇒ t a b → m b → a → m a
updateM l xM = alterM l $ const xM

-- ## Lens

instance Category (⟢) where
  refl = isoLens id id
  Lens g ⊚ Lens f = Lens $ \ a →
    let (b :꘍ ba) = f a
        (c :꘍ cb) = g b
    in (c :꘍ (ba ∘ cb))
instance Alter (⟢) where
  alter l f a = let (b :꘍ ba) = runLens l a in ba $ f b
instance AlterM (⟢) where
  alterM l f a = let (b :꘍ ba) = runLens l a in map ba $ f b

lens ∷ (a → b) → (a → b → a) → a ⟢ b
lens getter setter = Lens $ \ s → (getter s :꘍ setter s)

isoLens ∷ (a → b) → (b → a) → a ⟢ b
isoLens to from = lens to $ const from

access ∷ a ⟢ b → a → b
access l = fst ∘ runLens l

-- ## Prism

instance Category (⌲) where
  refl = isoPrism id id
  g ⊚ f = Prism
    { view = view g *∘ view f
    , construct = construct f ∘ construct g
    }
instance Alter (⌲) where
  alter p f a = elim𝑂 a (construct p ∘ f) $ view p a

prism ∷ (b → a) → (a → 𝑂 b) → a ⌲ b
prism = Prism

isoPrism ∷ (b → a) → (a → b) → a ⌲ b
isoPrism from to = prism from $ Some ∘ to

unsafeView ∷ a ⌲ b → a → b
unsafeView p = elim𝑂 (error "unsafeView") id ∘ view p

shape ∷ a ⌲ b → a → 𝔹
shape p = elim𝑂 False (const True) ∘ view p

leftL ∷ a ∨ b ⌲ a
leftL = Prism Inl $ elimAlt Some $ const None

rightL ∷ a ∨ b ⌲ b
rightL = Prism Inr $ elimAlt (const None) Some

fstL ∷ a ∧ b ⟢ a
fstL = lens fst $ \ (_ :꘍ b) → ( :꘍ b)

sndL ∷ a ∧ b ⟢ b
sndL = lens snd $ \ (a :꘍ _) → (a :꘍ )

nothingL ∷ 𝑂 a ⌲ ()
nothingL = prism (const None) $ elim𝑂 (Some ()) $ const None

justL ∷ 𝑂 a ⌲ a
justL = Prism Some id

singleL ∷ 𝐿 a ⌲ a
singleL = Prism single $ \case
  x :& Nil → Some x
  _ → None

