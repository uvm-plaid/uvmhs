module UVMHS.Core.Data.Lens where

import UVMHS.Core.Init
import UVMHS.Core.Classes

import UVMHS.Core.Data.Choice
import UVMHS.Core.Data.Iter
import UVMHS.Core.Data.Option
import UVMHS.Core.Data.Dict
import UVMHS.Core.Data.Pair
import UVMHS.Core.Data.Sequence
import UVMHS.Core.Data.Set

infixr 1 ⟢
infixr 1 ⌲

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

instance Reflexive (⟢) where
  refl = isoLens id id
instance Transitive (⟢) where
  Lens g ⊚ Lens f = Lens $ \ a →
    let (b :* ba) = f a
        (c :* cb) = g b
    in (c :* (ba ∘ cb))
instance Category (⟢)
instance Alter (⟢) where
  alter l f a = let (b :* ba) = runLens l a in ba $ f b
instance AlterM (⟢) where
  alterM l f a = let (b :* ba) = runLens l a in map ba $ f b

lens ∷ (a → b) → (a → b → a) → a ⟢ b
lens getter setter = Lens $ \ s → (getter s :* setter s)

isoLens ∷ (a → b) → (b → a) → a ⟢ b
isoLens to from = lens to $ const from

access ∷ a ⟢ b → a → b
access l = fst ∘ runLens l

-- ## Prism

instance Reflexive (⌲) where
  refl = isoPrism id id
instance Transitive (⌲) where
  g ⊚ f = Prism
    { view = view g *∘ view f
    , construct = construct f ∘ construct g
    }
instance Category (⌲)
instance Alter (⌲) where
  alter p f a = elim𝑂 (const a) (construct p ∘ f) $ view p a

prism ∷ (b → a) → (a → 𝑂 b) → a ⌲ b
prism = Prism

isoPrism ∷ (b → a) → (a → b) → a ⌲ b
isoPrism from to = prism from $ Some ∘ to

viewΩ ∷ a ⌲ b → a → b
viewΩ p = elim𝑂 (\ () → error "viewΩ") id ∘ view p

shape ∷ a ⌲ b → a → 𝔹
shape p = elim𝑂 (const False) (const True) ∘ view p

inlL ∷ a ∨ b ⌲ a
inlL = Prism Inl $ elimChoice Some $ const None

inrL ∷ a ∨ b ⌲ b
inrL = Prism Inr $ elimChoice (const None) Some

fstL ∷ a ∧ b ⟢ a
fstL = lens fst $ \ (_ :* b) → ( :* b)

sndL ∷ a ∧ b ⟢ b
sndL = lens snd $ \ (a :* _) → (a :* )

noneL ∷ 𝑂 a ⌲ ()
noneL = prism (const None) $ elim𝑂 (const $ Some ()) $ const None

someL ∷ 𝑂 a ⌲ a
someL = Prism Some id

singleL ∷ 𝐿 a ⌲ a
singleL = Prism single $ \case
  x :& Nil → Some x
  _ → None

consL ∷ 𝐿 a ⌲ (a ∧ 𝐿 a)
consL = Prism (curry (:&)) $ \case { x:&xs → Some (x:*xs) ; _ → None}

single𝑃L ∷ (Ord a) ⇒ 𝑃 a ⌲ a
single𝑃L = prism single𝑃 $ \ xs → case pminView𝑃 xs of
  Some (x :* xs') | isEmpty xs' → Some x
  _ → None

single𝑄L ∷ (Ord a) ⇒ 𝑄 a ⌲ a
single𝑄L = prism single𝑄 $ \ xs → case uncons𝑄 xs of
  Some (x :* xs') | isEmpty xs' → Some x
  _ → None

single𝐷L ∷ (Ord k) ⇒ (k ⇰ v) ⌲ (k ∧ v)
single𝐷L = prism (curry (↦♭)) $ \ kvs → case dminView𝐷 kvs of
  Some (kv :* kvs') | isEmpty kvs' → Some kv
  _ → None

keyL ∷ (Ord k) ⇒ k → (k ⇰ v) ⟢ 𝑂 v
keyL k = lens (⋕? k) $ flip $ \case
  None → drem𝐷 k
  Some v → ((k ↦♭ v) ⩌♭)

keyL𝑂 ∷ (Ord k,Null v) ⇒ k → (k ⇰ v) ⟢ v
keyL𝑂 k =
  let ℓ = lens (ifNone null) $ const Some
  in ℓ ⊚ keyL k

--------------------------
-- HasPrism and HasLens --
--------------------------

class HasPrism a b where hasPrism ∷ a ⌲ b
class HasLens a b where hasLens ∷ a ⟢ b

instance HasPrism a a where
  hasPrism = refl
instance HasLens a a where
  hasLens = refl

𝛊 ∷ (HasPrism a b) ⇒ b → a
𝛊 = construct hasPrism

𝛎 ∷ ∀ b a. (HasPrism a b) ⇒ a → 𝑂 b
𝛎 = view hasPrism

𝛑 ∷ (HasLens a b) ⇒ a → b
𝛑 = access hasLens

𝛏 ∷ (HasLens a b) ⇒ b → a → a
𝛏 y x = snd (runLens hasLens x) y
