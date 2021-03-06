module UVMHS.Lib.Variables where

import UVMHS.Core
import UVMHS.Lib.Pretty

---------------
-- VARIABLES --
---------------

data 𝕏 = 𝕏
  { 𝕩name ∷ 𝕊
  , 𝕩mark ∷ 𝑂 ℕ64
  } deriving (Eq,Ord,Show)
makeLenses ''𝕏

var ∷ 𝕊 → 𝕏
var x = 𝕏 x None

instance Pretty 𝕏 where
  pretty (𝕏 x nO) = concat
    [ ppString x
    , elim𝑂 null (\ n → concat [ppPun "#",ppPun $ show𝕊 n]) nO
    ]

-----------------------------------------
-- LOCALLY NAMELESS WITH SHIFTED NAMES --
-----------------------------------------

data 𝕐 =
    NamedVar 𝕏 ℕ64
  | BoundVar ℕ64
  deriving (Eq,Ord,Show)
makePrisms ''𝕐

named ∷ 𝕏 → 𝕐
named x = NamedVar x zero

instance Pretty 𝕐 where
  pretty = \case
    NamedVar x n → concat
      [ pretty x
      , if n ≡ zero then null else concat [ppPun "@",ppPun $ show𝕊 n]
      ]
    BoundVar n → concat [ppPun "!",ppString $ show𝕊 n]

openVar ∷ ℕ64 → 𝕏 → 𝕐 → 𝕐
openVar u x = \case
  NamedVar y n 
    | x ≡ y → NamedVar y $ succ n
    | otherwise → NamedVar y n
  BoundVar n
    | n < u → BoundVar n
    | n ≡ u → NamedVar x zero
    | otherwise → BoundVar $ pred n

closeVar ∷ ℕ64 → 𝕏 → 𝕐 → 𝕐
closeVar u x = \case
  NamedVar y n
    | x ≡ y,n ≡ zero → BoundVar zero
    | x ≡ y,n ≢ zero → NamedVar y $ pred n
    | otherwise      → NamedVar y n
  BoundVar n 
    | n < u → BoundVar n
    | otherwise → BoundVar $ n + one

bindVar ∷ (𝕐 → a) → ℕ64 → a → 𝕐 → a
bindVar mkvar' u e = \case
  NamedVar x n → mkvar' $ NamedVar x n
  BoundVar n
    | n < u → mkvar' $ BoundVar n
    | n ≡ u → e
    | otherwise → mkvar' $ BoundVar $ pred n

substVar ∷ (𝕐 → a) → 𝕏 → a → 𝕐 → a
substVar mkvar' x e = \case
  NamedVar y n
    | x ≡ y,n ≡ zero → e
    | x ≡ y,n ≢ zero → mkvar' $ NamedVar y $ pred n
    | otherwise → mkvar' $ NamedVar y n
  BoundVar n → mkvar' $ BoundVar n

introVar ∷ ℕ64 → ℕ64 → 𝕐 → 𝕐
introVar u n = \case
  NamedVar x n' → NamedVar x n'
  BoundVar n' 
    | n' < u → BoundVar n'
    | otherwise → BoundVar $ n + n'

shiftVar ∷ 𝕏 → 𝕐 → 𝕐
shiftVar x = \case
  NamedVar y n
    | x ≡ y → NamedVar y $ succ n
    | otherwise → NamedVar y n
  BoundVar n → BoundVar n

--------------------------
-- SUPPORT SUBSTITUTION --
--------------------------

class FromVar s a | a → s where
  frvar ∷ s → 𝕐 → a

newtype Subst s a = Subst { unSubst ∷ s → ℕ64 → 𝕐 → 𝑂 a }

mapSubst ∷ (s₂ → s₁) → (a → 𝑂 b) → Subst s₁ a → Subst s₂ b
mapSubst f g (Subst 𝓈) = Subst $ \ s u x → g *$ 𝓈 (f s) u x

nullSubst ∷ (FromVar s a) ⇒ Subst s a
nullSubst = Subst $ \ s _ x → return $ frvar s x

appendSubst ∷ (Binding s a b) ⇒ Subst s a → Subst s b → Subst s b
appendSubst 𝓈₁ (Subst 𝓈₂) = Subst $ \ s' u' y → do
  e ← 𝓈₂ s' u' y
  substN s' u' 𝓈₁ e

instance (FromVar s a) ⇒ Null (Subst s a) where null = nullSubst
instance (Binding s a a) ⇒ Append (Subst s a) where (⧺) = appendSubst
instance (FromVar s a,Binding s a a) ⇒ Monoid (Subst s a) 

class Binding s b a | a → s,a → b where
  substN ∷ s → ℕ64 → Subst s b → a → 𝑂 a

substNL ∷ (Binding s₂ b' a) ⇒ s₁ ⌲ s₂ → b ⌲ b' → s₁ → ℕ64 → Subst s₁ b → a → 𝑂 a
substNL ℓˢ ℓᵇ s₁ u 𝓈 =
  case view ℓˢ s₁ of
    None → return
    Some s₂ → substN s₂ u $ mapSubst (construct ℓˢ) (view ℓᵇ) 𝓈

subst ∷ (Binding s b a) ⇒ s → Subst s b → a → 𝑂 a
subst s = substN s zero

rename ∷ (FromVar s b) ⇒ (s → ℕ64 → 𝕐 → 𝕐) → Subst s b
rename f = Subst $ \ s u x → return $ frvar s $ f s u x

bdrOpen ∷ (Eq s,FromVar s b) ⇒ s → 𝕏 → Subst s b
bdrOpen s x = rename $ \ s' u y →
  if s ≡ s'
  then openVar u x y
  else y

bdrClose ∷ (Eq s,FromVar s b) ⇒ s → 𝕏 → Subst s b
bdrClose s x = rename $ \ s' u y → 
  if s ≡ s'
  then closeVar u x y
  else y

bdrBind ∷ (Eq s,FromVar s b) ⇒ s → b → Subst s b
bdrBind s e = Subst $ \ s' u y →
  return $
    if s ≡ s'
    then bindVar (frvar s) u e y
    else frvar s' y

bdrSubst ∷ (Eq s,FromVar s b) ⇒ s → 𝕏 → b → Subst s b
bdrSubst s x e = Subst $ \ s' _u y →
  return $
    if s ≡ s'
    then substVar (frvar s) x e y
    else frvar s' y

bdrIntro ∷ (Eq s,FromVar s b) ⇒ s → ℕ64 → Subst s b
bdrIntro s n = rename $ \ s' u y →
  if s ≡ s'
  then introVar u n y
  else y

bdrShift ∷ (Eq s,FromVar s b) ⇒ s → 𝕏 → Subst s b
bdrShift s x = rename $ \ s' _u y →
  if s ≡ s'
  then shiftVar x y
  else y

applySubst ∷ (Eq s,FromVar s b,Binding s b a) ⇒ s → (b → 𝑂 a) → ℕ64 → Subst s b → 𝕐 → 𝑂 a
applySubst s afrb u (Subst 𝓈) x = subst s (bdrIntro s u) *$ afrb *$ 𝓈 s u x

applySubstL 
  ∷ (Eq s₂,FromVar s₁ b,FromVar s₂ b',Binding s₂ b' a) 
  ⇒ s₁ ⌲ s₂ → b ⌲ b' → s₁ → (b → 𝑂 a) → ℕ64 → Subst s₁ b → 𝕐 → 𝑂 a
applySubstL ℓˢ ℓᵇ s₁ afrb u 𝓈 =
  case view ℓˢ s₁ of
    None → afrb ∘ frvar s₁
    Some s₂ → applySubst s₂ (afrb ∘ construct ℓᵇ) u $ mapSubst (construct ℓˢ) (view ℓᵇ) 𝓈
