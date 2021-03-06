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

openVar ∷ 𝕏 → ℕ64 → 𝕐 → 𝕐
openVar x u = \case
  NamedVar y n 
    | x ≡ y → NamedVar y $ succ n
    | otherwise → NamedVar y n
  BoundVar n
    | n < u → BoundVar n
    | n ≡ u → NamedVar x zero
    | otherwise → BoundVar $ pred n

closeVar ∷ 𝕏 → ℕ64 → 𝕐 → 𝕐
closeVar x u = \case
  NamedVar y n
    | x ≡ y,n ≡ zero → BoundVar zero
    | x ≡ y,n ≢ zero → NamedVar y $ pred n
    | otherwise      → NamedVar y n
  BoundVar n 
    | n < u → BoundVar n
    | otherwise → BoundVar $ n + one

bindVar ∷ (𝕐 → a) → a → ℕ64 → 𝕐 → a
bindVar mkvar' e u = \case
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
introVar m u = \case
  NamedVar x n → NamedVar x n
  BoundVar n 
    | n < u → BoundVar n
    | otherwise → BoundVar $ m + n

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

class Binding s b a | a → s,a → b where
  gsubstMN ∷ s → ℕ64 → (ℕ64 → 𝕐 → 𝑂 b) → a → 𝑂 a

gsubstM ∷ (Binding s b a) ⇒ s → (ℕ64 → 𝕐 → 𝑂 b) → a → 𝑂 a
gsubstM s = gsubstMN s zero

grename ∷ (FromVar s b,Binding s b a) ⇒ s → (ℕ64 → 𝕐 → 𝕐) → a → a
grename s 𝓈 e = 
  ifNone (error "grename: bad handling of substitution for variables") $
  gsubstM s ((Some ∘ frvar s) ∘∘ 𝓈) e

openTerm ∷ (FromVar s b,Binding s b a) ⇒ s → 𝕏 → a → a
openTerm s x = grename s $ openVar x 

closeTerm ∷ (FromVar s b,Binding s b a) ⇒ s → 𝕏 → a → a
closeTerm s x = grename s $ closeVar x 

bindTermM ∷ (FromVar s b,Binding s b a) ⇒ s → b → a → 𝑂 a
bindTermM s e = gsubstM s $ return ∘∘ bindVar (frvar s) e

substTermM ∷ ∀ s b a. (FromVar s b,Binding s b a) ⇒ s → 𝕏 → b → a → 𝑂 a
substTermM s x e = gsubstM s $ const $ return ∘ substVar (frvar s) x e

introTerm ∷ (FromVar s b,Binding s b a) ⇒ s → ℕ64 → a → a
introTerm s m = grename s $ introVar m

shiftTerm ∷ (FromVar s b,Binding s b a) ⇒ s → 𝕏 → a → a
shiftTerm s x = grename s $ const $ shiftVar x

applySubst ∷ (FromVar s b,Binding s b a) ⇒ s → (b → 𝑂 a) → ℕ64 → (ℕ64 → 𝕐 → 𝑂 b) → 𝕐 → 𝑂 a
applySubst s afrb u 𝓈 x = introTerm s u ^$ afrb *$ 𝓈 u x

gsubstMNS ∷ (Binding s₂ b a) ⇒ s₁ ⌲ s₂ → s₁ → ℕ64 → (ℕ64 → 𝕐 → 𝑂 b) → a → 𝑂 a
gsubstMNS ℓ s₁ u 𝓈 e =
  case view ℓ s₁ of
    None → return e 
    Some s₂ → gsubstMN s₂ u 𝓈 e

applySubstS 
  ∷ (Eq s₁,FromVar s₁ a,FromVar s₁ b,Binding s₁ b a) 
  ⇒ s₁ → s₁ → (b → 𝑂 a) → ℕ64 → (ℕ64 → 𝕐 → 𝑂 b) → 𝕐 → 𝑂 a
applySubstS s₁ s₁' afrb u 𝓈 x =
  if s₁ ≢ s₁'
  then return $ frvar s₁ x
  else applySubst s₁ afrb u 𝓈 x
