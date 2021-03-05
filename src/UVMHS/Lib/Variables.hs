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

class FromVar a where
  frvar ∷ 𝕐 → a

class Binding s b a | a → s,a → b where
  gsubstMN ∷ s → ℕ64 → (ℕ64 → 𝕐 → 𝑂 b) → a → 𝑂 a

gsubstM ∷ (Binding s b a) ⇒ s → (ℕ64 → 𝕐 → 𝑂 b) → a → 𝑂 a
gsubstM s = gsubstMN s zero

gsubst ∷ (HasPrism b a,Binding s b a) ⇒ s → (ℕ64 → 𝕐 → a) → a → a
gsubst s 𝓈 e = 
  ifNone (error "gsubst: bad termL prism") 
  $ gsubstM s ((Some ∘ ι) ∘∘ 𝓈) e

grename ∷ (FromVar a,HasPrism b a,Binding s b a) ⇒ s → (ℕ64 → 𝕐 → 𝕐) → a → a
grename s 𝓈 e = gsubst s (frvar ∘∘ 𝓈) e

openTerm ∷ (FromVar a,HasPrism b a,Binding s b a) ⇒ s → 𝕏 → a → a
openTerm s x = grename s $ openVar x 

closeTerm ∷ (FromVar a,HasPrism b a,Binding s b a) ⇒ s → 𝕏 → a → a
closeTerm s x = grename s $ closeVar x 

bindTermM ∷ (FromVar b,Binding s b a) ⇒ s → b → a → 𝑂 a
bindTermM s e = gsubstM s $ return ∘∘ bindVar frvar e

bindTerm ∷ (FromVar a,HasPrism b a,Binding s b a) ⇒ s → a → a → a
bindTerm s e = gsubst s $ \ u x → bindVar frvar e u x

substTermM ∷ ∀ s b a. (FromVar b,Binding s b a) ⇒ s → 𝕏 → b → a → 𝑂 a
substTermM s x e = gsubstM s $ const $ return ∘ substVar frvar x e

substTerm ∷ (FromVar a,HasPrism b a,Binding s b a) ⇒ s → 𝕏 → a → a → a
substTerm s x e = gsubst s $ const $ substVar frvar x e

introTerm ∷ (FromVar a,HasPrism b a,Binding s b a) ⇒ s → ℕ64 → a → a
introTerm s m = grename s $ introVar m

shiftTerm ∷ (FromVar a,HasPrism b a,Binding s b a) ⇒ s → 𝕏 → a → a
shiftTerm s x = grename s $ const $ shiftVar x

applySubst ∷ (FromVar a,HasPrism b a,Binding s b a) ⇒ s → ℕ64 → (ℕ64 → 𝕐 → 𝑂 b) → 𝕐 → 𝑂 a
applySubst s u 𝓈 x = do
  e' ← 𝓈 u x
  e'' ← ιview e'
  return $ introTerm s u e''
