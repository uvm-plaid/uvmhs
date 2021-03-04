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

named ∷ 𝕊 → 𝕐
named x = NamedVar (var x) zero

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

bindVar ∷ (𝕐 → a) → (ℕ64 → a → a) → a → ℕ64 → 𝕐 → a
bindVar mkvar' intro' e u = \case
  NamedVar x n → mkvar' $ NamedVar x n
  BoundVar n
    | n < u → mkvar' $ BoundVar n
    | n ≡ zero → intro' u e
    | otherwise → mkvar' $ BoundVar $ pred n

substVar ∷ (𝕐 → a) → (ℕ64 → a → a) → 𝕏 → a → ℕ64 → 𝕐 → a
substVar mkvar' intro' x e u = \case
  NamedVar y n
    | x ≡ y,n ≡ zero → intro' u e
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

class (Ord s) ⇒ Binding s a | a → s where
  mkvar ∷ 𝕐 → a
  gsubstMN ∷ (Monad m) ⇒ (s → ℕ64) → (s → ℕ64 → 𝕐 → m a) → a → m a

gsubstM ∷ (Monad m,Binding s a) ⇒ (s → ℕ64 → 𝕐 → m a) → a → m a
gsubstM = gsubstMN $ const zero

gsubst ∷ (Binding s a) ⇒ (s → ℕ64 → 𝕐 → a) → a → a
gsubst 𝓈 e = unID $ gsubstM (\ s u x → ID $ 𝓈 s u x) e

grename ∷ (Binding s a) ⇒ (s → ℕ64 → 𝕐 → 𝕐) → a → a
grename 𝓈 = gsubst $ \ s u x → mkvar $ 𝓈 s u x

openTerm ∷ (Binding s a) ⇒ s → 𝕏 → a → a
openTerm s x = grename $ \ s' → 
  if s ≢ s' then const id
  else openVar x 

closeTerm ∷ (Binding s a) ⇒ s → 𝕏 → a → a
closeTerm s x = grename $ \ s' → 
  if s ≢ s' then const id
  else closeVar x 

bindTerm ∷ (Binding s a) ⇒ s → a → a → a
bindTerm s e = gsubst $ \ s' → 
  if s ≢ s' then const mkvar
  else bindVar mkvar (introTerm s) e 

bindTermM ∷ (Monad m,Binding s a) ⇒ s → m a → a → m a
bindTermM s e = gsubstM $ \ s' → 
  if s ≢ s' then const $ return ∘ mkvar
  else bindVar (return ∘ mkvar) (map ∘ introTerm s) e

substTerm ∷ (Binding s a) ⇒ s → 𝕏 → a → a → a
substTerm s x e = gsubst $ \ s' →
  if s ≢ s' then const mkvar
  else substVar mkvar (introTerm s) x e

substTermM ∷ (Monad m,Binding s a) ⇒ s → 𝕏 → m a → a → m a
substTermM s x e = gsubstM $ \ s' →
  if s ≢ s' then const $ return ∘ mkvar
  else substVar (return ∘ mkvar) (map ∘ introTerm s) x e

introTerm ∷ (Binding s a) ⇒ s → ℕ64 → a → a
introTerm s m = grename $ \ s' →
  if s ≢ s' then const id
  else introVar m

shiftTerm ∷ (Binding s a) ⇒ s → 𝕏 → a → a
shiftTerm s x = grename $ \ s' →
  if s ≢ s' then const id
  else const $ shiftVar x
