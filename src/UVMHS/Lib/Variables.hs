module UVMHS.Lib.Variables where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Parser

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

cpName ∷ CParser TokenBasic 𝕏
cpName = var ^$ cpShaped $ view nameTBasicL

cpNameWS ∷ CParser TokenWSBasic 𝕏
cpNameWS = var ^$ cpShaped $ view nameTWSBasicL

-----------------------------------------
-- LOCALLY NAMELESS WITH SHIFTED NAMES --
-----------------------------------------

data 𝕐 =
    NamedVar 𝕏 ℕ64
  | BoundVar ℕ64
  deriving (Eq,Ord,Show)
makePrisms ''𝕐

free ∷ 𝕏 → 𝕐
free x = NamedVar x zero

freeL ∷ 𝕐 ⌲ 𝕏
freeL = prism free $ \case
  NamedVar x n | n ≡ zero → return x
  _ → abort

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
    | x ≡ y,n ≡ zero → BoundVar u
    | x ≡ y,n ≢ zero → NamedVar y $ pred n
    | otherwise      → NamedVar y n
  BoundVar n 
    | n < u → BoundVar n
    | otherwise → BoundVar $ n + one

bindVar ∷ ℕ64 → a → 𝕐 → 𝕐 ∨ a
bindVar u e = \case
  NamedVar x n → Inl $ NamedVar x n
  BoundVar n
    | n < u → Inl $ BoundVar n
    | n ≡ u → Inr e
    | otherwise → Inl $ BoundVar $ pred n

substVar ∷ 𝕏 ⇰ a → 𝕐 → 𝕐 ∨ a
substVar xes = \case
  NamedVar y n → case xes ⋕? y of
    None → Inl $ NamedVar y n
    Some e 
      | n ≡ zero  → Inr e
      | otherwise → Inl $ NamedVar y $ pred n
  BoundVar n → Inl $ BoundVar n

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
  frvar ∷ 𝑃 SrcCxt → s → 𝕐 → 𝑂 a

newtype Subst s a = Subst { unSubst ∷ s ⇰ ℕ64 → 𝑃 SrcCxt → s → 𝕐 → 𝑂 a }

mapSubst ∷ (Ord s₁) ⇒ (s₂ → s₁) → (a → 𝑂 b) → Subst s₁ a → Subst s₂ b
mapSubst f g (Subst 𝓈) = Subst $ \ su cxt s x → 
  let su' = concat $ mapOn (iter su) $ \ (s' :* u) → f s' ↦ u
  in g *$ 𝓈 su' cxt (f s) x

nullSubst ∷ (FromVar s a) ⇒ Subst s a
nullSubst = Subst $ \ _ cxt s x → frvar cxt s x

appendSubst ∷ (Binding s a b) ⇒ Subst s a → Subst s b → Subst s b
appendSubst 𝓈₁ (Subst 𝓈₂) = Subst $ \ su cxt s x → do
  e ← 𝓈₂ su cxt s x
  substScope su 𝓈₁ e

instance (FromVar s a) ⇒ Null (Subst s a) where null = nullSubst
instance (Binding s a a) ⇒ Append (Subst s a) where (⧺) = appendSubst
instance (FromVar s a,Binding s a a) ⇒ Monoid (Subst s a) 

class (Ord s) ⇒ Binding s b a | a → s,a → b where
  substScope ∷ s ⇰ ℕ64 → Subst s b → a → 𝑂 a

substScopeRestrict ∷ (Ord s₁,Binding s₂ b' a) ⇒ s₁ ⌲ s₂ → (b → 𝑂 b') → s₁ ⇰ ℕ64 → Subst s₁ b → a → 𝑂 a
substScopeRestrict ℓˢ mkb su 𝓈 =
  let su' = concat $ mapOn (iter su) $ \ (s :* u) →
        case view ℓˢ s of
          None → null
          Some s' → s' ↦ u
  in substScope su' $ mapSubst (construct ℓˢ) mkb 𝓈

subst ∷ (Binding s b a) ⇒ Subst s b → a → 𝑂 a
subst = substScope null

rename ∷ (FromVar s a) ⇒ (s ⇰ ℕ64 → s → 𝕐 → 𝕐) → Subst s a
rename f = Subst $ \ su cxt s x → frvar cxt s $ f su s x

bdrOpen ∷ (Ord s,FromVar s a) ⇒ s → 𝕏 → Subst s a
bdrOpen s x = rename $ \ su s' y →
  if s ≡ s'
  then 
    let u = ifNone zero $ su ⋕? s
    in openVar u x y
  else y

bdrClose ∷ (Ord s,FromVar s a) ⇒ s → 𝕏 → Subst s a
bdrClose s x = rename $ \ su s' y → 
  if s ≡ s'
  then 
    let u = ifNone zero $ su ⋕? s
    in closeVar u x y
  else y

bdrBindWith 
  ∷ (Ord s,FromVar s a,FromVar s b,Binding s b a) 
  ⇒ (s ⇰ ℕ64 → a → 𝑂 a) → s → a → Subst s a
bdrBindWith f s e = Subst $ \ su cxt s' y →
  if s ≡ s'
  then do
    let u = ifNone zero $ su ⋕? s
    case bindVar u e y of
      Inl y' → frvar cxt s y'
      Inr e' → f su e'
  else frvar cxt s' y

bdrBind ∷ (Ord s,FromVar s a,FromVar s b,Binding s b a) ⇒ s → a → Subst s a
bdrBind = bdrBindWith $ \ su e → subst (bdrIntro su) e

bdrBindNoIntro ∷ (Ord s,FromVar s a,FromVar s b,Binding s b a) ⇒ s → a → Subst s a
bdrBindNoIntro = bdrBindWith $ const return

bdrSubstWith 
  ∷ (Ord s,FromVar s a,FromVar s b,Binding s b a) 
  ⇒ (s ⇰ ℕ64 → a → 𝑂 a) → s ⇰ 𝕏 ⇰ a → Subst s a
bdrSubstWith f sxes = Subst $ \ su cxt s' y →
  case sxes ⋕? s' of
    None → frvar cxt s' y
    Some xes →
      case substVar xes y of
        Inl y' → frvar cxt s' y'
        Inr e' → f su e'

bdrSubst ∷ (Ord s,FromVar s a,FromVar s b,Binding s b a) ⇒ s ⇰ 𝕏 ⇰ a → Subst s a
bdrSubst = bdrSubstWith $ \ su e → subst (bdrIntro su) e

bdrSubstNoIntro ∷ (Ord s,FromVar s a,FromVar s b,Binding s b a) ⇒ s ⇰ 𝕏 ⇰ a → Subst s a
bdrSubstNoIntro = bdrSubstWith $ const return

bdrIntro ∷ (Ord s,FromVar s a) ⇒ s ⇰ ℕ64 → Subst s a
bdrIntro su = rename $ \ su' s y →
  let u = ifNone zero $ su' ⋕? s
      n = ifNone zero $ su ⋕? s
  in introVar u n y

bdrShift ∷ (Eq s,FromVar s a) ⇒ s → 𝕏 → Subst s a
bdrShift s x = rename $ \ _su s' y →
  if s ≡ s'
  then shiftVar x y
  else y

vsubst ∷ (b → 𝑂 a) → Subst s b → s ⇰ ℕ64 → 𝑃 SrcCxt → s → 𝕐 → 𝑂 a
vsubst afrb (Subst 𝓈) su cxt s x = afrb *$ 𝓈 su cxt s x

---------------
-- FREE VARS --
---------------

class HasFV a where
  fv ∷ a → 𝑃 𝕏

fvVar ∷ 𝕐 → 𝑃 𝕏
fvVar = elim𝑂 null single ∘ view freeL

instance HasFV 𝕐 where fv = fvVar
