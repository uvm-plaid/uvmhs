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

class (Ord s) ⇒ Binding s a | a → s where
  mkvar ∷ 𝕐 → a
  gsubstMN ∷ (Monad m) ⇒ ℕ64 → s ⇰ (ℕ64 → 𝕐 → m a) → a → m a

gsubstM ∷ (Monad m,Binding s a) ⇒ s ⇰ (ℕ64 → 𝕐 → m a) → a → m a
gsubstM = gsubstMN zero

gsubst ∷ (Binding s a) ⇒ s ⇰ (ℕ64 → 𝕐 → a) → a → a
gsubst 𝓈 e = unID $ gsubstM (map (\ f u x → ID $ f u x) 𝓈) e

grename ∷ (Binding s a) ⇒ s ⇰ (ℕ64 → 𝕐 → 𝕐) → a → a
grename 𝓈 = gsubst $ mapOn 𝓈 $ \ f n x → mkvar $ f n x

openTerm ∷ (Binding s a) ⇒ s → 𝕏 → a → a
openTerm s x = grename $ s ↦ openVar x

closeTerm ∷ (Binding s a) ⇒ s → 𝕏 → a → a
closeTerm s x = grename $ s ↦ closeVar x

bindTerm ∷ (Binding s a) ⇒ s → a → a → a
bindTerm s e = gsubst $ s ↦ bindVar mkvar (introTerm s) e

bindTermM ∷ (Monad m,Binding s a) ⇒ s → m a → a → m a
bindTermM s e = gsubstM $ s ↦ bindVar (return ∘ mkvar) (map ∘ introTerm s) e

substTerm ∷ (Binding s a) ⇒ s → 𝕏 → a → a → a
substTerm s x e = gsubst $ s ↦ substVar mkvar (introTerm s) x e

substTermM ∷ (Monad m,Binding s a) ⇒ s → 𝕏 → m a → a → m a
substTermM s x e = gsubstM $ s ↦ substVar (return ∘ mkvar) (map ∘ introTerm s) x e

introTerm ∷ (Binding s a) ⇒ s → ℕ64 → a → a
introTerm s m = grename $ s ↦ introVar m

shiftTerm ∷ (Binding s a) ⇒ s → 𝕏 → a → a
shiftTerm s x = grename $ s ↦ const (shiftVar x)

-- closeTerm ∷ (Binding s a) ⇒ 

-- rename x y = open y ∘ close x
-- subst x e = bind e ∘ close x
-- shift x = open x ∘ intro

-------------------
-- VARIABLE SETS --
-------------------

-- type FV = FV
--   { fvLexis ∷ 𝑃 𝕏
--   , fvMetas ∷ 𝑃 𝕏
--   , fvDebrs ∷ 𝑃 ℕ
--   } deriving (Eq,Ord,Show)
-- 
-- instance Pretty FV where
--   pretty (FV 𝓍ˡ 𝓍ᵐ 𝓍ᵈ) 
--     | 𝓍ˡ ≡ pø,𝓍ᵐ ≡ pø,𝓍ᵈ ≡ pø = ppLit "∅"
--     | 𝓍ᵐ ≡ pø,𝓍ᵈ ≡ pø = ppApp (ppPrim "lexi") $ single𝐼 $ pretty 𝓍ˡ
--     | 𝓍ˡ ≡ pø,𝓍ᵈ ≡ pø = ppApp (ppPrim "meta") $ single𝐼 $ pretty 𝓍ᵐ
--     | 𝓍ˡ ≡ pø,𝓍ᵐ ≡ pø = ppApp (ppPrim "debr") $ single𝐼 $ pretty 𝓍ᵈ
--     | otherwise = ppRecord  (ppPun "↦")
--         [ ppPrim "lexis" :* pretty 𝓍ˡ
--         , ppPrim "metas" :* pretty 𝓍ᵐ
--         , ppPrim "debrs" :* pretty 𝓍ᵈ
--         ]
-- 
-- instance POrd FV where
--   FV 𝓍ˡ₁ 𝓍ᵐ₁ 𝓍ᵈ₁ ⊑ FV 𝓍ˡ₂ 𝓍ᵐ₂ 𝓍ᵈ₂ = and [𝓍ˡ₁ ⊆ 𝓍ˡ₂,𝓍ᵐ₁ ⊆ 𝓍ᵐ₂,𝓍ᵈ₁ ⊆ 𝓍ᵈ₂]
-- instance Bot FV where 
--   bot = FV pø pø pø
-- instance Join FV where 
--   FV 𝓍ˡ₁ 𝓍ᵐ₁ 𝓍ᵈ₁ ⊔ FV 𝓍ˡ₂ 𝓍ᵐ₂ 𝓍ᵈ₂ = FV (𝓍ˡ₁ ∪ 𝓍ˡ₂) (𝓍ᵐ₁ ∪ 𝓍ᵐ₂) $ 𝓍ᵈ₁ ∪ 𝓍ᵈ₂ 
-- instance Meet FV where 
--   FV 𝓍ˡ₁ 𝓍ᵐ₁ 𝓍ᵈ₁ ⊓ FV 𝓍ˡ₂ 𝓍ᵐ₂ 𝓍ᵈ₂ = FV (𝓍ˡ₁ ∩ 𝓍ˡ₂) (𝓍ᵐ₁ ∩ 𝓍ᵐ₂) $ 𝓍ᵈ₁ ∩ 𝓍ᵈ₂
-- instance Difference FV where 
--   FV 𝓍ˡ₁ 𝓍ᵐ₁ 𝓍ᵈ₁ ⊟ FV 𝓍ˡ₂ 𝓍ᵐ₂ 𝓍ᵈ₂ = FV (𝓍ˡ₁ ∖ 𝓍ˡ₂) (𝓍ᵐ₁ ∖ 𝓍ᵐ₂) $ 𝓍ᵈ₁ ∖ 𝓍ᵈ₂
-- instance JoinLattice FV
-- 
-- fvLexi ∷ 𝑃 𝕏 → FV
-- fvLexi 𝓍 = FV 𝓍 pø pø
-- 
-- fvMeta ∷ 𝑃 𝕏 → FV
-- fvMeta 𝓍 = FV pø 𝓍 pø
-- 
-- fvDebr ∷ 𝑃 ℕ → FV
-- fvDebr 𝓍 = FV pø pø 𝓍

-------------------------
-- VARIABLE SCOPE SETS --
-------------------------

-- class (Ord s) ⇒ HasFV s a | a → s where
--   fv ∷ a → s ⇰ 𝑃 𝕐
-- 
-- data Sub s a = Sub
--   { subVals ∷ 𝕐 ⇰ a
--   , subFree ∷ s ⇰ 𝑃 𝕐
--   } deriving (Eq,Ord,Show)

-- -------------------
-- -- SUBSTITUTIONS --
-- -------------------
-- 
-- data Sub s a = Sub
--   { subLexis ∷ 𝕏 ⇰ a
--   , subMetas ∷ 𝕏 ⇰ a
--   , subDebrs ∷ ℕ ⇰ a
--   , subFrees ∷ s ⇰ FV
--   } deriving (Eq,Ord,Show)
-- 
-- instance (Pretty s,Eq a,Pretty a) ⇒ Pretty (Sub s a) where
--   pretty (Sub 𝓈ˡ 𝓈ᵐ 𝓈ᵈ 𝓍)
--     | 𝓈ˡ ≡ dø,𝓈ᵐ ≡ dø,𝓈ᵈ ≡ dø = ppLit "∅"
--     | 𝓈ᵐ ≡ dø,𝓈ᵈ ≡ dø = ppApp (ppPrim "lexi") $ [pretty 𝓈ˡ,pretty 𝓍]
--     | 𝓈ˡ ≡ dø,𝓈ᵈ ≡ dø = ppApp (ppPrim "meta") $ [pretty 𝓈ᵐ,pretty 𝓍]
--     | 𝓈ˡ ≡ dø,𝓈ᵐ ≡ dø = ppApp (ppPrim "debr") $ [pretty 𝓈ᵈ,pretty 𝓍]
--     | otherwise = ppRecord  (ppPun "↦")
--         [ ppPrim "lexis" :* pretty 𝓈ˡ
--         , ppPrim "metas" :* pretty 𝓈ᵐ
--         , ppPrim "debrs" :* pretty 𝓈ᵈ
--         , ppPrim "fvars" :* pretty 𝓍
--         ]
-- 
-- instance Null (Sub s a) where 
--   null = Sub dø dø dø bot
-- instance (Ord s) ⇒ Append (Sub s a) where 
--   Sub 𝓈ˡ₁ 𝓈ᵐ₁ 𝓈ᵈ₁ 𝓍₁ ⧺ Sub 𝓈ˡ₂ 𝓈ᵐ₂ 𝓈ᵈ₂ 𝓍₂ = Sub (𝓈ˡ₁ ⩌ 𝓈ˡ₂) (𝓈ᵐ₁ ⩌ 𝓈ᵐ₂) (𝓈ᵈ₁ ⩌ 𝓈ᵈ₂) $ 𝓍₁ ⊔ 𝓍₂
-- instance (Ord s) ⇒ Monoid (Sub s a)
-- 
-- mkSub ∷ (HasFV s a) ⇒ 𝕏 ⇰ a → 𝕏 ⇰ a → ℕ ⇰ a → Sub s a
-- mkSub 𝓈ˡ 𝓈ᵐ 𝓈ᵈ =
--   let 𝓍 = joins $ map fv $ concat [values 𝓈ˡ,values 𝓈ᵐ,values 𝓈ᵈ]
--   in Sub 𝓈ˡ 𝓈ᵐ 𝓈ᵈ 𝓍
-- 
-- subLexi ∷ (HasFV s a) ⇒ 𝕏 ⇰ a → Sub s a
-- subLexi 𝓈ˡ = mkSub 𝓈ˡ dø dø
-- 
-- subMeta ∷ (HasFV s a) ⇒ 𝕏 ⇰ a → Sub s a
-- subMeta 𝓈ᵐ = mkSub dø 𝓈ᵐ dø
-- 
-- subDebr ∷ (HasFV s a) ⇒ ℕ ⇰ a → Sub s a
-- subDebr 𝓈ᵈ = mkSub dø dø 𝓈ᵈ
-- 
-- subRestrict ∷ (HasFV s a) ⇒ FV → Sub s a → Sub s a
-- subRestrict (FV 𝓍ˡ 𝓍ᵐ 𝓍ᵈ) (Sub 𝓈ˡ 𝓈ᵐ 𝓈ᵈ _𝓍) = mkSub (restrict 𝓍ˡ 𝓈ˡ) (restrict 𝓍ᵐ 𝓈ᵐ) $ restrict 𝓍ᵈ 𝓈ᵈ
-- 
-- subWithout ∷ (HasFV s a) ⇒ FV → Sub s a → Sub s a
-- subWithout (FV 𝓍ˡ 𝓍ᵐ 𝓍ᵈ) (Sub 𝓈ˡ 𝓈ᵐ 𝓈ᵈ _𝓍) = mkSub (without 𝓍ˡ 𝓈ˡ) (without 𝓍ᵐ 𝓈ᵐ) $ without 𝓍ᵈ 𝓈ᵈ
-- 
-- subSupport ∷ (Eq a) ⇒ Sub s a → FV
-- subSupport (Sub 𝓈ˡ 𝓈ᵐ 𝓈ᵈ _𝓍) = FV (keys 𝓈ˡ) (keys 𝓈ᵐ) $ keys 𝓈ᵈ
-- 
-- subValues ∷ Sub s a → 𝐼 a
-- subValues (Sub 𝓈ˡ 𝓈ᵐ 𝓈ᵈ _𝓍) = concat [values 𝓈ˡ,values 𝓈ᵐ,values 𝓈ᵈ]
-- 
-- substVarLexi ∷ (Monad m) ⇒ (𝕏 → b) → (a → m b) → Sub s a → 𝕏 → m b
-- substVarLexi mkvar 𝒸 𝓈 x = case subLexis 𝓈 ⋕? x of
--   None → return $ mkvar x
--   Some e → 𝒸 e
-- 
-- substVarMeta ∷ (Monad m) ⇒ (𝕏 → b) → (a → m b) → Sub s a → 𝕏 → m b
-- substVarMeta mkvar 𝒸 𝓈 x = case subMetas 𝓈 ⋕? x of
--   None → return $ mkvar x
--   Some e → 𝒸 e
-- 
-- underBdrLexi ∷ (Monad m,Ord s,HasFV s a) ⇒ s → (𝕏 → m 𝕏) → (𝕏 → a) → 𝕏 → Sub s a → m (𝕏 ∧ Sub s a)
-- underBdrLexi s gsym mkvar x 𝓈 = do
--   if x ∈ fvLexis (subFrees 𝓈 ⋕! s)
--   then do
--     x' ← gsym x
--     let 𝓈' = subLexi (x ↦ mkvar x') ⧺ 𝓈
--     return $ x' :* 𝓈'
--   else return $ x :* 𝓈
-- 
-- underBdrDebr ∷ Sub s a → Sub s a
-- underBdrDebr 𝓈 =
--   let 𝓈ᵈ = subDebrs 𝓈
--       𝓈ᵈ' = assoc $ map (mapFst succ) $ iter 𝓈ᵈ
--    in 𝓈 { subDebrs = 𝓈ᵈ' }
-- 
-- mapMSub ∷ (Monad m,HasFV s b) ⇒ (a → m b) → Sub s a → m (Sub s b)
-- mapMSub f (Sub 𝓈ˡ 𝓈ᵐ 𝓈ᵈ _𝓍) = do
--   𝓈ˡ' ← dict ^$ mapMOn (iter 𝓈ˡ) $ \ (x :* a) → do
--     b ← f a
--     return $ x ↦ b
--   𝓈ᵐ' ← dict ^$ mapMOn (iter 𝓈ᵐ) $ \ (χ :* a) → do
--     b ← f a
--     return $ χ ↦ b
--   𝓈ᵈ' ← dict ^$ mapMOn (iter 𝓈ᵈ) $ \ (χ :* a) → do
--     b ← f a
--     return $ χ ↦ b
--   return $ mkSub 𝓈ˡ' 𝓈ᵐ' 𝓈ᵈ'
