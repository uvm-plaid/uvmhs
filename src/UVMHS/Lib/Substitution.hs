module UVMHS.Lib.Substitution where

import UVMHS.Core
import UVMHS.Lib.Variables
import UVMHS.Lib.Pretty
import UVMHS.Lib.Rand

---------------------------------
-- GENERIC SUBSTITUION ELEMENT --
---------------------------------

-- ℯ ⩴ i | s⇈e
data GSubstElem s a = 
    Var_GSE ℕ64
  | Val_GSE s (() → 𝑂 a)
  deriving (Eq)

instance (Pretty s,Pretty a) ⇒ Pretty (GSubstElem s a) where
  pretty = \case
    Var_GSE i → pretty $ DVar i
    Val_GSE n e → concat
      [ ppPun $ ppshow n
      , ppPun "⇈"
      , ifNone (ppPun "bu") $ pretty ^$ e ()
      ]

introGSubstElem ∷ (Additive 𝑠) ⇒ (𝑠 → ℕ64 → ℕ64) → 𝑠 → GSubstElem 𝑠 e → GSubstElem 𝑠 e
introGSubstElem introVar 𝑠 = \case
  Var_GSE n → Var_GSE $ introVar 𝑠 n
  Val_GSE 𝑠' ueO → Val_GSE (𝑠' + 𝑠 ) ueO

------------------------------------
-- GENERIC DE BRUIJN SUBSTITUTION --
------------------------------------

-- 𝓈 ⩴ ⟨ρ,es,ι⟩ 
-- INVARIANT: |es| + ι ≥ 0
data GDSubst 𝑠 e = GDSubst
  { dsubstShift ∷ ℕ64
  , dsubstElems ∷ 𝕍 (GSubstElem 𝑠 e)
  , dsubstIntro ∷ ℤ64
  } deriving (Eq)
makeLenses ''GDSubst
makePrettyRecord ''GDSubst

isNullGDSubst ∷ GDSubst 𝑠 e → 𝔹
isNullGDSubst (GDSubst ρ es ι) = ρ ≡ 0 ⩓ csize es ≡ 0 ⩓ ι ≡ 0

-- 𝓈 ≜ ⟨ρ,es,ι⟩
-- 𝔰 ≜ |es|
-- 𝓈(i) ≜
--   cases (disjoint):
--     |       i < ρ   ⇒ i
--     |   ρ ≤ i < ρ+𝔰 ⇒ es[i-ρ]
--     | ρ+𝔰 ≤ i       ⇒ i+ι
-- 𝓈(i) ≜
--   cases (sequential):
--     | i < ρ   ⇒ i
--     | i < ρ+𝔰 ⇒ es[i-ρ]
--     | ⊤       ⇒ i+ι
-- e.g.,
-- 𝓈 = ⟨2,[e],-1⟩
-- 𝓈 is logically equivalent to the (infinite) substitution vector
-- [ …
-- ,  0 ↦ ⌊ 0⌋    | ≡
-- ,  1 ↦ ⌊ 1⌋    |
-- ----------------
-- ,  2 ↦   e     | [e]
-- ----------------
-- ,  3 ↦ ⌊ 2⌋    | -1
-- ,  4 ↦ ⌊ 3⌋    |
-- , …
-- ]
gsubstVar ∷ GDSubst 𝑠 e → ℕ64 → GSubstElem 𝑠 e
gsubstVar (GDSubst ρ̇ es ι) ṅ =
  let 𝔰̇  = csize es
      n  = intΩ64 ṅ
  in 
  if
  | ṅ < ρ̇     → Var_GSE ṅ
  | ṅ < 𝔰̇+ρ̇   → es ⋕! (ṅ-ρ̇)
  | otherwise → Var_GSE $ natΩ64 $ n+ι

-- esubst(𝑠,𝓈,e) ≡ 𝓈(𝑠⇈e)
gsubstElem ∷ (Null 𝑠) ⇒ (𝑠 → e → 𝑂 e) → GDSubst 𝑠 e → GSubstElem 𝑠 e → GSubstElem 𝑠 e
gsubstElem esubst 𝓈 = \case
  Var_GSE n     → gsubstVar 𝓈 n
  Val_GSE 𝑠 ueO → Val_GSE null $ \ () → esubst 𝑠 *$ ueO ()

----------------------------
-- DE BRUIJN SUBSTITUTION --
----------------------------

type DSubstElem = GSubstElem ℕ64

newtype DSubst e = DSubst { unDSubst ∷ GDSubst ℕ64 e }

introDSubstElem ∷ ℕ64 → DSubstElem e → DSubstElem e
introDSubstElem = introGSubstElem (+)

𝓈shiftD ∷ ℕ64 → DSubst e → DSubst e
𝓈shiftD n 𝓈 =
  let GDSubst ρ es ι = unDSubst 𝓈
      ρ'             = ρ+n
      es'            = mapOn es $ introDSubstElem n
  in DSubst $ GDSubst ρ' es' ι

𝓈introD ∷ ℕ64 → DSubst e
𝓈introD n = DSubst $ GDSubst zero null $ intΩ64 n

𝓈bindsD ∷ 𝕍 e → DSubst e
𝓈bindsD es = 
  let ℯs = map (Val_GSE 0 ∘ const ∘ return) es
      ι  = neg $ intΩ64 $ csize es
  in DSubst $ GDSubst zero ℯs ι

𝓈bindD ∷ e → DSubst e
𝓈bindD = 𝓈bindsD ∘ single

dsubstElem ∷ (DSubst e → ℕ64 → e → 𝑂 e) → DSubst e → DSubstElem e → DSubstElem e
dsubstElem esubst 𝓈 = gsubstElem (esubst 𝓈) $ unDSubst 𝓈

-- 𝓈₁ ≜ ⟨ρ₁,es₁,ι₁⟩
-- 𝓈₂ ≜ ⟨ρ₂,es₂,ι₂⟩
-- 𝔰₁ = |es₁| 
-- 𝔰₂ = |es₂| 
-- (𝓈₂⧺𝓈₁)(i) 
-- ==
-- 𝓈₂(𝓈₁(i))
-- ==
-- cases (sequential):
--   | i < ρ₁    ⇒ 𝓈₂(i)
--   | i < ρ₁+𝔰₁ ⇒ 𝓈₂(es₁[i-ρ₁])
--   | ⊤         ⇒ 𝓈₂(i+ι₁)
-- ==
-- cases (sequential):
--   | i < ρ₁    ⇒ cases (sequential):
--                    | i < ρ₂    ⇒ i
--                    | i < ρ₂+𝔰₂ ⇒ es₂[i-ρ₂]
--                    | ⊤         ⇒ i+ι₂
--   | i < ρ₁+𝔰₁ ⇒ 𝓈₂(es₁[i-ρ₁])
--   | ⊤         ⇒ cases (sequential):
--                    | i < ρ₂-ι₁    ⇒ i+ι₁
--                    | i < ρ₂+𝔰₂-ι₁ ⇒ es₂[i+ι₁-ρ₂]
--                    | ⊤            ⇒ i+ι₁+ι₂
-- ==
-- cases (sequential):
--   | i < ρ₁⊓ρ₂      ⇒ i
--   ---------------------------------
--   | i < ρ₁⊓(ρ₂+𝔰₂) ⇒ es₂[i-ρ₂]
--   | i < ρ₁         ⇒ i+ι₂
--   | i < ρ₁+𝔰₁      ⇒ 𝓈₂(es₁[i-ρ₁])
--   | i < ρ₂-ι₁      ⇒ i+ι₁
--   | i < ρ₂+𝔰₂-ι₁   ⇒ es₂[i+ι₁-ρ₂]
--   ---------------------------------
--   | ⊤              ⇒ i+ι₁+ι₂
-- == ⟨ρ,es,ι⟩(i)
-- where
--     ρ = ρ₁⊓ρ₂
--     ι = ι₁+ι₂
--     𝔰 ≜ |es|
--   ρ+𝔰 = (ρ₁+𝔰₁)⊔(ρ₂+𝔰₂-ι₁)
--     𝔰 = ((ρ₁+𝔰₁)⊔(ρ₂+𝔰₂-ι₁))-ρ

dsubstAppend ∷ (DSubst e → e → 𝑂 e) → DSubst e → DSubst e → DSubst e
dsubstAppend esubst 𝓈₂ 𝓈₁ =
  if
  | isNullGDSubst $ unDSubst 𝓈₁ → 𝓈₂
  | isNullGDSubst $ unDSubst 𝓈₂ → 𝓈₁
  | otherwise →
      let GDSubst ρ̇₂ es₂ ι₂ = unDSubst 𝓈₂
          GDSubst ρ̇₁ es₁ ι₁ = unDSubst 𝓈₁
          𝔰₁ = intΩ64 $ csize es₁
          𝔰₂ = intΩ64 $ csize es₂
          ρ₁  = intΩ64 ρ̇₁
          ρ₂  = intΩ64 ρ̇₂
          ρ̇   = ρ̇₁⊓ρ̇₂
          ρ   = intΩ64 ρ̇
          ι   = ι₁+ι₂
          𝔰  = ((ρ₁+𝔰₁)⊔(ρ₂+𝔰₂-ι₁))-ρ
          δ₂  = ρ
          sub = dsubstElem $ \ 𝓈 n → esubst $ dsubstAppend esubst (𝓈introD n) 𝓈
          es = vecF (natΩ64 𝔰) $ \ ṅ → 
            let n = intΩ64 ṅ + δ₂ in 
            if
            | n < ρ₁⊓(ρ₂+𝔰₂) → es₂ ⋕! natΩ64 (n-ρ₂)
            | n < ρ₁         → Var_GSE $ natΩ64 $ n+ι₂
            | n < ρ₁+𝔰₁      → sub 𝓈₂ $ es₁ ⋕! natΩ64 (n-ρ₁)
            | n < ρ₂-ι₁      → Var_GSE $ natΩ64 $ n+ι₁
            | n < ρ₂+𝔰₂-ι₁   → es₂ ⋕! natΩ64 (n+ι₁-ρ₂)
            | otherwise      → error "bad"
      in
      DSubst $ GDSubst ρ̇ es ι

-------------------------
-- SCOPED SUBSTITUTION --
-------------------------

type SubstElem s = GSubstElem (s ⇰ ℕ64)
data Subst s₁ s₂ e = Subst 
  { substGlobal ∷ s₁ ⇰ ((s₂ ⇰ ℕ64) ∧ (() → 𝑂 e))
  , substScoped ∷ s₂ ⇰ GDSubst (s₂ ⇰ ℕ64) e 
  } 
  deriving (Eq)
makeLenses ''Subst
makePrettyUnion ''Subst

introSubstElem ∷ (Ord s) ⇒ s → s ⇰ ℕ64 → SubstElem s e → SubstElem s e
introSubstElem s = introGSubstElem $ \ 𝑠 n → n + ifNone 0 (𝑠 ⋕? s)

𝓈shiftG ∷ (Ord s₂) ⇒ s₂ ⇰ ℕ64 → Subst s₁ s₂ e → Subst s₁ s₂ e
𝓈shiftG 𝑠 (Subst esᴳ 𝓈s) = 
  let 𝓈s' = mapWithKeyOn 𝓈s $ \ s 𝓈 →
        case 𝑠 ⋕? s of
          None   → 𝓈
          Some n →
            let GDSubst ρ es ι = 𝓈
                ρ'             = ρ+n
                es'            = mapOn es $ introSubstElem s 𝑠
            in GDSubst ρ' es' ι
      esᴳ' = mapOn esᴳ $ \ (𝑠' :* ueO) → (𝑠'+𝑠) :* ueO
  in Subst esᴳ' 𝓈s'

𝓈introG ∷ s₂ ⇰ ℕ64 → Subst s₁ s₂ e
𝓈introG 𝑠 = Subst null $ mapOn 𝑠 $ GDSubst 0 null ∘ intΩ64

𝓈sbindsG ∷ s₂ ⇰ 𝕍 e → Subst s₁ s₂ e
𝓈sbindsG ess = Subst null $ mapOn ess $ \ es →
  let ℯs = map (Val_GSE null ∘ const ∘ return) es
      ι  = neg $ intΩ64 $ csize es
  in GDSubst zero ℯs ι

𝓈sbindG ∷ (Ord s₂) ⇒ s₂ → e → Subst s₁ s₂ e
𝓈sbindG s e = 𝓈sbindsG $ s ↦ single e

𝓈gbindsG ∷ s₁ ⇰ e → Subst s₁ s₂ e
𝓈gbindsG esᴳ = Subst (map ((:*) null ∘ const ∘ return) esᴳ) null

𝓈gbindG ∷ (Ord s₁) ⇒ s₁ → e → Subst s₁ s₂ e
𝓈gbindG s e = 𝓈gbindsG $ s ↦ e

substElem 
  ∷ (Ord s₂) 
  ⇒ s₂ 
  → (Subst s₁ s₂ e → s₂ ⇰ ℕ64 → e → 𝑂 e) 
  → Subst s₁ s₂ e 
  → SubstElem s₂ e 
  → SubstElem s₂ e
substElem s esubst 𝓈̂ = 
  let Subst _esᴳ 𝓈s = 𝓈̂
  in 
  case 𝓈s ⋕? s of
    None   → id
    Some 𝓈 → gsubstElem (esubst 𝓈̂) 𝓈

substAppend ∷ 
  (Ord s₁,Ord s₂) 
  ⇒ (Subst s₁ s₂ e → e → 𝑂 e) 
  → Subst s₁ s₂ e 
  → Subst s₁ s₂ e 
  → Subst s₁ s₂ e
substAppend esubst 𝓈̂₂ 𝓈̂₁ =
  let Subst esᴳ₁ 𝓈s₁ = 𝓈̂₁
      Subst esᴳ₂ 𝓈s₂ = 𝓈̂₂
      esᴳ₁' = mapOn esᴳ₁ $ \ (𝑠 :* ueO) → (:*) null $ \ () →
        esubst (substAppend esubst 𝓈̂₂ (𝓈introG 𝑠)) *$ ueO ()
      esᴳ = esᴳ₁' ⩌ esᴳ₂ 
      𝓈s = unionWithKeyOn 𝓈s₂ 𝓈s₁ $ \ s 𝓈₂@(GDSubst ρ̇₂ es₂ ι₂) 𝓈₁@(GDSubst ρ̇₁ es₁ ι₁) →
        if
        | isNullGDSubst 𝓈₁ → 𝓈₂
        | isNullGDSubst 𝓈₂ → 𝓈₁
        | otherwise →
            let 𝔰₁ = intΩ64 $ csize es₁
                𝔰₂ = intΩ64 $ csize es₂
                ρ₁  = intΩ64 ρ̇₁
                ρ₂  = intΩ64 ρ̇₂
                ρ̇   = ρ̇₁⊓ρ̇₂
                ρ   = intΩ64 ρ̇
                ι   = ι₁+ι₂
                𝔰  = ((ρ₁+𝔰₁)⊔(ρ₂+𝔰₂-ι₁))-ρ
                δ₂  = ρ
                sub = substElem s $ \ 𝓈 𝑠 → esubst $ substAppend esubst (𝓈introG 𝑠) 𝓈
                es = vecF (natΩ64 𝔰) $ \ ṅ → 
                  let n = intΩ64 ṅ + δ₂ in 
                  if
                  | n < ρ₁⊓(ρ₂+𝔰₂) → es₂ ⋕! natΩ64 (n-ρ₂)
                  | n < ρ₁         → Var_GSE $ natΩ64 $ n+ι₂
                  | n < ρ₁+𝔰₁      → sub 𝓈̂₂ $ es₁ ⋕! natΩ64 (n-ρ₁)
                  | n < ρ₂-ι₁      → Var_GSE $ natΩ64 $ n+ι₁
                  | n < ρ₂+𝔰₂-ι₁   → es₂ ⋕! natΩ64 (n+ι₁-ρ₂)
                  | otherwise      → error "bad"
            in
            GDSubst ρ̇ es ι
  in Subst esᴳ 𝓈s

-- ====== --
-- SUBSTY --
-- ====== --

newtype SubstT s₁ s₂ e a = SubstT { unSubstT ∷ UContT (ReaderT (Subst s₁ s₂ e) (FailT ID)) a }
  deriving
  ( Return,Bind,Functor,Monad
  , MonadUCont
  , MonadReader (Subst s₁ s₂ e)
  , MonadFail
  )

runSubstT ∷ Subst s₁ s₂ e → SubstT s₁ s₂ e a → 𝑂 a
runSubstT γ = unID ∘ unFailT ∘ runReaderT γ ∘ evalUContT ∘ unSubstT

class Substy s₁ s₂ e a | a→s₁,a→s₂,a→e where
  substy ∷ a → SubstT s₁ s₂ e a

subst ∷ (Substy s₁ s₂ e a) ⇒ Subst s₁ s₂ e → a → 𝑂 a
subst 𝓈 x = runSubstT 𝓈 $ substy x

instance                                    Null   (Subst s₁ s₂ e) where null = Subst null null
instance (Ord s₁,Ord s₂,Substy s₁ s₂ e e) ⇒ Append (Subst s₁ s₂ e) where (⧺)  = substAppend subst
instance (Ord s₁,Ord s₂,Substy s₁ s₂ e e) ⇒ Monoid (Subst s₁ s₂ e)

substyDBdrG ∷ (Ord s₂) ⇒ s₂ → SubstT s₁ s₂ e ()
substyDBdrG s = umodifyEnv $ 𝓈shiftG $ s ↦ 1

substyNBdrG ∷ (Ord s₂) ⇒ (𝕏 → s₂) → 𝕏 → SubstT s₁ s₂ e ()
substyNBdrG 𝒸 x = umodifyEnv $ 𝓈shiftG $ 𝒸 x ↦ 1

substyVarG ∷ (Ord s₂,Substy s₁ s₂ e e) ⇒ (ℕ64 → e) → s₂ → ℕ64 → SubstT s₁ s₂ e e
substyVarG 𝓋 s n = do
  𝓈s ← askL substScopedL
  case 𝓈s ⋕? s of
    None → return $ 𝓋 n
    Some 𝓈 → case gsubstVar 𝓈 n of
      Var_GSE n' → return $ 𝓋 n'
      Val_GSE 𝑠 ueO → failEff $ subst (𝓈introG 𝑠) *$ ueO ()

substyDVarG ∷ (Ord s₂,Substy s₁ s₂ e e) ⇒ s₂ → (ℕ64 → e) → ℕ64 → SubstT s₁ s₂ e e
substyDVarG s 𝓋 = substyVarG 𝓋 s

substyNVarG ∷ (Ord s₂,Substy s₁ s₂ e e) ⇒ (𝕏 → s₂) → (ℕ64 → e) → 𝕏 → ℕ64 → SubstT s₁ s₂ e e
substyNVarG 𝒸 𝓋 x = substyVarG 𝓋 $ 𝒸 x

substyGVarG ∷ (Ord s₁,Substy s₁ s₂ e e) ⇒ (𝕏 → s₁) → (𝕏 → e) → 𝕏 → SubstT s₁ s₂ e e
substyGVarG 𝒸 𝓋 x = do
  gsᴱ ← askL substGlobalL
  case gsᴱ ⋕? 𝒸 x of
    None → return $ 𝓋 x
    Some (𝑠 :* ueO) → failEff $ subst (𝓈introG 𝑠) *$ ueO ()

--------------------
-- Standard Scope --
--------------------

data 𝔖 s = 
    Dbr_𝔖 s
  | Nmd_𝔖 s 𝕏
  deriving (Eq,Ord,Show)
makePrettyUnion ''𝔖

substyDBdr ∷ (Ord s) ⇒ s → SubstT 𝕏 (𝔖 s) e ()
substyDBdr s = substyDBdrG $ Dbr_𝔖 s

substyNBdr ∷ (Ord s) ⇒ s → 𝕏 → SubstT 𝕏 (𝔖 s) e ()
substyNBdr s = substyNBdrG $ Nmd_𝔖 s

substyDVar ∷ (Ord s,Substy 𝕏 (𝔖 s) e e) ⇒ s → (ℕ64 → e) → ℕ64 → SubstT 𝕏 (𝔖 s) e e
substyDVar s = substyDVarG $ Dbr_𝔖 s

substyNVar ∷ (Ord s,Substy 𝕏 (𝔖 s) e e) ⇒ s → (ℕ64 → e) → 𝕏 → ℕ64 → SubstT 𝕏 (𝔖 s) e e
substyNVar s = substyNVarG $ Nmd_𝔖 s

substyGVar ∷ (Substy 𝕏 (𝔖 s) e e) ⇒ (𝕏 → e) → 𝕏 → SubstT 𝕏 (𝔖 s) e e
substyGVar = substyGVarG id 

substy𝕐 ∷ (Ord s,Substy 𝕏 (𝔖 s) e e) ⇒ s → (𝕐 → e) → 𝕐 → SubstT 𝕏 (𝔖 s) e e
substy𝕐 s 𝓋 = \case
  DVar n → substyDVar s (𝓋 ∘ DVar) n
  NVar n x → substyNVar s (𝓋 ∘ flip NVar x) x n
  GVar x → substyGVar (𝓋 ∘ GVar) x

𝓈sdshift ∷ (Ord s) ⇒ s ⇰ ℕ64 → Subst 𝕏 (𝔖 s) e → Subst 𝕏 (𝔖 s) e
𝓈sdshift = 𝓈shiftG ∘ assoc ∘ map (mapFst Dbr_𝔖) ∘ iter

𝓈snshift ∷ (Ord s) ⇒ s ⇰ 𝕏 ⇰ ℕ64 → Subst 𝕏 (𝔖 s) e → Subst 𝕏 (𝔖 s) e
𝓈snshift 𝑠 = 𝓈shiftG $ assoc $ do
  s :* xns ← iter 𝑠
  x :* n ← iter xns
  return $ (Nmd_𝔖 s x) :* n

𝓈sdintro ∷ (Ord s) ⇒ s ⇰ ℕ64 → Subst 𝕏 (𝔖 s) e
𝓈sdintro = 𝓈introG ∘ assoc ∘ map (mapFst Dbr_𝔖) ∘ iter

𝓈snintro ∷ (Ord s) ⇒ s ⇰ 𝕏 ⇰ ℕ64 → Subst 𝕏 (𝔖 s) e
𝓈snintro 𝑠 = 𝓈introG $ assoc $ do
  s :* xns ← iter 𝑠
  x :* n ← iter xns
  return $ (Nmd_𝔖 s x) :* n

𝓈sdbinds ∷ (Ord s) ⇒ s ⇰ 𝕍 e → Subst 𝕏 (𝔖 s) e
𝓈sdbinds = 𝓈sbindsG ∘ assoc ∘ map (mapFst Dbr_𝔖) ∘ iter

𝓈sdbind ∷ (Ord s) ⇒ s → e → Subst 𝕏 (𝔖 s) e
𝓈sdbind s e = 𝓈sdbinds $ s ↦ single e

𝓈snbinds ∷ (Ord s) ⇒ s ⇰ 𝕏 ⇰ 𝕍 e → Subst 𝕏 (𝔖 s) e
𝓈snbinds 𝑠 = 𝓈sbindsG $ assoc $ do
  s :* xess ← iter 𝑠
  x :* es ← iter xess
  return $ (Nmd_𝔖 s x) :* es

𝓈snbind ∷ (Ord s) ⇒ s → 𝕏 → e → Subst 𝕏 (𝔖 s) e
𝓈snbind s x e = 𝓈snbinds $ s ↦ x ↦ single e

𝓈sgbinds ∷ (Ord s) ⇒ 𝕏 ⇰ e → Subst 𝕏 (𝔖 s) e
𝓈sgbinds = 𝓈gbindsG

𝓈sgbind ∷ (Ord s) ⇒ 𝕏 → e → Subst 𝕏 (𝔖 s) e
𝓈sgbind x e = 𝓈sgbinds $ x ↦ e

𝓈dshift ∷ ℕ64 → Subst 𝕏 (𝔖 ()) e → Subst 𝕏 (𝔖 ()) e
𝓈dshift = 𝓈sdshift ∘ (↦) ()

𝓈nshift ∷ 𝕏 ⇰ ℕ64 → Subst 𝕏 (𝔖 ()) e → Subst 𝕏 (𝔖 ()) e
𝓈nshift = 𝓈snshift ∘ (↦) ()

𝓈dintro ∷ ℕ64 → Subst 𝕏 (𝔖 ()) e
𝓈dintro = 𝓈sdintro ∘ (↦) ()

𝓈nintro ∷ 𝕏 ⇰ ℕ64 → Subst 𝕏 (𝔖 ()) e
𝓈nintro = 𝓈snintro ∘ (↦) ()

𝓈dbinds ∷ 𝕍 e → Subst 𝕏 (𝔖 ()) e
𝓈dbinds = 𝓈sdbinds ∘ (↦) ()

𝓈dbind ∷ e → Subst 𝕏 (𝔖 ()) e
𝓈dbind = 𝓈sdbind ()

𝓈nbinds ∷ 𝕏 ⇰ 𝕍 e → Subst 𝕏 (𝔖 ()) e
𝓈nbinds = 𝓈snbinds ∘ (↦) ()

𝓈nbind ∷ 𝕏 → e → Subst 𝕏 (𝔖 ()) e
𝓈nbind = 𝓈snbind ()

𝓈gbinds ∷ 𝕏 ⇰ e → Subst 𝕏 (𝔖 ()) e
𝓈gbinds = 𝓈sgbinds

𝓈gbind ∷ 𝕏 → e → Subst 𝕏 (𝔖 ()) e
𝓈gbind = 𝓈sgbind

-----------
-- Fuzzy --
-----------

fuzzyGSubstElem ∷ FuzzyM s → FuzzyM a → FuzzyM (GSubstElem s a)
fuzzyGSubstElem sM xM = rchoose
    [ \ () → Var_GSE ^$ fuzzy
    , \ () → do
        𝑠 ← sM
        e ← xM
        return $ Val_GSE 𝑠 $ const $ return e
    ]

fuzzyGDSubst ∷ FuzzyM s → FuzzyM a → FuzzyM (GDSubst s a)
fuzzyGDSubst sM xM = do
  ρ ← fuzzy
  𝔰 ← fuzzy
  es ← mapMOn (vecF 𝔰 id) $ const $ fuzzyGSubstElem sM xM
  ι ← randr (neg $ intΩ64 𝔰) $ intΩ64 𝔰
  return $ GDSubst ρ es ι

fuzzy𝔖 ∷ FuzzyM s → FuzzyM (𝔖 s)
fuzzy𝔖 sM = rchoose
  [ \ () → Dbr_𝔖 ^$ sM
  , \ () → do
      s ← sM
      x ← fuzzy
      return $ Nmd_𝔖 s x
  ]

instance (Fuzzy s,Fuzzy a) ⇒ Fuzzy (GSubstElem s a) where fuzzy = fuzzyGSubstElem fuzzy fuzzy
instance (Fuzzy s,Fuzzy a) ⇒ Fuzzy (GDSubst s a) where fuzzy = fuzzyGDSubst fuzzy fuzzy
instance (Fuzzy s) ⇒ Fuzzy (𝔖 s) where fuzzy = fuzzy𝔖 fuzzy

instance (Ord s₂,Fuzzy s₂,Fuzzy a) ⇒ Fuzzy (Subst s₁ s₂ a) where fuzzy = Subst null ^$ fuzzy

