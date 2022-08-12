module UVMHS.Lib.Substitution where

import UVMHS.Core
import UVMHS.Lib.Annotated
import UVMHS.Lib.Variables
import UVMHS.Lib.Testing
import UVMHS.Lib.Rand
import UVMHS.Lib.Pretty

import UVMHS.Lang.ULCD

-- ===================== --
-- DEBRUIJN SUBSTITUTION --
-- ===================== --

-- ℯ ⩴ i | ⟨ι,e⟩
data DSubstElem e = 
    Var_DSE ℕ64
  | Trm_DSE ℕ64 (() → 𝑂 e)
  deriving (Eq)

instance (Pretty a) ⇒ Pretty (DSubstElem a) where
  pretty = \case
    Var_DSE i → pretty $ DVar i
    Trm_DSE ι e → concat
      [ ppPun $ show𝕊 ι
      , ppPun "⇈"
      , ifNone (ppPun "bu") $ pretty ^$ e ()
      ]

-- 𝓈 ⩴ ⟨ρ,es,ι⟩ 
-- INVARIANT: |es| + ι ≥ 0
data DSubst e = DSubst
  { dsubstShift ∷ ℕ64
  , dsubstElems ∷ 𝕍 (DSubstElem e)
  , dsubstIntro ∷ ℤ64
  } deriving (Eq)
makeLenses ''DSubst
makePrettyRecord ''DSubst

introDSubstElem ∷ ℤ64 → DSubstElem e → DSubstElem e
introDSubstElem ι = \case
  Var_DSE i → Var_DSE $ natΩ64 $ intΩ64 i+ι
  Trm_DSE ι' ueO → Trm_DSE (natΩ64 $ intΩ64 ι'+ι) ueO

𝓈shift ∷ ℕ64 → DSubst e → DSubst e
𝓈shift n (DSubst ρ es ι) =
  let ρ'   = ρ+n
      es' = mapOn es $ introDSubstElem $ intΩ64 n
  in DSubst ρ' es' ι

𝓈intro ∷ ℕ64 → DSubst e
𝓈intro ι = DSubst zero null $ intΩ64 ι

𝓈binds ∷ 𝕍 e → DSubst e
𝓈binds es = DSubst zero (map (Trm_DSE 0 ∘ const ∘ return) es) $ neg $ intΩ64 $ csize es

𝓈bind ∷ e → DSubst e
𝓈bind = 𝓈binds ∘ single

-- 𝓈 ≜ ⟨ρ,es,ι⟩
-- 𝔰 ≜ |es|
-- cases (disjoint):
--   |       i < ρ   ⇒ i
--   |   ρ ≤ i < ρ+𝔰 ⇒ es[i-ρ]
--   | ρ+𝔰 ≤ i       ⇒ i+ι
-- cases (sequential):
--   | i < ρ   ⇒ i
--   | i < ρ+𝔰 ⇒ es[i-ρ]
--   | ⊤       ⇒ i+ι
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
dsubstVar ∷ DSubst e → ℕ64 → DSubstElem e
dsubstVar (DSubst ρ̇ es ι) ṅ =
  let 𝔰̇ = csize es
      n = intΩ64 ṅ
  in 
  if
  | ṅ < ρ̇      → Var_DSE ṅ
  | ṅ < 𝔰̇+ρ̇   → es ⋕! (ṅ-ρ̇)
  | otherwise  → Var_DSE $ natΩ64 $ n+ι

-- esubst(ι,σ,e) ≡ σ(ι(e))
dsubstElem ∷ (ℕ64 → DSubst e → e → 𝑂 e) → DSubst e → DSubstElem e → DSubstElem e
dsubstElem esubst 𝓈 = \case
  Var_DSE n → dsubstVar 𝓈 n
  Trm_DSE ι ueO → Trm_DSE 0 $ \ () → esubst ι 𝓈 *$ ueO ()

-----------------
-- COMPOSITION --
-----------------

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

dsubstAppend ∷ (Pretty e) ⇒ (ℕ64 → DSubst e → e → 𝑂 e) → DSubst e → DSubst e → DSubst e
dsubstAppend esubst 𝓈₂@(DSubst ρ̇₂ es₂ ι₂) (DSubst ρ̇₁ es₁ ι₁) =
  let 𝔰₁ = intΩ64 $ csize es₁
      𝔰₂ = intΩ64 $ csize es₂
      ρ₁  = intΩ64 ρ̇₁
      ρ₂  = intΩ64 ρ̇₂
      ρ̇   = ρ̇₁⊓ρ̇₂
      ρ   = intΩ64 ρ̇
      ι   = ι₁+ι₂
      𝔰  = ((ρ₁+𝔰₁)⊔(ρ₂+𝔰₂-ι₁))-ρ
      δ₂  = ρ
      sub = dsubstElem esubst
      es = vecF (natΩ64 𝔰) $ \ ṅ → 
        let n = intΩ64 ṅ + δ₂ in 
        if
        | n < ρ₁⊓(ρ₂+𝔰₂) → es₂ ⋕! natΩ64 (n-ρ₂)
        | n < ρ₁         → Var_DSE $ natΩ64 $ n+ι₂
        | n < ρ₁+𝔰₁      → sub 𝓈₂ $ es₁ ⋕! natΩ64 (n-ρ₁)
        | n < ρ₂-ι₁      → Var_DSE $ natΩ64 $ n+ι₁
        | n < ρ₂+𝔰₂-ι₁   → es₂ ⋕! natΩ64 (n+ι₁-ρ₂)
        | otherwise      → error "bad"
  in
  DSubst ρ̇ es ι

-- ====== --
-- SUBSTY --
-- ====== --

newtype SubstT e a = SubstT { unSubstT ∷ UContT (ReaderT (DSubst e) (FailT ID)) a }
  deriving
  ( Return,Bind,Functor,Monad
  , MonadUCont
  , MonadReader (DSubst e)
  , MonadFail
  )

runSubstT ∷ DSubst e → SubstT e a → 𝑂 a
runSubstT γ = unID ∘ unFailT ∘ runReaderT γ ∘ evalUContT ∘ unSubstT

class Substy e a | a→e where
  substy ∷ a → SubstT e a

subst ∷ (Substy e a) ⇒ DSubst e → a → 𝑂 a
subst 𝓈 x = runSubstT 𝓈 $ substy x

instance                Null (DSubst e)   where null = DSubst zero null 0
instance (Pretty e,Substy e e) ⇒ Append (DSubst e) where 
  (⧺) = dsubstAppend $ \ i 𝓈 → 
    subst 𝓈 *∘ subst (𝓈intro i)
instance (Pretty e,Substy e e) ⇒ Monoid (DSubst e)

substyDBdr ∷ SubstT e ()
substyDBdr = umodifyEnv $ 𝓈shift 1

substyDVar ∷ (Substy e e) ⇒ (ℕ64 → e) → ℕ64 → SubstT e e
substyDVar 𝓋 i = do
  𝓈 ← ask
  case dsubstVar 𝓈 i of
    Var_DSE i' → return $ 𝓋 i'
    Trm_DSE ι ueO → failEff $ subst (𝓈intro ι) *$ ueO ()


-- ======== --
-- FOR ULCD --
-- ======== --

instance Substy (ULCDExp 𝒸) (ULCDExp 𝒸) where
  substy = pipe unULCDExp $ \ (𝐴 𝒸 e₀) → ULCDExp ^$ case e₀ of
    Var_ULCD y → case y of
      DVar i → unULCDExp ^$ substyDVar (ULCDExp ∘ 𝐴 𝒸 ∘ Var_ULCD ∘ DVar) i
      _      → return $ 𝐴 𝒸 $ Var_ULCD y
    Lam_ULCD e → ureset $ do
      substyDBdr
      e' ← substy e
      return $ 𝐴 𝒸 $ Lam_ULCD e'
    App_ULCD e₁ e₂ → do
      e₁' ← substy e₁
      e₂' ← substy e₂
      return $ 𝐴 𝒸 $ App_ULCD e₁' e₂'

prandDVar ∷ ℕ64 → ℕ64 → State RG ℕ64
prandDVar nˢ nᵇ = prandr 0 $ nᵇ + nˢ

prandNVar ∷ ℕ64 → State RG 𝕏
prandNVar nˢ = flip 𝕏 "x" ∘ Some ^$ prandr 0 nˢ

-- TODO: be aware of named scope
prandVar ∷ ℕ64 → ℕ64 → State RG 𝕐
prandVar nˢ nᵇ = mjoin $ prchoose
  [ \ () → DVar ^$ prandDVar nˢ nᵇ
  , \ () → NVar 0 ^$ prandNVar nˢ
  ]

prandULCDExp ∷ ℕ64 → ℕ64 → ℕ64 → State RG ULCDExpR
prandULCDExp nˢ nᵇ nᵈ = ULCDExp ∘ 𝐴 () ^$ mjoin $ prwchoose
    [ (2 :*) $ \ () → do
        Var_ULCD ^$ prandVar nˢ nᵇ
    , (nᵈ :*) $ \ () → do
        Lam_ULCD ^$ prandULCDExp nˢ (nᵇ + 1) $ nᵈ - 1
    , (nᵈ :*) $ \ () → do
        e₁ ← prandULCDExp nˢ nᵇ $ nᵈ - 1
        e₂ ← prandULCDExp nˢ nᵇ $ nᵈ - 1
        return $ App_ULCD e₁ e₂
    ]

instance Rand ULCDExpR where prand = flip prandULCDExp zero

prandSubstElem ∷ (Rand a) ⇒ ℕ64 → ℕ64 → State RG (DSubstElem a)
prandSubstElem nˢ nᵈ = mjoin $ prchoose
  [ \ () → do
      i ← prand @ℕ64 nˢ nᵈ
      return $ Var_DSE i
  , \ () → do
      ι ← prand @ℕ64 nˢ nᵈ
      e ← prand nˢ nᵈ
      return $ Trm_DSE ι $ const $ return e
  ]

instance (Rand a) ⇒ Rand (DSubstElem a) where prand = prandSubstElem

prandDSubst ∷ (Rand a) ⇒ ℕ64 → ℕ64 → State RG (DSubst a)
prandDSubst nˢ nᵈ = do
  ρ ← prand nˢ nᵈ
  𝔰 ← prandr zero nˢ
  es ← mapMOn (vecF 𝔰 id) $ const $ prand nˢ nᵈ
  ι ← prandr (neg $ intΩ64 𝔰) $ intΩ64 nˢ
  return $ DSubst ρ es ι

instance (Rand a) ⇒  Rand (DSubst a) where prand = prandDSubst

-- basic --

𝔱 "subst:id" [| subst null [ulcd| λ → 0   |] |] [| Some [ulcd| λ → 0   |] |]
𝔱 "subst:id" [| subst null [ulcd| λ → 1   |] |] [| Some [ulcd| λ → 1   |] |]
𝔱 "subst:id" [| subst null [ulcd| λ → 2   |] |] [| Some [ulcd| λ → 2   |] |]
𝔱 "subst:id" [| subst null [ulcd| λ → 0 2 |] |] [| Some [ulcd| λ → 0 2 |] |]

𝔱 "subst:intro" [| subst (𝓈intro 1) [ulcd| λ → 0   |] |] [| Some [ulcd| λ → 0   |] |]
𝔱 "subst:intro" [| subst (𝓈intro 1) [ulcd| λ → 1   |] |] [| Some [ulcd| λ → 2   |] |]
𝔱 "subst:intro" [| subst (𝓈intro 1) [ulcd| λ → 2   |] |] [| Some [ulcd| λ → 3   |] |]
𝔱 "subst:intro" [| subst (𝓈intro 1) [ulcd| λ → 0 2 |] |] [| Some [ulcd| λ → 0 3 |] |]

𝔱 "subst:intro" [| subst (𝓈intro 2) [ulcd| λ → 0   |] |] [| Some [ulcd| λ → 0   |] |]
𝔱 "subst:intro" [| subst (𝓈intro 2) [ulcd| λ → 1   |] |] [| Some [ulcd| λ → 3   |] |]
𝔱 "subst:intro" [| subst (𝓈intro 2) [ulcd| λ → 2   |] |] [| Some [ulcd| λ → 4   |] |]
𝔱 "subst:intro" [| subst (𝓈intro 2) [ulcd| λ → 0 2 |] |] [| Some [ulcd| λ → 0 4 |] |]

𝔱 "subst:bind" [| subst (𝓈bind [ulcd| λ → 0 |]) [ulcd| λ → 0 |] |] [| Some [ulcd| λ → 0     |] |]
𝔱 "subst:bind" [| subst (𝓈bind [ulcd| λ → 1 |]) [ulcd| λ → 0 |] |] [| Some [ulcd| λ → 0     |] |]
𝔱 "subst:bind" [| subst (𝓈bind [ulcd| λ → 0 |]) [ulcd| λ → 1 |] |] [| Some [ulcd| λ → λ → 0 |] |]
𝔱 "subst:bind" [| subst (𝓈bind [ulcd| λ → 1 |]) [ulcd| λ → 1 |] |] [| Some [ulcd| λ → λ → 2 |] |]

𝔱 "subst:shift" [| subst (𝓈shift 1 $ 𝓈bind [ulcd| λ → 0 |]) [ulcd| λ → 0 |] |] 
                 [| Some [ulcd| λ → 0 |] |]
𝔱 "subst:shift" [| subst (𝓈shift 1 $ 𝓈bind [ulcd| λ → 1 |]) [ulcd| λ → 0 |] |] 
                 [| Some [ulcd| λ → 0 |] |]
𝔱 "subst:shift" [| subst (𝓈shift 1 $ 𝓈bind [ulcd| λ → 0 |]) [ulcd| λ → 1 |] |] 
                 [| Some [ulcd| λ → 1 |] |]
𝔱 "subst:shift" [| subst (𝓈shift 1 $ 𝓈bind [ulcd| λ → 1 |]) [ulcd| λ → 1 |] |] 
                 [| Some [ulcd| λ → 1 |] |]
𝔱 "subst:shift" [| subst (𝓈shift 1 $ 𝓈bind [ulcd| λ → 2 |]) [ulcd| λ → 0 |] |] 
                 [| Some [ulcd| λ → 0 |] |]
𝔱 "subst:shift" [| subst (𝓈shift 1 $ 𝓈bind [ulcd| λ → 2 |]) [ulcd| λ → 1 |] |] 
                 [| Some [ulcd| λ → 1 |] |]
𝔱 "subst:shift" [| subst (𝓈shift 1 $ 𝓈bind [ulcd| λ → 1 |]) [ulcd| λ → 2 |] |] 
                 [| Some [ulcd| λ → λ → 3 |] |]
𝔱 "subst:shift" [| subst (𝓈shift 1 $ 𝓈bind [ulcd| λ → 2 |]) [ulcd| λ → 2 |] |] 
                 [| Some [ulcd| λ → λ → 4 |] |]

-- append --

𝔱 "subst:⧺" [| subst null            [ulcd| λ → 0 |] |] [| Some [ulcd| λ → 0 |] |]
𝔱 "subst:⧺" [| subst (null ⧺ null)   [ulcd| λ → 0 |] |] [| Some [ulcd| λ → 0 |] |]
𝔱 "subst:⧺" [| subst (𝓈shift 1 null) [ulcd| λ → 0 |] |] [| Some [ulcd| λ → 0 |] |]
𝔱 "subst:⧺" [| subst (𝓈shift 2 null) [ulcd| λ → 0 |] |] [| Some [ulcd| λ → 0 |] |]

𝔱 "subst:⧺" [| subst null          [ulcd| λ → 1 |] |] [| Some [ulcd| λ → 1 |] |]
𝔱 "subst:⧺" [| subst (null ⧺ null) [ulcd| λ → 1 |] |] [| Some [ulcd| λ → 1 |] |]

𝔱 "subst:⧺" [| subst (𝓈intro 1)               [ulcd| λ → 0 |] |] [| Some [ulcd| λ → 0 |] |]
𝔱 "subst:⧺" [| subst (null ⧺ 𝓈intro 1 ⧺ null) [ulcd| λ → 0 |] |] [| Some [ulcd| λ → 0 |] |]

𝔱 "subst:⧺" [| subst (𝓈intro 1)               [ulcd| λ → 1 |] |] [| Some [ulcd| λ → 2 |] |]
𝔱 "subst:⧺" [| subst (null ⧺ 𝓈intro 1 ⧺ null) [ulcd| λ → 1 |] |] [| Some [ulcd| λ → 2 |] |]

𝔱 "subst:⧺" [| subst (𝓈bind [ulcd| λ → 0 |]) [ulcd| λ → 1 |] |] 
            [| Some [ulcd| λ → λ → 0 |] |]
𝔱 "subst:⧺" [| subst (null ⧺ 𝓈bind [ulcd| λ → 0 |] ⧺ null) [ulcd| λ → 1 |] |] 
            [| Some [ulcd| λ → λ → 0 |] |]

𝔱 "subst:⧺" [| subst (𝓈intro 2) [ulcd| λ → 1 |] |]            [| Some [ulcd| λ → 3 |] |]
𝔱 "subst:⧺" [| subst (𝓈intro 1 ⧺ 𝓈intro 1) [ulcd| λ → 1 |] |] [| Some [ulcd| λ → 3 |] |]

𝔱 "subst:⧺" [| subst (𝓈bind [ulcd| λ → 0 |]) [ulcd| λ → 1 |] |] 
            [| Some [ulcd| λ → λ → 0 |] |]
𝔱 "subst:⧺" [| subst (𝓈shift 1 (𝓈bind [ulcd| λ → 0 |]) ⧺ 𝓈intro 1) [ulcd| λ → 1 |] |] 
            [| Some [ulcd| λ → λ → 0 |] |]

𝔱 "subst:⧺" [| subst (𝓈intro 1 ⧺ 𝓈bind [ulcd| 1 |]) [ulcd| 0 (λ → 2) |] |] 
            [| Some [ulcd| 2 (λ → 2) |] |]
𝔱 "subst:⧺" [| subst (𝓈shift 1 (𝓈bind [ulcd| 1 |]) ⧺ 𝓈intro 1) [ulcd| 0 (λ → 2) |] |] 
            [| Some [ulcd| 2 (λ → 2) |] |]

𝔱 "subst:⧺" [| subst (𝓈intro 1) *$ subst (𝓈shift 1 null) [ulcd| 0 |] |]
            [| subst (𝓈intro 1 ⧺ 𝓈shift 1 null) [ulcd| 0 |] |]

𝔱 "subst:⧺" [| subst (𝓈bind [ulcd| 1 |]) *$ subst (𝓈shift 1 (𝓈intro 1)) [ulcd| 0 |] |]
            [| subst (𝓈bind [ulcd| 1 |] ⧺ 𝓈shift 1 (𝓈intro 1)) [ulcd| 0 |] |]

𝔱 "subst:⧺" [| subst (𝓈shift 1 (𝓈bind [ulcd| 1 |])) *$ subst (𝓈shift 1 null) [ulcd| 1 |] |]
            [| subst (𝓈shift 1 (𝓈bind [ulcd| 1 |]) ⧺ 𝓈shift 1 null) [ulcd| 1 |] |]

𝔱 "subst:⧺" [| subst (𝓈shift 1 (𝓈bind [ulcd| 3 |]) ⧺ null) [ulcd| 0 |] |]
            [| subst (𝓈shift 1 (𝓈bind [ulcd| 3 |])) [ulcd| 0 |] |]

-- fuzzing --

𝔣 "zzz:subst:hom:refl" 100 
  [| do e ← randSml @ULCDExpR
        return $ e
  |]
  [| \ e → 
       subst null e ≡ Some e
  |]

𝔣 "zzz:subst:hom:⧺" 100
  [| do 𝓈₁ ← randSml @(DSubst ULCDExpR)
        𝓈₂ ← randSml @(DSubst ULCDExpR)
        e ← randSml @ULCDExpR
        return $ 𝓈₁ :* 𝓈₂ :* e
  |]
  [| \ (𝓈₁ :* 𝓈₂ :* e) → 
       subst (𝓈₁ ⧺ 𝓈₂) e ≡ (subst 𝓈₁ *$ subst 𝓈₂ e)
  |]

𝔣 "zzz:subst:lunit:⧺" 100 
  [| do 𝓈 ← randSml @(DSubst ULCDExpR)
        e ← randSml @ULCDExpR
        return $ 𝓈 :* e
  |]
  [| \ (𝓈 :* e) → 
       subst (null ⧺ 𝓈) e ≡ subst 𝓈 e
  |]

𝔣 "zzz:subst:runit:⧺" 100 
  [| do 𝓈 ← randSml @(DSubst ULCDExpR)
        e ← randSml @ULCDExpR
        return $ 𝓈 :* e
  |]
  [| \ (𝓈 :* e) → 
       subst (𝓈 ⧺ null) e ≡ subst 𝓈 e
  |]

𝔣 "zzz:subst:trans:⧺" 100 
  [| do 𝓈₁ ← randSml @(DSubst ULCDExpR)
        𝓈₂ ← randSml @(DSubst ULCDExpR)
        𝓈₃ ← randSml @(DSubst ULCDExpR)
        e ← randSml @ULCDExpR
        return $ 𝓈₁ :* 𝓈₂ :* 𝓈₃ :* e
  |]
  [| \ (𝓈₁ :* 𝓈₂ :* 𝓈₃ :* e) → 
       subst ((𝓈₁ ⧺ 𝓈₂) ⧺ 𝓈₃) e ≡ subst (𝓈₁ ⧺ (𝓈₂ ⧺ 𝓈₃)) e 
  |]

𝔣 "zzz:subst:unit:shift" 100
  [| do i ← randSml @ℕ64
        e ← randSml @ULCDExpR
        return $ i :* e
  |]
  [| \ (i :* e) → subst (𝓈shift i null) e ≡ Some e 
  |]

𝔣 "zzz:subst:unit:bind∘intro" 100
  [| do e₁ ← randSml @ULCDExpR
        e₂ ← randSml @ULCDExpR
        return $ e₁ :* e₂
  |]
  [| \ (e₁ :* e₂) → (subst (𝓈bind e₁) *$ subst (𝓈intro 1) e₂) ≡ Some e₂
  |]

𝔣 "zzz:subst:commute:intro∘bind" 100
  [| do e₁ ← randSml @ULCDExpR
        e₂ ← randSml @ULCDExpR
        return $ e₁ :* e₂
  |]
  [| \ (e₁ :* e₂) → 
       (subst (𝓈intro 1) *$ subst (𝓈bind e₁) e₂)
       ≡ 
       (subst (𝓈shift 1 $ 𝓈bind e₁) *$ subst (𝓈intro 1) e₂)
  |]

𝔣 "zzz:subst:dist:shift/⧺" 100 
  [| do n ← randSml @ℕ64
        𝓈₁ ← randSml @(DSubst ULCDExpR)
        𝓈₂ ← randSml @(DSubst ULCDExpR)
        e ← randSml @ULCDExpR
        return $ n :* 𝓈₁ :* 𝓈₂ :* e
  |]
  [| \ (n :* 𝓈₁ :* 𝓈₂ :* e) → 
       subst (𝓈shift n (𝓈₁ ⧺ 𝓈₂)) e ≡ subst (𝓈shift n 𝓈₁ ⧺ 𝓈shift n 𝓈₂) e 
  |]

buildTests
