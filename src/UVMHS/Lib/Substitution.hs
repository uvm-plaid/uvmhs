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
    Var_DSE ℤ64
  | Trm_DSE ℤ64 (() → 𝑂 e)
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
data DSubst e = DSubst
  { dsubstShift ∷ ℕ64
  , dsubstElems ∷ 𝕍 (DSubstElem e)
  , dsubstIntro ∷ ℤ64
  } deriving (Eq)
makeLenses ''DSubst
makePrettyRecord ''DSubst

introDSubstElem ∷ ℤ64 → DSubstElem e → DSubstElem e
introDSubstElem ι = \case
  Var_DSE i → Var_DSE $ i+ι
  Trm_DSE ι' ueO → Trm_DSE (ι'+ι) ueO

𝓈shift ∷ ℕ64 → DSubst e → DSubst e
𝓈shift n (DSubst ρ es ι) =
  let ρ'   = ρ+n
      es' = mapOn es $ introDSubstElem $ intΩ64 n
  in DSubst ρ' es' ι

𝓈intro ∷ ℤ64 → DSubst e
𝓈intro ι = DSubst zero null ι

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
dsubstVar ∷ DSubst e → ℤ64 → DSubstElem e
dsubstVar (DSubst ρ̇ es ι) i =
  let ρ  = intΩ64 ρ̇
      𝔰 = intΩ64 $ csize es
  in 
  if
  | i < ρ      → Var_DSE i
  | i < 𝔰+ρ   → es ⋕! natΩ64 (i-ρ)
  | otherwise  → Var_DSE $ i+ι

-- esubst(ι,σ,e) ≡ σ(ι(e))
dsubstElem ∷ (ℤ64 → DSubst e → e → 𝑂 e) → DSubst e → DSubstElem e → DSubstElem e
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

dsubstAppend ∷ (Pretty e) ⇒ (ℤ64 → DSubst e → e → 𝑂 e) → DSubst e → DSubst e → DSubst e
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
      es = vecF (natΩ64 𝔰) $ \ n → let i = intΩ64 n + δ₂ in if
        | i < ρ₁⊓(ρ₂+𝔰₂)          → es₂ ⋕! natΩ64 (i-ρ₂)
        | i < ρ₁                   → Var_DSE $ i+ι₂
        | i < ρ₁+𝔰₁               → sub 𝓈₂ $ es₁ ⋕! natΩ64 (i-ρ₁)
        | i < ρ₂-ι₁                → Var_DSE $ i+ι₁
        | i < ρ₂+𝔰₂-ι₁            → es₂ ⋕! natΩ64 (i+ι₁-ρ₂)
        | otherwise                → error "bad"
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

substyDVar ∷ (Substy e e) ⇒ (ℤ64 → e) → ℤ64 → SubstT e e
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

prandDVar ∷ ℕ64 → ℕ64 → State RG ℤ64
prandDVar nˢ nᵇ = prandr 0 $ intΩ64 $ nᵇ + nˢ

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
      i ← intΩ64 ^$ prand @ℕ64 nˢ nᵈ
      return $ Var_DSE i
  , \ () → do
      ι ← intΩ64 ^$ prand @ℕ64 nˢ nᵈ
      e ← prand nˢ nᵈ
      return $ Trm_DSE ι $ const $ return e
  ]

instance (Rand a) ⇒ Rand (DSubstElem a) where prand = prandSubstElem

prandSSubst ∷ (Rand a) ⇒ ℕ64 → ℕ64 → State RG (DSubst a)
prandSSubst nˢ nᵈ = do
  ρ ← prand nˢ nᵈ
  𝔰 ← prandr zero nˢ
  es ← mapMOn (vecF 𝔰 id) $ const $ prand nˢ nᵈ
  ι ← prandr (neg $ intΩ64 𝔰) $ intΩ64 nˢ
  return $ DSubst ρ es ι

instance (Rand a) ⇒  Rand (DSubst a) where prand = prandSSubst

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

𝔣 "zzz:subst:refl:hom" 100 
  [| do e ← randSml @ULCDExpR
        return $ e
  |]
  [| \ e → 
       subst null e ≡ Some e
  |]

𝔣 "zzz:subst:refl/shift:hom" 100
  [| do i ← randSml @ℕ64
        e ← randSml @ULCDExpR
        return $ i :* e
  |]
  [| \ (i :* e) → subst (𝓈shift i null) e ≡ Some e 
  |]

-- 𝔣 "zzz:subst:bind" 100
--   [| do e₁ ← randSml @ULCDExpR
--         e₂ ← randSml @ULCDExpR
--         return $ e₁ :* e₂
--   |]
--   [| \ (e₁ :* e₂) → (subst (𝓈bind e₁) *$ subst (𝓈intro 1) e₂) ≡ Some e₂
--   |]

-- 𝔣 "zzz:subst:commute" 100
--   [| do e₁ ← randSml @ULCDExpR
--         e₂ ← randSml @ULCDExpR
--         return $ e₁ :* e₂
--   |]
--   [| \ (e₁ :* e₂) → 
--        (subst (𝓈intro 1) *$ subst (𝓈bind e₁) e₂)
--        ≡ 
--        (subst (𝓈shift 1 $ 𝓈bind e₁) *$ subst (𝓈intro 1) e₂)
--   |]

𝔣 "zzz:subst:⧺:hom" 100
  [| do 𝓈₁ ← randSml @(DSubst ULCDExpR)
        𝓈₂ ← randSml @(DSubst ULCDExpR)
        e ← randSml @ULCDExpR
        return $ 𝓈₁ :* 𝓈₂ :* e
  |]
  [| \ (𝓈₁ :* 𝓈₂ :* e) → 
       subst (𝓈₁ ⧺ 𝓈₂) e ≡ (subst 𝓈₁ *$ subst 𝓈₂ e)
  |]

-- 𝔣 "zzz:ssubst:⧺:lrefl" 100 
--   [| do 𝓈 ← randSml @(DSubst ULCDExpR)
--         e ← randSml @ULCDExpR
--         return $ 𝓈 :* e
--   |]
--   [| \ (𝓈 :* e) → 
--        subst (null ⧺ 𝓈) e ≡ subst 𝓈 e
--   |]

-- 𝔣 "zzz:ssubst:⧺:rrefl" 100 
--   [| do 𝓈 ← randSml @(SSubst ULCDExpR)
--         e ← randSml @ULCDExpR
--         return $ 𝓈 :* e
--   |]
--   [| \ (𝓈 :* e) → 
--        ssubst (𝓈 ⧺ null) e ≡ ssubst 𝓈 e
--   |]
-- 
-- 𝔣 "zzz:ssubst:⧺:lrefl/shift" 100
--   [| do n ← randSml @ℕ64
--         𝓈 ← randSml @(SSubst ULCDExpR)
--         e ← randSml @ULCDExpR
--         return $ n :* 𝓈 :* e
--   |]
--   [| \ (n :* 𝓈 :* e) → ssubst (𝓈shift n null ⧺ 𝓈) e ≡ ssubst 𝓈 e 
--   |]
-- 
-- 𝔣 "zzz:ssubst:⧺:rrefl/shift" 100
--   [| do n ← randSml @ℕ64
--         𝓈 ← randSml @(SSubst ULCDExpR)
--         e ← randSml @ULCDExpR
--         return $ n :* 𝓈 :* e
--   |]
--   [| \ (n :* 𝓈 :* e) → ssubst (𝓈 ⧺ 𝓈shift n null) e ≡ ssubst 𝓈 e 
--   |]
-- 
-- 𝔣 "zzz:ssubst:⧺:trans" 100 
--   [| do 𝓈₁ ← randSml @(SSubst ULCDExpR)
--         𝓈₂ ← randSml @(SSubst ULCDExpR)
--         𝓈₃ ← randSml @(SSubst ULCDExpR)
--         e ← randSml @ULCDExpR
--         return $ 𝓈₁ :* 𝓈₂ :* 𝓈₃ :* e
--   |]
--   [| \ (𝓈₁ :* 𝓈₂ :* 𝓈₃ :* e) → 
--        ssubst ((𝓈₁ ⧺ 𝓈₂) ⧺ 𝓈₃) e ≡ ssubst (𝓈₁ ⧺ (𝓈₂ ⧺ 𝓈₃)) e 
--   |]
-- 
-- 𝔣 "zzz:ssubst:shift/⧺:shift:dist" 100 
--   [| do n ← randSml @ℕ64
--         𝓈₁ ← randSml @(SSubst ULCDExpR)
--         𝓈₂ ← randSml @(SSubst ULCDExpR)
--         e ← randSml @ULCDExpR
--         return $ n :* 𝓈₁ :* 𝓈₂ :* e
--   |]
--   [| \ (n :* 𝓈₁ :* 𝓈₂ :* e) → 
--        ssubst (𝓈shift n (𝓈₁ ⧺ 𝓈₂)) e ≡ ssubst (𝓈shift n 𝓈₁ ⧺ 𝓈shift n 𝓈₂) e 
--   |]
-- 
-- 𝔣 "zzz:usubst:⧺:hom" 100 
--   [| do 𝓈₁ ← randSml @(USubst ULCDExpR)
--         𝓈₂ ← randSml @(USubst ULCDExpR)
--         e ← randSml @ULCDExpR
--         return $ 𝓈₁ :* 𝓈₂ :* e
--   |]
--   [| \ (𝓈₁ :* 𝓈₂ :* e) → 
--        usubst (𝓈₁ ⧺ 𝓈₂) e ≡ usubst 𝓈₁ (usubst 𝓈₂ e)
--   |]
-- 
-- 𝔣 "zzz:usubst:⧺:lrefl" 100 
--   [| do 𝓈 ← randSml @(USubst ULCDExpR)
--         e ← randSml @ULCDExpR
--         return $ 𝓈 :* e
--   |]
--   [| \ (𝓈 :* e) → 
--        usubst (null ⧺ 𝓈) e ≡ usubst 𝓈 e
--   |]
-- 
-- 𝔣 "zzz:usubst:⧺:rrefl" 100 
--   [| do 𝓈 ← randSml @(USubst ULCDExpR)
--         e ← randSml @ULCDExpR
--         return $ 𝓈 :* e
--   |]
--   [| \ (𝓈 :* e) → 
--        usubst (𝓈 ⧺ null) e ≡ usubst 𝓈 e
--   |]
-- 
-- 𝔣 "zzz:usubst:⧺:trans" 100 
--   [| do 𝓈₁ ← randSml @(USubst ULCDExpR)
--         𝓈₂ ← randSml @(USubst ULCDExpR)
--         𝓈₃ ← randSml @(USubst ULCDExpR)
--         e ← randSml @ULCDExpR
--         return $ 𝓈₁ :* 𝓈₂ :* 𝓈₃ :* e
--   |]
--   [| \ (𝓈₁ :* 𝓈₂ :* 𝓈₃ :* e) → 
--        usubst ((𝓈₁ ⧺ 𝓈₂) ⧺ 𝓈₃) e ≡ usubst (𝓈₁ ⧺ (𝓈₂ ⧺ 𝓈₃)) e 
--   |]
-- 
-- 𝔣 "zzz:ssubst:open∘close" 100 
--   [| do randSml @ULCDExpR
--   |]
--   [| \ e → 
--        ssubst (𝓈open (var "z") ⧺ 𝓈close (var "z")) e ≡ e
--   |]
-- 
-- 𝔣 "zzz:ssubst:close∘open" 100 
--   [| do randSml @ULCDExpR
--   |]
--   [| \ e → 
--        ssubst (𝓈close (var "z") ⧺ 𝓈open (var "z")) e ≡ e
--   |]
buildTests

-- -- substyVar ∷ (Ord s,Monad m) ⇒ (𝕐 → e₂) → (ℤ64 → (𝕏 ⇰ ℤ64) → e₂ → e₂) → s → 𝕐 → SubstT s e₁ e₂ m e₂
-- -- substyVar evar eintro s y = do
-- --   σO ← lup s ^$ askL substEnvSubsL
-- --   case σO of
-- --     None → return $ evar y
-- --     Some σ → do
-- --       𝓋 ← askL substEnvViewL
-- --       case substVar σ y of
-- --         Var_DSE y' → return $ evar y'
-- --         Trm_DSE i' xis e → failEff $ eintro i' xis ^$ 𝓋 e
-- 
-- 
-- 
-- 
-- -- data Subst e = Subst
-- --   { substDShf ∷ ℕ64
-- --   , substNShf ∷ 𝕏 ⇰ ℕ64
-- --   , substDIro ∷ ℤ64
-- --   , substNIro ∷ 𝕏 ⇰ ℤ64
-- --   , substDMap ∷ 𝕍 (SubstElem e)
-- --   , substNMap ∷ 𝕏 ⇰ 𝕍 (SubstElem e)
-- --   , substMMap ∷ 𝕏 ⇰ e
-- --   } deriving (Eq,Ord,Show)
-- -- makeLenses ''Subst
-- -- 
-- -- substVar ∷ Subst e → 𝕐 → SubstElem e
-- -- substVar (Subst ρᴰ ρᴺ ιᴰ ιᴺ σᴰ σᴺ σᴹ) y = 
-- --   let ℯO = case y of
-- --         DVar i → tries
-- --           [ do guard $ i < intΩ64 ρᴰ
-- --                return $ Var_DSE $ DVar i
-- --           , do n ← natO64 $ i - intΩ64 ρᴰ
-- --                lup n σᴰ
-- --           ]
-- --         NVar i x → tries
-- --           [ do guard $ i < ifNone 0 (intΩ64 ^$ ρᴺ ⋕? x)
-- --                return $ Var_DSE $ NVar i x
-- --           , do n ← natO64 $ i - ifNone 0 (intΩ64 ^$ ρᴺ ⋕? x)
-- --                lup n *$ lup x σᴺ
-- --           ]
-- --         MVar x → do
-- --           e ← lup x σᴹ
-- --           return $ Trm_DSE 0 zero e
-- --   in case ℯO of
-- --     None → Var_DSE $ intro𝕐 ιᴰ ιᴺ y
-- --     Some ℯ → ℯ
-- -- 
-- -- data SubstEnv s e₁ e₂ = SubstEnv
-- --   { substEnvSubs ∷ s ⇰ Subst e₁
-- --   , substEnvView ∷ e₁ → 𝑂 e₂
-- --   }
-- -- makeLenses ''SubstEnv
-- -- 
-- -- newtype SubstT s e₁ e₂ (m ∷ ★ → ★) a = SubstT { unSubstT ∷ UContT (ReaderT (SubstEnv s e₁ e₂) (FailT m)) a }
-- --   deriving
-- --   ( Return,Bind,Functor,Monad
-- --   , MonadUCont
-- --   , MonadReader (SubstEnv s e₁ e₂)
-- --   , MonadFail
-- --   )
-- -- 
-- -- runSubstT ∷ (Monad m) ⇒ SubstEnv s e₁ e₂ → SubstT s e₁ e₂ m a → m (𝑂 a)
-- -- runSubstT γ = unFailT ∘ runReaderT γ ∘ evalUContT ∘ unSubstT
-- -- 
-- -- class Substy s e a where
-- --   substy ∷ ∀ e' m. a → SubstT s e' e m a
-- -- 
-- -- substyBdr ∷ (Ord s,ToIter s t,Monad m) ⇒ t → 𝕏 → SubstT s e₁ e₂ m ()
-- -- substyBdr ss x = do
-- --   umodifyEnvL substEnvSubsL $ compose $ mapOn (iter ss) $ mapOnKeyWith $ compose
-- --     [ alter substDShfL $ (+) 1
-- --     , alter substNShfL $ (+) $ x ↦ 1
-- --     ]
-- -- 
-- -- substyVar ∷ (Ord s,Monad m) ⇒ (𝕐 → e₂) → (ℤ64 → (𝕏 ⇰ ℤ64) → e₂ → e₂) → s → 𝕐 → SubstT s e₁ e₂ m e₂
-- -- substyVar evar eintro s y = do
-- --   σO ← lup s ^$ askL substEnvSubsL
-- --   case σO of
-- --     None → return $ evar y
-- --     Some σ → do
-- --       𝓋 ← askL substEnvViewL
-- --       case substVar σ y of
-- --         Var_DSE y' → return $ evar y'
-- --         Trm_DSE i' xis e → failEff $ eintro i' xis ^$ 𝓋 e
-- -- 
-- -- substAppend ∷ (Monad m) ⇒ (Subst a → b → m a) → Subst a → Subst b → m (Subst a)
-- -- substAppend 
-- --   esubst 
-- --   σ₂@(Subst ρᴰ₂ ρᴺ₂ ιᴰ₂ ιᴺ₂ σᴰ₂ σᴺ₂ σᴹ₂) 
-- --   σ₁@(Subst ρᴰ₁ ρᴺ₁ ιᴰ₁ ιᴺ₁ σᴰ₁ σᴺ₁ σᴹ₁) = do
-- --     let ρᴰ = ρᴰ₁ ⊓ ρᴰ₂
-- --         ρᴺ = ρᴺ₁ ⊓ ρᴺ₂
-- --         σᴰLogicalSize = natΩ64 $ joins
-- --           [ intΩ64 (csize σᴰ₁) + intΩ64 ρᴰ₁
-- --           , intΩ64 (csize σᴰ₂) + intΩ64 ρᴰ₂ - ιᴰ₁
-- --           ]
-- --         σᴰSize = σᴰLogicalSize - ρᴰ
-- --         σᴰOffset = ρᴰ₁ - ρᴰ
-- --         ιᴰ = ιᴰ₁ + ιᴰ₂
-- --     σᴰ ← exchange $ vecF σᴰSize $ \ n → do
-- --         if i < σᴰOffset
-- --         then return $ substVar σ₂ $ DVar $ int64 $ ρᴰ + n
-- --         else
-- --           case bvs₁ ⋕? (𝓍 - vsOffset₁) of
-- --             Some v → case v of
-- --               Inl 𝓎 → return $ 𝓈varsubst 𝓈₂ 𝓎
-- --               Inr (ρₑ :* e) → do
-- --                 𝓈 ← 𝓈combinesubst sub 𝓈₂ $ 𝓈intro ρₑ
-- --                 Inr ∘ (0 :*) ^$ sub 𝓈 e
-- --             None → return $ 𝓈bvarsubst 𝓈₂ $ natΩ64 $ intΩ64 (ρ + 𝓍) + ι₁
-- --      
-- --     σᴹ₁' ← mapMOn σᴹ₁ $ esubst σ₂
-- --     let σᴹ = σᴹ₁' ⩌ σᴹ₂
-- --     return $ Subst _ _ _ _ _ _ σᴹ₁'
-- -- 
-- -- 
-- -- 𝓈rnams ∷ 𝕏 ⇰ 𝕏 → Subst e
-- -- 𝓈rnams xxs = 
-- --   let n = count xxs
-- --       ℯs = vecC $ map (Var_DSE ∘ DVar ∘ intΩ64) $ upToC n
-- --       xℯs = map (single ∘ Var_DSE ∘ NVar 0) xxs
-- --   in Subst 0 null 0 null ℯs xℯs null
-- -- 
-- -- -- 𝓈dren ∷ 𝕏 → Subst e
-- -- -- 𝓈dren x = 
-- -- --   let ℯ = Var_DSE $ DVar 0
-- -- --   in Subst 0 null 0 null (single ℯ) (x ↦ single ℯ) null
-- -- -- 
-- -- -- 𝓈repl ∷ 𝕏 → e → Subst e
-- -- -- 𝓈repl x e = 
-- -- --   let ℯ = Trm_DSE 1 (x ↦ 1) e
-- -- --   in Subst 0 null 0 null (single ℯ) (x ↦ single ℯ) null
-- -- 
-- -- 𝓈metas ∷ 𝕏 ⇰ e → Subst e
-- -- 𝓈metas = Subst 0 null 0 null null null
-- 
-- -- -- data Subst s a m = Subst
-- -- --   { substBdr ∷ s ⇰ 𝕏 ⇰ 𝕏
-- -- --   , substVar ∷ s ⇰ 𝕐 ⇰ 𝕐 ∨ FailT m a
-- -- --   }
-- -- -- makeLenses ''Subst
-- -- -- 
-- -- -- data SubstEnv s a b m = SubstEnv
-- -- --   { substEnvFresh ∷ 𝑂 (m ℕ64)
-- -- --   , substEnvView  ∷ a → 𝑂 b
-- -- --   , substEnvSubst ∷ Subst s a m
-- -- --   }
-- -- -- makeLenses ''SubstEnv
-- -- -- 
-- -- -- newtype SubstT s e₁ e₂ m a = SubstM { unSubstM ∷ UContT (ReaderT (SubstEnv s e₁ e₂ m) (FailT m)) a }
-- -- --   deriving
-- -- --   ( Return,Bind,Functor,Monad
-- -- --   , MonadFail
-- -- --   , MonadReader (SubstEnv s e₁ e₂ m)
-- -- --   , MonadUCont
-- -- --   )
-- -- -- 
-- -- -- instance Transformer (SubstT s e₁ e₂) where lift = SubstM ∘ lift ∘ lift ∘ lift
-- -- -- 
-- -- -- runSubstT ∷ (Return m) ⇒ SubstEnv s e₁ e₂ m → SubstT s e₁ e₂ m a → m (𝑂 a)
-- -- -- runSubstT γ = unFailT ∘ runReaderT γ ∘ evalUContT ∘ unSubstM
-- -- -- 
-- -- -- class Substy s e a | a→s,a→e where
-- -- --   substy ∷ ∀ e' m. (Monad m) ⇒ a → SubstT s e' e m a
-- -- -- 
-- -- -- subst ∷ (Substy s e a,Monad m) ⇒ Subst s e m → a → m (𝑂 a)
-- -- -- subst γ = runSubstT (SubstEnv None return γ) ∘ substy
-- -- -- 
-- -- -- freshen ∷ (Substy s e a,Monad m) ⇒ m ℕ64 → a → m (𝑂 a)
-- -- -- freshen 𝑓M = runSubstT (SubstEnv (Some 𝑓M) return null) ∘ substy
-- -- -- 
-- -- -- instance Null (Subst s a m) where
-- -- --   null = Subst null null
-- -- -- instance (Ord s,Monad m,Substy s a a) ⇒ Append (Subst s a m) where
-- -- --   𝓈₁@(Subst sρ₁ sγ₁) ⧺ Subst sρ₂ sγ₂=
-- -- --     let sρ₂' = dmapOnWithKey sρ₂ $ \ s → map $ \ x →
-- -- --           ifNone x $ do
-- -- --             ρ ← sρ₁ ⋕? s
-- -- --             ρ ⋕? x
-- -- --         sγ₂' = dmapOnWithKey sγ₂ $ \ s → map $ \case
-- -- --           Inl x → ifNone (Inl x) $ do
-- -- --             γ ← sγ₁ ⋕? s
-- -- --             γ ⋕? x
-- -- --           Inr eM → Inr $ do
-- -- --             e ← eM
-- -- --             FailT $ subst 𝓈₁ e
-- -- --         sρ = unionWith (⩌) sρ₂' sρ₁
-- -- --         sγ = unionWith (⩌) sγ₂' sγ₁
-- -- --     in Subst sρ sγ 
-- -- -- instance (Ord s,Monad m,Substy s a a) ⇒ Monoid (Subst s a m)
-- -- -- 
-- -- -- 𝓈rescope ∷ (Ord s) ⇒ s ⇰ 𝕏 ⇰ 𝕏 → Subst s a m
-- -- -- 𝓈rescope ρ= Subst ρ null
-- -- -- 
-- -- -- 𝓈rename ∷ (Ord s) ⇒ s ⇰ 𝕐 ⇰ 𝕐 → Subst s a m
-- -- -- 𝓈rename sxx = Subst null $ map (map Inl) sxx
-- -- -- 
-- -- -- 𝓈bindM ∷ (Ord s,Monad m) ⇒ s ⇰ 𝕐 ⇰ m a → Subst s a m
-- -- -- 𝓈bindM sxeM = Subst null $ map (map $ Inr ∘ lift) sxeM
-- -- -- 
-- -- -- 𝓈bind ∷ (Ord s,Monad m) ⇒ s ⇰ 𝕐 ⇰ a → Subst s a m
-- -- -- 𝓈bind = 𝓈bindM ∘ mapp return
-- -- -- 
-- -- -- substyVar ∷ (Ord s,Monad m) ⇒ (𝕐 → e₂) → s → 𝕐 → SubstT s e₁ e₂ m e₂
-- -- -- substyVar v s x = mjoin $ tries
-- -- --   [ do SubstEnv _ 𝓋 (Subst _ sγ) ← ask
-- -- --        γ ← failEff $ sγ ⋕? s
-- -- --        xeM ← failEff $ γ ⋕? x
-- -- --        return $ case xeM of
-- -- --          Inl x' → return $ v x'
-- -- --          Inr eM → do
-- -- --            e ← failEff *$ lift $ unFailT eM
-- -- --            failEff $ 𝓋 e
-- -- --   , return $ return $ v x
-- -- --   ]
-- -- -- 
-- -- -- substyBdr ∷ (Ord s,Monad m,ToIter s t) ⇒ t → 𝕏 → SubstT s e₁ e₂ m 𝕏
-- -- -- substyBdr ss x = do
-- -- --   sρ ← askL $ substBdrL ⊚ substEnvSubstL
-- -- --   𝑓M ← askL substEnvFreshL
-- -- --   xO ← tries $ concat
-- -- --     -- first see if we are rescoping
-- -- --     [ mapOn (iter ss) $ \ s → do
-- -- --         do ρ ← failEff $ sρ ⋕? s
-- -- --            x' ← failEff $ ρ ⋕? x
-- -- --            return $ Some x'
-- -- --     -- next see if we are freshening binders
-- -- --     , single $ do
-- -- --         n ← lift *$ failEff 𝑓M
-- -- --         let x' = 𝕏 (Some n) $ 𝕩name x
-- -- --         return $ Some x'
-- -- --     -- just leave the binder alone...
-- -- --     , single $ return None
-- -- --     ]
-- -- --   x' ← case xO of
-- -- --     Some x' → do
-- -- --       eachOn ss $ \ s →
-- -- --         umodifyEnvL (keyL s ⊚ substVarL ⊚ substEnvSubstL) $ \ 𝓈O →
-- -- --           Some $ (svar𝕏 x ↦ Inl (svar𝕏 x')) ⩌ ifNone null 𝓈O
-- -- --       return x'
-- -- --     None → return x
-- -- --   eachOn ss $ \ s →
-- -- --     umodifyEnvL (keyL s ⊚ substVarL ⊚ substEnvSubstL) $ map $ delete $ svar𝕏 x'
-- -- --   return x'
-- -- -- 
-- -- -- substyFrame ∷ (Monad m) ⇒ (e₂ → 𝑂 e₃) → SubstT s e₁ e₃ m a → SubstT s e₁ e₂ m a
-- -- -- substyFrame 𝓋 xM = do
-- -- --   SubstEnv 𝑓M 𝓋' 𝓈 ← ask
-- -- --   failEff *$ lift $ runSubstT (SubstEnv 𝑓M (𝓋 *∘ 𝓋') 𝓈) xM
-- 
-- ---------------
-- -- FREE VARS --
-- ---------------
-- 
-- newtype FreevM s a = FreevM { unFreevM ∷ UContT (RWS (s ⇰ 𝕏 ⇰ ℤ64) (s ⇰ 𝑃 𝕐) ()) a }
--   deriving
--   ( Return,Bind,Functor,Monad
--   , MonadReader (s ⇰ 𝕏 ⇰ ℤ64)
--   , MonadWriter (s ⇰ 𝑃 𝕐)
--   , MonadUCont
--   )
-- 
-- runFreevM ∷ (s ⇰ 𝕏 ⇰ ℤ64) → FreevM s a → (s ⇰ 𝑃 𝕐) ∧ a
-- runFreevM γ = mapFst snd ∘ runRWS γ () ∘ evalUContT ∘ unFreevM
-- 
-- evalFreevM ∷ FreevM s a → a
-- evalFreevM = snd ∘ runFreevM null
-- 
-- class Freevy s a | a → s where
--   freevy ∷ a → FreevM s ()
-- 
-- freevyBdr ∷ (Ord s,ToIter s t) ⇒ t → 𝕏 → FreevM s ()
-- freevyBdr ss x = umodifyEnv $ (⧺) $ concat $ mapOn (iter ss) $ \ s → s ↦ x ↦ 1
-- 
-- freevyVar ∷ (Ord s) ⇒ s → 𝕐 → FreevM s ()
-- freevyVar s y = do
--   𝑠 ← ask
--   case y of
--     NVar n x → do
--       case lup x *$ lup s 𝑠 of
--         None → tell $ (↦) s $ single $ NVar n x
--         Some n' → whenZ (n ≥ n') $ tell $ (↦) s $ single $ NVar (n - n') x
--     DVar n → do
--       case lup s 𝑠 of
--         None → tell $ (↦) s $ single $ DVar n
--         Some xns → do
--           let n' = sum $ values xns
--           whenZ (n ≥ n') $ tell $ (↦) s $ single $ DVar $ n - n'
--     MVar x → tell $ (↦) s $ single $ MVar x
-- 
-- fvs ∷ (Ord s,Freevy s a) ⇒ a → s ⇰ 𝑃 𝕐
-- fvs = evalFreevM ∘ retOut ∘ freevy
-- 
-- sfvs ∷ (Ord s,Freevy s a) ⇒ s → a → 𝑃 𝕐
-- sfvs s = ifNone bot ∘ lup s ∘ fvs
-- 
-- 
