module UVMHS.Lib.Substitution where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Parser
import UVMHS.Lib.Rand

----------------------
-- SIMPLE VARIABLES --
----------------------

-- simple variables
data 𝕏 = 𝕏
  { 𝕩mark ∷ 𝑂 ℕ64
  , 𝕩name ∷ 𝕊
  } deriving (Eq,Ord,Show)
makeLenses ''𝕏

var ∷ 𝕊 → 𝕏
var = 𝕏 None

cpVar ∷ CParser TokenBasic 𝕏
cpVar = var ^$ cpShaped $ view nameTBasicL

cpVarWS ∷ CParser TokenWSBasic 𝕏
cpVarWS = var ^$ cpShaped $ view nameTWSBasicL

instance Pretty 𝕏 where
  pretty (𝕏 nO x) = concat
    [ ppString x
    , elim𝑂 null (\ n → ppPun $ concat ["#",show𝕊 n]) nO
    ]

instance Fuzzy 𝕏 where
  fuzzy = do
    nO ← fuzzy
    return $ 𝕏 nO "x"

ppDVar ∷ ℕ64 → Doc
ppDVar n = concat [ppPun "⌊",pretty n,ppPun "⌋"]

--------------------------
-- SUBSTITUTION ELEMENT --
--------------------------

-- ℯ ⩴ s⇈e
data SubstElem s e = SubstElem
  { substElemIntro ∷ s ⇰ ℕ64
  , substElemValue ∷ () → 𝑂 e
  } deriving (Eq,Ord,Show)
makeLenses ''SubstElem

instance Functor (SubstElem s) where
  map f (SubstElem a b) = SubstElem a (map f ∘ b)

instance (Pretty s,Pretty e) ⇒ Pretty (SubstElem s e) where
  pretty (SubstElem s ueO) =
    let def = ifNone (ppPun "⊥") $ map (ppPun "≔" ⧺) (pretty ^$ ueO ()) in
    ppGA $
      if csize s ≡ 0
        then ppHorizontal [def]
        -- Attempt to remove keys that map to 0 from the output
        -- else ppGA $ ppHorizontal [def, ppKey "where", pretty (omap𝐷 (\ n → if n ≡ 0 then None else Some n) s)]
        else ppGA $ ppHorizontal [ppPun "⎨", def, ppKey "where", pretty s, ppPun "⎬"]

instance (Ord s,Fuzzy s,Fuzzy e) ⇒ Fuzzy (SubstElem s e) where
  fuzzy = return SubstElem ⊡ fuzzy ⊡ fuzzy

introSubstElem ∷ (Ord s) ⇒ s ⇰ ℕ64 → SubstElem s e → SubstElem s e
introSubstElem = alter substElemIntroL ∘ (+)

subSubstElem ∷ (s ⇰ ℕ64 → e → 𝑂 e) → SubstElem s e → SubstElem s e
subSubstElem substE (SubstElem 𝑠 ueO) = SubstElem zero $ \ () → substE 𝑠 *$ ueO ()

--------------------------------
-- SCOPED SUBSTITUION ELEMENT --
--------------------------------

-- ℯ ⩴ i | s⇈e
data SSubstElem s e =
    Var_SSE ℕ64
  | Trm_SSE (SubstElem s e)
  deriving (Eq,Ord,Show)

instance Functor (SSubstElem s) where
  map _ (Var_SSE n) = Var_SSE n
  map f (Trm_SSE s) = Trm_SSE (map f s)

instance (Pretty s,Pretty e) ⇒ Pretty (SSubstElem s e) where
  pretty = \case
    Var_SSE i → ppDVar i
    Trm_SSE e → pretty e

instance (Ord s,Fuzzy s,Fuzzy e) ⇒ Fuzzy (SSubstElem s e) where
  fuzzy = rchoose $ map const
    [ Var_SSE ^$ fuzzy
    , Trm_SSE ^$ fuzzy
    ]

introSSubstElem ∷ (Ord s) ⇒ s → s ⇰ ℕ64 → SSubstElem s e → SSubstElem s e
introSSubstElem s 𝑠 = \case
  Var_SSE n → Var_SSE $ n + ifNone 0 (𝑠 ⋕? s)
  Trm_SSE e → Trm_SSE $ introSubstElem 𝑠 e

subSSubstElem ∷ (ℕ64 → SSubstElem s e) → (s ⇰ ℕ64 → e → 𝑂 e) → SSubstElem s e → SSubstElem s e
subSSubstElem substV substE = \case
  Var_SSE n → substV n
  Trm_SSE ℯ → Trm_SSE $ subSubstElem substE ℯ

----------------------------
-- DE BRUIJN SUBSTITUTION --
----------------------------

-- 𝓈 ⩴ ⟨ρ,es,ι⟩
-- INVARIANT: |es| + ι ≥ 0
data DSubst s e = DSubst
  { dsubstShift ∷ ℕ64
  -- ^ de Bruijn indices lower than this number will be untouched by this substitution.  Think of it
  -- as a substitution working over all natural numbers being shifted to the right to ignore this
  -- many first indices.
  , dsubstElems ∷ 𝕍 (SSubstElem s e)
  -- ^ Instantiates as many of the first indices (post-shift) as the length of this vector with the
  -- values in the vector.
  , dsubstIntro ∷ ℤ64
  -- ^ Starting at the de Bruijn index after all the shifts and all the instantiations, simulate an
  -- introduction of this many de Bruijn variables, by bumping all subsequent indices by this much.
  } deriving (Eq,Ord,Show)

-- | If we get a `DSubst` where some `dsubstElems` elements are merely emulating what happens under
-- a shift, or under an intro, we simplify it to instead use those, making the vector of elements
-- shorter.
--
-- For instance, consider:
--   DSubst 3 [3, 4, 1, 1, 9, 10] 2
-- supposedly, it:
--   * keeps the first 3 indices protected (0 ↦ 0, 1 ↦ 1, 2 ↦ 2)
--   * then maps indices [3,4,5,6,7,8] to [3,4,1,1,9,10]
--   * then maps indices [9,10,11,…] to [11,12,13,‥]
-- but this could be better expressed as:
--   DSubst 5 [1, 1] 2
--   * keeps the first 5 indices protected, i.e. [0,1,2,3,4] ↦ [0,1,2,3,4]
--   * then [5,6] ↦ [1, 1]
--   * then [7,8,9,10,11,…] ↦ [9,10,11,12,13,…]
simplifyDSubst ∷ DSubst s e → DSubst s e
simplifyDSubst (DSubst s es i) =
  let
    (shifts :* intermediate) = peelPrefix s (list es)
    elems = peelReverseSuffix shifts (list $ reverse intermediate) i
  in DSubst shifts elems i
  where
    peelPrefix ∷ ℕ64 → 𝐿 (SSubstElem s e) → (ℕ64 ∧ 𝐿 (SSubstElem s e))
    peelPrefix shifts (Var_SSE h :& t) | h ≡ s = peelPrefix (shifts + 1) t
    peelPrefix shifts elems = shifts :* elems

    -- Note: technically we could pre-add shifts and intros, but this is a bit more readable
    peelReverseSuffix ∷ ℕ64 → 𝐿 (SSubstElem s e) → ℤ64 → 𝕍 (SSubstElem s e)
    peelReverseSuffix shifts (Var_SSE h :& t) intros
      | intΩ64 h ≡ intΩ64 (shifts + count t) + intros
      = peelReverseSuffix shifts t intros
    peelReverseSuffix _ revElems _ = vec (reverse revElems)

-- instance (Eq e, Eq s) ⇒ Eq (DSubst s e) where
--   ds1 == ds2 =
--     let
--       DSubst s1 es1 i1 = simplifyDSubst ds1
--       DSubst s2 es2 i2 = simplifyDSubst ds2
--       in meets [s1 ≡ s2, es1 ≡ es2, i1 ≡ i2]

makeLenses ''DSubst

-- Note: DSubst tend to be quite verbose under makePrettyRecord, so this instance tries to make them
-- print more concisely.
--
-- ⊘ means the identity substitution
--
-- Otherwise the pattern is: `/n{...}↑o` where:
-- - `/n` represents `n` shifts,
-- - `{...}` is the vector of de Bruijn instantiations,
-- - `↑o` represents `o` introductions,
-- Each of these subparts is optional if it's zero/zero-length.
instance (Pretty e, Pretty s) ⇒ Pretty (DSubst s e) where
  pretty (DSubst 0 (csize → 0) 0) = ppPun "⊘"
  pretty (DSubst s e i) =
    concat $
      (if s ≡ 0 then [] else [ppPun "/", pretty s])
      ⧺ (if csize e ≡ 0
          then []
          else [ppCollection (ppPun "{") (ppPun "}") (ppPun ",") (map pretty e)])
      ⧺ (if i ≡ 0 then [] else [ppPun "↑", pretty i])

instance Functor (DSubst s) where
  map f (DSubst a b c) = DSubst a (map (map f) b) c

instance (Ord s,Fuzzy s,Fuzzy e) ⇒ Fuzzy (DSubst s e) where
  fuzzy = do
    ρ ← fuzzy
    𝔰 ← fuzzy
    es ← mapMOn (vecF 𝔰 id) $ const fuzzy
    ι ← randr (neg $ intΩ64 𝔰) $ intΩ64 𝔰
    return $ DSubst ρ es ι

isNullDSubst ∷ DSubst s e → 𝔹
isNullDSubst (DSubst _ρ es ι) = csize es ≡ 0 ⩓ ι ≡ 0

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
-- ,  0 ↦ ⌊0⌋    | ≡
-- ,  1 ↦ ⌊1⌋    |
-- ---------------
-- ,  2 ↦  e     | [e]
-- ---------------
-- ,  3 ↦ ⌊2⌋    | -1
-- ,  4 ↦ ⌊3⌋    |
-- , …
-- ]
dsubstVar ∷ DSubst 𝑠 e → ℕ64 → SSubstElem 𝑠 e
dsubstVar (DSubst ρ̇ es ι) ṅ =
  let 𝔰̇  = csize es
      n  = intΩ64 ṅ
  in
  if
  | ṅ < ρ̇     → Var_SSE ṅ
  | ṅ < 𝔰̇+ρ̇   → es ⋕! (ṅ-ρ̇)
  | otherwise → Var_SSE $ natΩ64 $ n+ι

-------------------------------
-- GENERIC SCOPED SUBSTITUTION --
-------------------------------

-- A "named" variable will still use GSubst. Substitutions for named variables
-- are seen as maps from variable names (i.e., 𝕏 things, or just strings
-- conceptually) to a DSubst. In order to perform substitutions on DSubst, you
-- need to also have the GVar and MVar substitution environments lying around.
-- So the GSubst type is used for both named and de-bruijn substitutions.
--
-- Put another way, you can think of `DVar` substitutions using `DSubst` and `NVar`
-- substitutions using `𝕏 ⇰ DSubst`. When you keep around the GVar and MVar
-- subsitution environments, you end up with `GSubst` and `𝕏 ⇰ GSubst` as the
-- DVar and NVar substitution structures.

data GSubst s₁ s₂ e = GSubst
  { gsubstGVars ∷ s₁ ⇰ SubstElem s₂ e
  -- , gsubstMetas ∷ s₁ ⇰ SubstElem s₂ e
  , gsubstSubst ∷ s₂ ⇰ DSubst s₂ e
  }
  deriving (Eq,Ord,Show)
makeLenses ''GSubst

instance (Pretty a, Pretty b, Pretty c) ⇒ Pretty (GSubst a b c) where
  pretty (GSubst g s)
    | csize g ≡ 0 ⩓ csize s ≡ 0 = ppString "⊘"
    | otherwise =
        ppGA $ ppCollection (ppPun "⟨") (ppPun "⟩") (ppPun ",")
          [ concat [ppString "𝐆:", ppGA $ pretty g]
          -- , concat [ppString "𝐌:", ppGA $ pretty m]
          , concat [ppString "𝐒:", ppGA $ pretty s]
          ]

instance Functor (GSubst s₁ s₂) where
  map f (GSubst a c) = GSubst (map (map f) a) (map (map f) c)

-- generates random substitutions for property based testing
instance (Ord s₁,Ord s₂,Fuzzy s₁,Fuzzy s₂,Fuzzy e) ⇒ Fuzzy (GSubst s₁ s₂ e) where
  fuzzy = return GSubst ⊡ fuzzy ⊡ fuzzy

-- alter a substitution to "protect" the first n de bruijn indices
-- 0 ↦ 1
-- 1 ↦ 2
-- 2 ↦ 3
-- ⇒ shift 1
-- 0 ↦ 0
-- 1 ↦ 2
-- 2 ↦ 3
-- 3 ↦ 4
𝓈shiftG ∷ (Ord s₂) ⇒ s₂ ⇰ ℕ64 → GSubst s₁ s₂ e → GSubst s₁ s₂ e
𝓈shiftG 𝑠 (GSubst esᴳ 𝓈s) =
  let esᴳ' = map (introSubstElem 𝑠) esᴳ
      𝓈s' = kmapOn 𝓈s $ \ s (DSubst ρ es ι) →
        let ρ'  = ρ + ifNone 0 (𝑠 ⋕? s)
            es' = mapOn es $ introSSubstElem s 𝑠
        in DSubst ρ' es' ι
  in GSubst esᴳ' 𝓈s'

-- the substitution that introduces de bruijn variable 0, and shifts everything
-- else up by one
-- 0 ↦ 1
-- 1 ↦ 2
-- etc.
𝓈introG ∷ s₂ ⇰ ℕ64 → GSubst s₁ s₂ e
𝓈introG 𝑠 = GSubst null $ mapOn 𝑠 $ DSubst 0 null ∘ intΩ64

𝓈sbindsG ∷ s₂ ⇰ 𝕍 e → GSubst s₁ s₂ e
𝓈sbindsG ess = GSubst null $ mapOn ess $ \ es →
  let ℯs = map (Trm_SSE ∘ SubstElem null ∘ const ∘ return) es
      ι  = neg $ intΩ64 $ csize es
  in DSubst zero ℯs ι

𝓈sgbindsG ∷ s₁ ⇰ e → GSubst s₁ s₂ e
𝓈sgbindsG esᴳ = GSubst (map (SubstElem null ∘ const ∘ return) esᴳ) null

-- 𝓈smbindsG ∷ s₁ ⇰ e → GSubst s₁ s₂ e
-- 𝓈smbindsG esᴳ = GSubst null (map (SubstElem null ∘ const ∘ return) esᴳ) null

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
appendGSubst ∷
  (Ord s₁,Ord s₂)
  ⇒ (GSubst s₁ s₂ e → e → 𝑂 e)
  → GSubst s₁ s₂ e
  → GSubst s₁ s₂ e
  → GSubst s₁ s₂ e
appendGSubst esubst 𝓈̂₂ 𝓈̂₁ =
  let GSubst esᴳ₁ 𝓈s₁ = 𝓈̂₁
      GSubst esᴳ₂ 𝓈s₂ = 𝓈̂₂
      esub 𝓈 𝑠 = esubst $ appendGSubst esubst 𝓈 $ 𝓈introG 𝑠
      ℯsub s 𝓈 = subSSubstElem (elim𝑂 (const Var_SSE) dsubstVar $ gsubstSubst 𝓈 ⋕? s) $ esub 𝓈
      esᴳ₁' = map (subSubstElem $ esub 𝓈̂₂) esᴳ₁
      -- esᴹ₁' = map (subSubstElem $ esub 𝓈̂₂) esᴹ₁
      𝓈s₁' = kmapOn 𝓈s₁ $ \ s (DSubst ρ̇₁ es₁ ι₁) → DSubst ρ̇₁ (mapOn es₁ $ ℯsub s 𝓈̂₂) ι₁
      esᴳ = esᴳ₁' ⩌ esᴳ₂
      -- esᴹ = esᴹ₁' ⩌ esᴹ₂
      𝓈s = dunionByOn 𝓈s₂ 𝓈s₁' $ \ 𝓈₂@(DSubst ρ̇₂ es₂ ι₂) 𝓈₁@(DSubst ρ̇₁ es₁ ι₁) →
        if
        | isNullDSubst 𝓈₁ → 𝓈₂
        | isNullDSubst 𝓈₂ → 𝓈₁
        | otherwise →
            let 𝔰₁ = intΩ64 $ csize es₁
                𝔰₂ = intΩ64 $ csize es₂
                ρ₁ = intΩ64 ρ̇₁
                ρ₂ = intΩ64 ρ̇₂
                ρ̇  = ρ̇₁⊓ρ̇₂
                ρ  = intΩ64 ρ̇
                ι  = ι₁+ι₂
                𝔰  = ((ρ₁+𝔰₁)⊔(ρ₂+𝔰₂-ι₁))-ρ
                δ  = ρ
                es = vecF (natΩ64 𝔰) $ \ ṅ →
                  let n = intΩ64 ṅ + δ in
                  if
                  | n < ρ₁⊓(ρ₂+𝔰₂) → es₂ ⋕! natΩ64 (n-ρ₂)
                  | n < ρ₁         → Var_SSE $ natΩ64 $ n+ι₂
                  | n < ρ₁+𝔰₁      → es₁ ⋕! natΩ64 (n-ρ₁)
                  | n < ρ₂-ι₁      → Var_SSE $ natΩ64 $ n+ι₁
                  | n < ρ₂+𝔰₂-ι₁   → es₂ ⋕! natΩ64 (n+ι₁-ρ₂)
                  | otherwise      → error "bad"
            in
            DSubst ρ̇ es ι
  in GSubst esᴳ 𝓈s

-------------------------------------------
-- SUBSTY (STANDARD SCOPED SUBSTITUTION) --
-------------------------------------------

-- ========= --
-- VARIABLES --
-- ========= --

-- FYI there is no Substy instance for Subst, which would be "applying a
-- substitution to a substition". The way to achieve that is just through
-- append, or `⧺`, via the Append type class for which Subst has an instance.
newtype Subst s e = Subst {
  unSubst ∷
    GSubst
      (s ∧ 𝕏)   -- domain for global variables: scope + gvar name
      (s ∧ 𝑂 𝕏) -- domain for scoped variables: scope + either name or None for de Bruijn substitution
      e
  }
  deriving (Eq,Ord,Show,Pretty,Fuzzy)
makeLenses ''Subst

newtype MetaSubst s e = MetaSubst { unMetaSubst ∷ (s ∧ 𝕏) ⇰ SubstElem (s ∧ 𝑂 𝕏) e }
  deriving (Eq,Ord,Show,Pretty,Fuzzy)
makeLenses ''MetaSubst

instance Functor (Subst s) where
  map f (Subst s) = Subst (map f s)

-- fancy variables
data 𝕐 s e =
    DVar ℕ64            -- de bruijn variable
  | NVar ℕ64 𝕏          -- named (+ de bruijn index for that name)
                        -- λ x. λ x. x↑0
                        --        └───┘
                        -- λ x. λ x. x↑1
                        --   └────────┘
  | GVar 𝕏              -- global variable
  | MVar 𝕏 (Subst s e)  -- meta variable
  deriving (Eq,Ord,Show)
makePrisms ''𝕐

instance Functor (𝕐 s) where
  map _ (DVar n) = DVar n
  map _ (NVar n x) = NVar n x
  map _ (GVar n) = GVar n
  map f (MVar x s) = MVar x (map f s)

nvar ∷ 𝕏 → 𝕐 s e
nvar = NVar 0

nvarL ∷ 𝕐 s e ⌲ 𝕏
nvarL = prism nvar $ \case
  NVar n x | n≡0 → Some x
  _ → None

gensymVar ∷ (Monad m,MonadState s m) ⇒ s ⟢ ℕ64 → 𝕊 → m 𝕏
gensymVar ℓ s = do
  n ← nextL ℓ
  return $ 𝕏 (Some n) s

instance (Pretty e, Pretty s) ⇒ Pretty (𝕐 s e) where
  pretty = \case
    NVar n x → concat [pretty x,if n ≡ 0 then null else ppPun $ concat ["↑",show𝕊 n]]
    DVar n → ppDVar n
    GVar x → concat [pretty x]
    MVar x 𝓈 → concat [pretty x,ppPun "†",pretty 𝓈]

cpNVar ∷ CParser TokenBasic (𝕐 s e)
cpNVar = nvar ∘ var ^$ cpShaped $ view nameTBasicL

cpGVar ∷ CParser TokenBasic (𝕐 s e)
cpGVar = GVar ∘ var ^$ cpShaped $ view nameTBasicL

cpNVarWS ∷ CParser TokenWSBasic (𝕐 s e)
cpNVarWS = nvar ∘ var ^$ cpShaped $ view nameTWSBasicL

cpGVarWS ∷ CParser TokenWSBasic (𝕐 s e)
cpGVarWS = GVar ∘ var ^$ cpShaped $ view nameTWSBasicL

-------------------------
-- FUZZY for Variables --
-------------------------

instance (Pretty e,Pretty s,Ord s,Fuzzy s,Fuzzy e) ⇒ Fuzzy (𝕐 s e) where
  fuzzy = do
    d ← askL fuzzyEnvDepthL
    wrchoose
      [ (:*) one $ \ () → DVar ^$ fuzzy
      , (:*) one $ \ () → return NVar ⊡ fuzzy ⊡ fuzzy
      , (:*) one $ \ () → GVar ^$ fuzzy
      , (:*) d $ \ () → return MVar ⊡ fuzzy ⊡ fuzzyRec fuzzy
      ]
data FreeVarsAction s e = FreeVarsAction
  { freeVarsActionFilter ∷ s → 𝕐 s e → 𝔹
  , freeVarsActionScope  ∷ (s ∧ 𝑂 𝕏) ⇰ ℕ64
  }
makeLenses ''FreeVarsAction

data SubstAction s e = SubstAction
  -- None == leave binders alone
  -- Some True ==  make everything nameless
  -- Some False == make everything named
  { substActionReBdr ∷ 𝑂 𝔹
  , substActionSubst ∷ Subst s e
  }
makeLenses ''SubstAction
makePrettyRecord ''SubstAction

-- Substy things are things that support having an action in the SubstM monad.
-- This "action" can either be a "compute free variables" action or a
-- "substition" action. This action is encoded as a parameter in the monadic
-- environment.
data SubstEnv s e =
    FVsSubstEnv (FreeVarsAction s e)
  | SubSubstEnv (SubstAction s e)
  | MetaSubstEnv (MetaSubst s e)
makePrisms ''SubstEnv

instance (Pretty e, Pretty s) ⇒ Pretty (SubstEnv s e) where
  pretty (FVsSubstEnv{}) = ppString "FVsSubstEnv (cannot be prettified)"
  pretty (SubSubstEnv sa) = pretty sa
  pretty (MetaSubstEnv s) = pretty s

-- ReaderT (SubstEnv s e)
-- ⇈ the action, which is either compute free variables
-- or perform substitution
-- WriterT (s ⇰ 𝑃 𝕐)
-- ⇈ computes free variables (I think only when the action says to do so TODO:
-- confirm)
newtype SubstM s e a = SubstM
  { unSubstM ∷ UContT (ReaderT (SubstEnv s e) (FailT (WriterT (s ⇰ 𝑃 (𝕐 s e)) ID))) a
  } deriving
  ( Return,Bind,Functor,Monad
  , MonadUCont
  , MonadReader (SubstEnv s e)
  , MonadWriter (s ⇰ 𝑃 (𝕐 s e))
  , MonadFail
  )

mkSubstM ∷ (∀ u. SubstEnv s e → (a → SubstEnv s e → (s ⇰ 𝑃 (𝕐 s e)) ∧ 𝑂 u) → (s ⇰ 𝑃 (𝕐 s e)) ∧ 𝑂 u)
         → SubstM s e a
mkSubstM f = SubstM $ UContT (\ 𝓀 → ReaderT $ \ γ → FailT $ WriterT $ ID $ f γ $ \ x γ' →
  unID $ unWriterT $ unFailT $ runReaderT γ' $ 𝓀 x)

runSubstM ∷
    SubstEnv s e
  → (a → SubstEnv s e → (s ⇰ 𝑃 (𝕐 s e)) ∧ 𝑂 u)
  → SubstM s e a
  → (s ⇰ 𝑃 (𝕐 s e)) ∧ 𝑂 u
runSubstM γ 𝓀 = unID ∘ unWriterT ∘ unFailT ∘ runReaderT γ ∘ runUContT 𝓀' ∘ unSubstM
  where
    𝓀' x = ReaderT $ \ γ' → FailT $ WriterT $ ID $ 𝓀 x γ'

runSubstMHalt ∷ SubstEnv s e → SubstM s e a → (s ⇰ 𝑃 (𝕐 s e)) ∧ 𝑂 a
runSubstMHalt γ = runSubstM γ (\ x _ → null :* Some x)

----------------
-- Substy API --
----------------

class Substy s e a | a→s,a→e where
  substy ∷ STACK ⇒ a → SubstM s e a

-- This is the big top level API point of entry for applying a substitution.
-- Most of the API lower down is concerned with constructing substitutions.
-- ("substitution" = substitution or free variable computation, per SubstEnv)
subst ∷ STACK ⇒ (Substy s e a) ⇒ Subst s e → a → 𝑂 a
subst 𝓈 = snd ∘ runSubstMHalt (SubSubstEnv $ SubstAction None 𝓈) ∘ substy

msubst  ∷ STACK ⇒ (Substy s e a) ⇒ MetaSubst s e → a → 𝑂 a
msubst 𝓈 = snd ∘ runSubstMHalt (MetaSubstEnv 𝓈) ∘ substy

todbr ∷ (Substy s e a) ⇒ a → 𝑂 a
todbr = snd ∘ runSubstMHalt (SubSubstEnv $ SubstAction (Some True) null) ∘ substy

tonmd ∷ (Substy s e a) ⇒ a → 𝑂 a
tonmd = snd ∘ runSubstMHalt (SubSubstEnv $ SubstAction (Some False) null) ∘ substy

fvsWith ∷ (Substy s e a) ⇒ (FreeVarsAction s e → FreeVarsAction s e) → a → s ⇰ 𝑃 (𝕐 s e)
fvsWith f = fst ∘ runSubstMHalt (FVsSubstEnv $ f $ FreeVarsAction (const $ const True) null) ∘ substy

fvsSMetas ∷ (Ord s,Ord e,Substy s e a) ⇒ 𝑃 s → a → s ⇰ 𝑃 (𝕏 ∧ Subst s e)
fvsSMetas ss =
  map (pow ∘ filterMap (view mVarL) ∘ iter)
  ∘ fvsWith (update freeVarsActionFilterL $ \ s y → s ∈ ss ⩓ shape mVarL y)

fvsMetas ∷ (Ord s,Ord e,Substy s e a) ⇒ s → a → 𝑃 (𝕏 ∧ Subst s e)
fvsMetas s x = ifNone pø $ fvsSMetas (single s) x ⋕? s

fvs ∷ (Substy s e a) ⇒ a → s ⇰ 𝑃 (𝕐 s e)
fvs = fvsWith id

nullSubst ∷ Subst s e
nullSubst = Subst $ GSubst null null

appendSubst ∷ (Ord s,Substy s e e) ⇒ Subst s e → Subst s e → Subst s e
appendSubst 𝓈₂ 𝓈₁ = Subst $ appendGSubst (subst ∘ Subst) (unSubst 𝓈₂) $ unSubst 𝓈₁

instance                        Null   (Subst s e) where null = nullSubst
instance (Ord s,Substy s e e) ⇒ Append (Subst s e) where (⧺)  = appendSubst
instance (Ord s,Substy s e e) ⇒ Monoid (Subst s e)

-- 𝓈     = substitution library
-- s     = scoped
-- d     = nameless
-- shift = "going under a binder"
𝓈sdshift ∷ (Ord s) ⇒ s ⇰ ℕ64 → Subst s e → Subst s e
𝓈sdshift = alter unSubstL ∘ 𝓈shiftG ∘ assoc ∘ map (mapFst $ flip (:*) None) ∘ iter

-- n = named
𝓈snshift ∷ (Ord s) ⇒ s ⇰ 𝕏 ⇰ ℕ64 → Subst s e → Subst s e
𝓈snshift 𝑠 = alter unSubstL $ 𝓈shiftG $ assoc $ do
  s :* xns ← iter 𝑠
  x :* n ← iter xns
  return $ s :* Some x :* n

-- intro = "
𝓈sdintro ∷ (Ord s) ⇒ s ⇰ ℕ64 → Subst s e
𝓈sdintro = Subst ∘ 𝓈introG ∘ assoc ∘ map (mapFst $ flip (:*) None) ∘ iter

𝓈snintro ∷ (Ord s) ⇒ s ⇰ 𝕏 ⇰ ℕ64 → Subst s e
𝓈snintro 𝑠 = Subst $ 𝓈introG $ assoc $ do
  s :* xns ← iter 𝑠
  x :* n ← iter xns
  return $ s :* Some x :* n

-- dbinds = "substitute de bruijn indices 0..n with elements of this vector"
𝓈sdbinds ∷ (Ord s) ⇒ s ⇰ 𝕍 e → Subst s e
𝓈sdbinds = Subst ∘ 𝓈sbindsG ∘ assoc ∘ map (mapFst $ flip (:*) None) ∘ iter

𝓈sdbind ∷ (Ord s) ⇒ s → e → Subst s e
𝓈sdbind s e = 𝓈sdbinds $ s ↦ single e

-- nbinds = "substitude named variables with key/value pairings in this
-- dictionary"
𝓈snbinds ∷ (Ord s) ⇒ s ⇰ 𝕏 ⇰ 𝕍 e → Subst s e
𝓈snbinds 𝑠 = Subst $ 𝓈sbindsG $ assoc $ do
  s :* xess ← iter 𝑠
  x :* es ← iter xess
  return $ s :* Some x :* es

𝓈snbind ∷ (Ord s) ⇒ s → 𝕏 → e → Subst s e
𝓈snbind s x e = 𝓈snbinds $ s ↦ x ↦ single e

-- g = global
𝓈sgbinds ∷ (Ord s) ⇒ s ⇰ 𝕏 ⇰ e → Subst s e
𝓈sgbinds sxes = Subst $ 𝓈sgbindsG $ assoc $ do
  s :* xes ← iter sxes
  x :* e ← iter xes
  return $ s :* x :* e

𝓈sgbind ∷ (Ord s) ⇒ s → 𝕏 → e → Subst s e
𝓈sgbind s x e = 𝓈sgbinds $ s ↦ x ↦ e

-- m = meta
𝓈smbinds ∷ (Ord s) ⇒ s ⇰ 𝕏 ⇰ e → MetaSubst s e
𝓈smbinds sxes = MetaSubst $ assoc $ do
  s :* xes ← iter sxes
  x :* e ← iter xes
  return $ s :* x :* SubstElem null (const (return e))

-- non-plural = singular
𝓈smbind ∷ (Ord s) ⇒ s → 𝕏 → e → MetaSubst s e
𝓈smbind s x e = 𝓈smbinds $ s ↦ x ↦ e

-- no s = unscoped
𝓈dshift ∷ ℕ64 → Subst () e → Subst () e
𝓈dshift = 𝓈sdshift ∘ (↦) ()

-- no s = unscoped
𝓈nshift ∷ 𝕏 ⇰ ℕ64 → Subst () e → Subst () e
𝓈nshift = 𝓈snshift ∘ (↦) ()

-- no s = unscoped
𝓈dintro ∷ ℕ64 → Subst () e
𝓈dintro = 𝓈sdintro ∘ (↦) ()

-- no s = unscoped
𝓈nintro ∷ 𝕏 ⇰ ℕ64 → Subst () e
𝓈nintro = 𝓈snintro ∘ (↦) ()

-- no s = unscoped
𝓈dbinds ∷ 𝕍 e → Subst () e
𝓈dbinds = 𝓈sdbinds ∘ (↦) ()

-- no s = unscoped
𝓈dbind ∷ e → Subst () e
𝓈dbind = 𝓈sdbind ()

-- no s = unscoped
𝓈nbinds ∷ 𝕏 ⇰ 𝕍 e → Subst () e
𝓈nbinds = 𝓈snbinds ∘ (↦) ()

-- no s = unscoped
𝓈nbind ∷ 𝕏 → e → Subst () e
𝓈nbind = 𝓈snbind ()

-- no s = unscoped
𝓈gbinds ∷ 𝕏 ⇰ e → Subst () e
𝓈gbinds = 𝓈sgbinds ∘ (↦) ()

-- no s = unscoped
𝓈gbind ∷ 𝕏 → e → Subst () e
𝓈gbind x e = 𝓈gbinds $ x ↦ e

-- no s = unscoped
𝓈mbinds ∷ 𝕏 ⇰ e → MetaSubst () e
𝓈mbinds = 𝓈smbinds ∘ (↦) ()

-- no s = unscoped
𝓈mbind ∷ 𝕏 → e → MetaSubst () e
𝓈mbind x e = 𝓈mbinds $ x ↦ e

--------------------------------------------------
-- CONCRETE IMPLEMENTATIONS OF SUBSTY INSTANCES --
--------------------------------------------------

substyDBdr ∷ (Ord s,Ord e) ⇒ s → SubstM s e ()
substyDBdr s = umodifyEnv $ compose
  [ alter subSubstEnvL $ alter substActionSubstL $ 𝓈sdshift $ s ↦ 1
  , alter fVsSubstEnvL $ alter freeVarsActionScopeL $ (⧺) $ (s :* None) ↦ 1
  ]

substyNBdr ∷ (Ord s,Ord e) ⇒ s → 𝕏 → SubstM s e ()
substyNBdr s x = umodifyEnv $ compose
  [ alter subSubstEnvL $ alter substActionSubstL $ 𝓈snshift $ s ↦ x ↦ 1
  , alter fVsSubstEnvL $ alter freeVarsActionScopeL $ (⧺) $ (s :* Some x) ↦ 1
  ]

substyBdr ∷ (Ord s,Ord e,Substy s e e) ⇒ s → (𝕐 s e' → e) → 𝕏 → SubstM s e ()
substyBdr s 𝓋 x = do
  substyDBdr s
  substyNBdr s x
  bO ← access substActionReBdrL *∘ view subSubstEnvL ^$ ask
  case bO of
    None → skip
    Some b → do
      if b
      then
        umodifyEnv $ alter subSubstEnvL $ alter substActionSubstL $ flip (⧺) $ concat
          [ 𝓈snintro $ s ↦ x ↦ 1
          , 𝓈snbind s x $ 𝓋 $ DVar 0
          ]
      else
        umodifyEnv $ alter subSubstEnvL $ alter substActionSubstL $ flip (⧺) $ concat
          [ 𝓈sdintro $ s ↦ 1
          , 𝓈sdbind s $ 𝓋 $ NVar 0 x
          ]

-- 𝑂 𝕏 parameter `xO`...
-- None = nameless
-- Some x = named with name `x`
-- this is "the name"
--
-- ℕ64 parameter `n` is the de bruijn level/number
substyVar ∷ (Ord s,Ord e,Substy s e e) ⇒ 𝑂 𝕏 → s → (ℕ64 → e) → ℕ64 → SubstM s e e
substyVar xO s 𝓋 n = do
  γ ← ask
  case γ of
    FVsSubstEnv 𝒶 → do
      let n₀ = ifNone 0 (freeVarsActionScope 𝒶 ⋕? (s :* xO))
      when (n ≥ n₀) $ \ () → do
        let n' = n-n₀
            y = elim𝑂 (const DVar) (flip NVar) xO n'
        when (freeVarsActionFilter 𝒶 s y) $ \ () →
          tell $ s ↦ single y
      return $ 𝓋 n
    SubSubstEnv 𝒶 → do
      let 𝓈s = gsubstSubst $ unSubst $ substActionSubst 𝒶
      case 𝓈s ⋕? (s :* xO) of
        None → return $ 𝓋 n
        Some 𝓈 → case dsubstVar 𝓈 n of
          Var_SSE n' → return $ 𝓋 n'
          Trm_SSE (SubstElem 𝑠 ueO) → failEff $ subst (Subst $ 𝓈introG 𝑠) *$ ueO ()
    MetaSubstEnv{} → return $ 𝓋 n -- I think we just don't apply meta-substitutions to D/NVars?

substyDVar ∷ (Ord s,Ord e,Substy s e e) ⇒ s → (ℕ64 → e) → ℕ64 → SubstM s e e
substyDVar = substyVar None

substyNVar ∷ (Ord s,Ord e,Substy s e e) ⇒ s → (ℕ64 → e) → 𝕏 → ℕ64 → SubstM s e e
substyNVar s 𝓋 x = substyVar (Some x) s 𝓋

substyGVar ∷ (Ord s,Ord e,Substy s e e) ⇒ s → (𝕏 → e) → 𝕏 → SubstM s e e
substyGVar s 𝓋 x = do
  γ ← ask
  case γ of
    FVsSubstEnv 𝒶 → do
      let y = GVar x
      when (freeVarsActionFilter 𝒶 s y) $ \ () →
        tell $ s ↦ single y
      return $ 𝓋 x
    SubSubstEnv 𝓈A → do
      let gsᴳ =  gsubstGVars $ unSubst $ substActionSubst 𝓈A
      case gsᴳ ⋕? (s :* x) of
        None → return $ 𝓋 x
        Some (SubstElem 𝑠 ueO) → failEff $ subst (Subst $ 𝓈introG 𝑠) *$ ueO ()
    MetaSubstEnv{} → return $ 𝓋 x -- I think we just don't apply meta-substitutions to GVars?

substyMVar ∷ (Ord s,Ord e,Pretty e,Pretty s,Substy s e e) ⇒ s → (𝕏 → Subst s e → e) → 𝕏 → Subst s e → SubstM s e e
substyMVar s 𝓋 x 𝓈₀ = do
  γ ← ask
  case γ of
    FVsSubstEnv 𝒶 → do
      let y = MVar x 𝓈₀
      when (freeVarsActionFilter 𝒶 s y) $ \ () →
        tell $ s ↦ single y
      return $ 𝓋 x 𝓈₀
    SubSubstEnv 𝓈A → do
      let 𝓈 = substActionSubst 𝓈A
          -- This versions makes more intuitive sense, in that the incoming substitution action
          -- should have the final word? (This assumes the append does RHS before LHS)
          𝓈' = 𝓈 ⧺ 𝓈₀
          -- This version seems to work better:
          -- 𝓈' = 𝓈₀ ⧺ 𝓈
      return $ 𝓋 x 𝓈'
    MetaSubstEnv (MetaSubst gs) →
      case gs ⋕? (s :* x) of
        None → return $ 𝓋 x 𝓈₀
        Some (SubstElem 𝑠 ueO) →
          failEff $ subst (Subst (𝓈introG 𝑠) ⧺ 𝓈₀) *$ ueO ()

-- subst (𝓈₁ ∘ 𝓈₂) e ≡ subst 𝓈₁ (subst 𝓈₂ e)
--
-- subst (apply 𝓈₁ 𝓈₂) e ≡ subst (mapOn 𝓈₂ (\ x e′ → apply 𝓈₁ e′)) e
-- apply 𝓈₁ id ≡ 𝓈₁
-- apply 𝓈 {0 ↦ 1 , 1 ↦ 2}
-- 𝓈₂(χ⋅𝓈₁)
--
-- (𝓈₂∘𝓈₁)(χ)
--
-- 𝓈₂(χ) = e
--
-- 𝓈₁(e) ← result
--
-- χ⋅id
--
-- 𝓈(χ⋅id) = χ⋅𝓈
--
-- 𝓈₁(𝓈₂(χ⋅id)) ≡ 𝓈₁(χ⋅𝓈₂) ≡ (𝓈₁∘𝓈₂)(χ)

substy𝕐 ∷ (Ord s,Ord e,Pretty e,Pretty s,Substy s e e) ⇒ s → (𝕐 s e → e) → 𝕐 s e → SubstM s e e
substy𝕐 s 𝓋 = \case
  DVar n     → substyDVar s (𝓋 ∘ DVar)        n
  NVar n x   → substyNVar s (𝓋 ∘ flip NVar x) x n
  GVar   x   → substyGVar s (𝓋 ∘ GVar)        x
  MVar   x 𝓈 → substyMVar s (𝓋 ∘∘ MVar)       x 𝓈
