module UVMHS.Lib.Substitution.DSubst where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Rand

import UVMHS.Lib.Substitution.Var

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

