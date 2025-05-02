module UVMHS.Lib.Substitution.SubstElem where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Rand

import UVMHS.Lib.Substitution.Var

-- ==================== --
-- SUBSTITUTION ELEMENT --
-- ==================== --

--------------------------------------------------------------------------------
-- A delayed element with a delayed intro renaming. It's nice to delay things
-- like this, because it is often the case that:
-- 1. substitution elements get further shifted, which can be done while
--    maintaining delayed form by just shifting the delayed renaming
-- 2. sometimes you don't need to fully materialize the substitution value
--------------------------------------------------------------------------------

-- e ⩴ e⇈ι
-- ⟦e⇈ι⟧ = ⟦ι⟧(e)
data SubstElem s e = SubstElem
  { substElemIntro ∷ s ⇰ ℕ64   -- ^ delayed renaming
  , substElemValue ∷ () → 𝑂 e  -- ^ delayed element
  } deriving (Eq,Ord,Show)
makeLenses ''SubstElem

interpSubstElem ∷ (s ⇰ ℕ64 → e → 𝑂 e) → SubstElem s e → 𝑂 e
interpSubstElem intro (SubstElem ι ueO) = intro ι *$ ueO ()

canonSubstElem ∷ (s ⇰ ℕ64 → e → 𝑂 e) → SubstElem s e → SubstElem s e
canonSubstElem intro e = SubstElem null $ const $ interpSubstElem intro e

eqSubstElem ∷ (Eq e) ⇒ (s ⇰ ℕ64 → e → 𝑂 e) → SubstElem s e → SubstElem s e → 𝔹
eqSubstElem intro e₁ e₂ = interpSubstElem intro e₁ ≡ interpSubstElem intro e₂

compareSubstElem ∷ (Ord e) ⇒ (s ⇰ ℕ64 → e → 𝑂 e) → SubstElem s e → SubstElem s e → Ordering
compareSubstElem intro e₁ e₂ = interpSubstElem intro e₁ ⋚ interpSubstElem intro e₂

introSubstElem ∷ (Ord s) ⇒ s ⇰ ℕ64 → SubstElem s e → SubstElem s e
introSubstElem = alter substElemIntroL ∘ (+)

subSubstElem ∷ (s ⇰ ℕ64 → e → 𝑂 e) → SubstElem s e → SubstElem s e
subSubstElem intro e = SubstElem zero $ \ () → interpSubstElem intro e

-------------
-- FUNCTOR --
-------------

instance Functor (SubstElem s) where
  map f (SubstElem ι e) = SubstElem ι $ mapp f e

---------------------
-- PRETTY PRINTING --
---------------------

instance (Pretty s,Pretty e) ⇒ Pretty (SubstElem s e) where
  pretty (SubstElem ι ueO) = 
    let eD = elim𝑂 (const $ ppPun "⊥") pretty $ ueO ()
    in
    if isEmpty ι 
    then eD
    else concat [eD,ppPun "⇈",pretty ι]
  --pretty (SubstElem s ueO) =
  --  let def = ifNone (ppPun "⊥") $ map (ppPun "≔" ⧺) (pretty ^$ ueO ()) in
  --  ppGA $
  --    if csize s ≡ 0
  --      then ppHorizontal [def]
  --      -- Attempt to remove keys that map to 0 from the output
  --      -- else ppGA $ ppHorizontal [def, ppKey "where", pretty (omap𝐷 (\ n → if n ≡ 0 then None else Some n) s)]
  --      else ppGA $ ppHorizontal [ppPun "⎨", def, ppKey "where", pretty s, ppPun "⎬"]

-------------
-- FUZZING --
-------------

instance (Ord s,Fuzzy s,Fuzzy e) ⇒ Fuzzy (SubstElem s e) where
  fuzzy = return SubstElem ⊡ fuzzy ⊡ fuzzy

-- ========================== --
-- SCOPED SUBSTITUION ELEMENT --
-- ========================== --

-- e ⩴ i | ι⇈e
-- ⟦i⟧ = i
-- ⟦ι⇈e⟧ = ⟦ι⟧(e)
-- NOTE: The `Eq` instance is strictly weaker than semantic equality. To
-- compare semantic equality, you should just compare their interpretations.
data SSubstElem s e =
    Var_SSE ℕ64
  | Trm_SSE (SubstElem s e)
  deriving (Eq,Ord,Show)

interpSSubstElem ∷ (ℕ64 → e) → (s ⇰ ℕ64 → e → 𝑂 e) → SSubstElem s e → 𝑂 e
interpSSubstElem mkVar intro = \case
  Var_SSE i → Some $ mkVar i
  Trm_SSE e → interpSubstElem intro e

canonSSubstElem ∷ (ℕ64 → e) → (s ⇰ ℕ64 → e → 𝑂 e) → SSubstElem s e → SSubstElem s e
canonSSubstElem mkVar intro = \case
  Var_SSE n → Trm_SSE $ SubstElem null $ const $ Some $ mkVar n
  Trm_SSE e → Trm_SSE $ canonSubstElem intro e

eqSSubstElem ∷ (Eq e) ⇒ (ℕ64 → e) → (s ⇰ ℕ64 → e → 𝑂 e) → SSubstElem s e → SSubstElem s e → 𝔹
eqSSubstElem mkVar intro e₁ e₂ = interpSSubstElem mkVar intro e₁ ≡ interpSSubstElem mkVar intro e₂

compareSSubstElem ∷ (Ord e) ⇒ (ℕ64 → e) → (s ⇰ ℕ64 → e → 𝑂 e) → SSubstElem s e → SSubstElem s e → Ordering
compareSSubstElem mkVar intro e₁ e₂ = interpSSubstElem mkVar intro e₁ ⋚ interpSSubstElem mkVar intro e₂

introSSubstElem ∷ (Ord s) ⇒ s → s ⇰ ℕ64 → SSubstElem s e → SSubstElem s e
introSSubstElem s ι = \case
  Var_SSE n → Var_SSE $ n + ifNone 0 (ι ⋕? s)
  Trm_SSE e → Trm_SSE $ introSubstElem ι e

subSSubstElem ∷ (ℕ64 → SSubstElem s e) → (s ⇰ ℕ64 → e → 𝑂 e) → SSubstElem s e → SSubstElem s e
subSSubstElem mkVar intro = \case
  Var_SSE n → mkVar n
  Trm_SSE e → Trm_SSE $ subSubstElem intro e

-------------
-- FUNCTOR --
-------------

instance Functor (SSubstElem s) where
  map _ (Var_SSE n) = Var_SSE n
  map f (Trm_SSE s) = Trm_SSE (map f s)

---------------------
-- PRETTY PRINTING --
---------------------

instance (Pretty s,Pretty e) ⇒ Pretty (SSubstElem s e) where
  pretty = \case
    Var_SSE i → ppDVar i
    Trm_SSE e → pretty e

-------------
-- FUZZING --
-------------

instance (Ord s,Fuzzy s,Fuzzy e) ⇒ Fuzzy (SSubstElem s e) where
  fuzzy = rchoose $ map const
    [ Var_SSE ^$ fuzzy
    , Trm_SSE ^$ fuzzy
    ]


