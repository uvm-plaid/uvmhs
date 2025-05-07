module UVMHS.Lib.Substitution.SubstElem where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Rand
import UVMHS.Lib.Parser

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
interpSubstElem substE (SubstElem ι ueO) = substE ι *$ ueO ()

canonSubstElem ∷ (s ⇰ ℕ64 → e → 𝑂 e) → SubstElem s e → SubstElem s e
canonSubstElem substE e = SubstElem null $ const $ interpSubstElem substE e

eqSubstElem ∷ (Eq e) ⇒ (s ⇰ ℕ64 → e → 𝑂 e) → SubstElem s e → SubstElem s e → 𝔹
eqSubstElem substE e₁ e₂ = interpSubstElem substE e₁ ≡ interpSubstElem substE e₂

compareSubstElem ∷ (Ord e) ⇒ (s ⇰ ℕ64 → e → 𝑂 e) → SubstElem s e → SubstElem s e → Ordering
compareSubstElem substE e₁ e₂ = interpSubstElem substE e₁ ⋚ interpSubstElem substE e₂

introSubstElem ∷ (Ord s) ⇒ s ⇰ ℕ64 → SubstElem s e → SubstElem s e
introSubstElem = alter substElemIntroL ∘ (+)

subSubstElem ∷ (s ⇰ ℕ64 → e → 𝑂 e) → SubstElem s e → SubstElem s e
subSubstElem substE e = SubstElem zero $ \ () → interpSubstElem substE e

-------------
-- FUNCTOR --
-------------

instance Functor (SubstElem s) where
  map f (SubstElem ι e) = SubstElem ι $ mapp f e

---------------------
-- PRETTY PRINTING --
---------------------

ppSubstElemNamed ∷ (Pretty e) ⇒ (s ⇰ ℕ64 → Doc) → SubstElem s e → Doc
ppSubstElemNamed ιD (SubstElem ι ueO) =
  let eD = elim𝑂 (const $ ppPun "⊥") pretty $ ueO ()
  in 
  if isEmpty ι
  then eD
  else ppInf pTOP (ppPun "⇈") eD $ ιD ι

instance (Pretty s,Pretty e) ⇒ Pretty (SubstElem s e) where
  pretty = ppSubstElemNamed pretty

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

ppSSubstElemNamed ∷ (Pretty e) ⇒ (s ⇰ ℕ64 → Doc) → SSubstElem s e → Doc
ppSSubstElemNamed ιD = \case
    Var_SSE i → ppDVar i
    Trm_SSE e → ppSubstElemNamed ιD e

instance (Pretty s,Pretty e) ⇒ Pretty (SSubstElem s e) where
  pretty = ppSSubstElemNamed pretty

-------------
-- FUZZING --
-------------

instance (Ord s,Fuzzy s,Fuzzy e) ⇒ Fuzzy (SSubstElem s e) where
  fuzzy = rchoose $ map const
    [ Var_SSE ^$ fuzzy
    , Trm_SSE ^$ fuzzy
    ]


