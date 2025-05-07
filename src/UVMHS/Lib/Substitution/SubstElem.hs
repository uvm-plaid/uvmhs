module UVMHS.Lib.Substitution.SubstElem where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Rand
import UVMHS.Lib.Fuzzy
import UVMHS.Lib.Parser
import UVMHS.Lib.Shrinky

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
  -- , substElemValue ∷ () → 𝑂 e  -- ^ delayed element
  , substelemValue ∷ 𝑂 e
  } deriving (Eq,Ord,Show)
makeLenses ''SubstElem

-- `substE ιs e`
-- ≡ 
-- first weaken `e` by `ιs`, and then optionally perform some substitution to
-- the result (e.g., just leaving it alone)
interpSubstElem ∷ (s ⇰ ℕ64 → e → 𝑂 e) → SubstElem s e → 𝑂 e
interpSubstElem substE (SubstElem ιs eO) = substE ιs *$ eO

canonSubstElem ∷ (s ⇰ ℕ64 → e → 𝑂 e) → SubstElem s e → SubstElem s e
canonSubstElem substE e = SubstElem null $ interpSubstElem substE e

eqSubstElem ∷ (Eq e) ⇒ (s ⇰ ℕ64 → e → 𝑂 e) → SubstElem s e → SubstElem s e → 𝔹
eqSubstElem substE e₁ e₂ = interpSubstElem substE e₁ ≡ interpSubstElem substE e₂

compareSubstElem ∷ (Ord e) ⇒ (s ⇰ ℕ64 → e → 𝑂 e) → SubstElem s e → SubstElem s e → Ordering
compareSubstElem substE e₁ e₂ = interpSubstElem substE e₁ ⋚ interpSubstElem substE e₂

introSubstElem ∷ (Ord s) ⇒ s ⇰ ℕ64 → SubstElem s e → SubstElem s e
introSubstElem = alter substElemIntroL ∘ (+)

substSubstElemE ∷ (s ⇰ ℕ64 → e → 𝑂 e) → SubstElem s e → 𝑂 e
substSubstElemE substE (SubstElem ιs e) = substE ιs *$ e

substSubstElem ∷ (s ⇰ ℕ64 → e → 𝑂 e) → SubstElem s e → SubstElem s e
substSubstElem substE = SubstElem null ∘ substSubstElemE substE

-------------
-- FUNCTOR --
-------------

instance Functor (SubstElem s) where
  map f (SubstElem ιs e) = SubstElem ιs $ f ^$ e

---------------------
-- PRETTY PRINTING --
---------------------

ppSubstElemNamed ∷ (Pretty e) ⇒ (s ⇰ ℕ64 → Doc) → SubstElem s e → Doc
ppSubstElemNamed ιD (SubstElem ιs eO) =
  let eD = elim𝑂 (const $ ppCon "⊥") pretty eO
  in
  if isEmpty ιs
  then eD
  else ppInf pTOP (ppPun "⇈") eD $ ιD ιs

instance (Pretty s,Pretty e) ⇒ Pretty (SubstElem s e) where
  pretty = ppSubstElemNamed pretty

-------------
-- FUZZING --
-------------

instance (Ord s,Fuzzy s,Fuzzy e) ⇒ Fuzzy (SubstElem s e) where
  fuzzy = return SubstElem ⊡ fuzzy ⊡ fuzzy

---------------
-- SHRINKING --
---------------

instance (Shrinky e) ⇒ Shrinky (SubstElem s e) where
  shrink (SubstElem ιs eO) = SubstElem ιs ^$ shrink eO

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

mkSSubstElem ∷ e ⌲ ℕ64 → 𝑂 e → SSubstElem s e
mkSSubstElem ℓvar eO = case view (ℓvar ⊚ someL) eO of
  Some n → Var_SSE n
  None → Trm_SSE $ SubstElem null eO

interpSSubstElem ∷ e ⌲ ℕ64 → (s ⇰ ℕ64 → e → 𝑂 e) → SSubstElem s e → 𝑂 e
interpSSubstElem ℓvar substE = \case
  Var_SSE i → Some $ construct ℓvar i
  Trm_SSE e → interpSubstElem substE e

canonSSubstElem ∷ e ⌲ ℕ64 → (s ⇰ ℕ64 → e → 𝑂 e) → SSubstElem s e → SSubstElem s e
canonSSubstElem ℓvar substE = \case
  Var_SSE n → Var_SSE n
  Trm_SSE e → mkSSubstElem ℓvar $ interpSubstElem substE e

eqSSubstElem ∷ (Eq e) ⇒ e ⌲ ℕ64 → (s ⇰ ℕ64 → e → 𝑂 e) → SSubstElem s e → SSubstElem s e → 𝔹
eqSSubstElem ℓvar substE e₁ e₂ = interpSSubstElem ℓvar substE e₁ ≡ interpSSubstElem ℓvar substE e₂

compareSSubstElem ∷ (Ord e) ⇒ e ⌲ ℕ64 → (s ⇰ ℕ64 → e → 𝑂 e) → SSubstElem s e → SSubstElem s e → Ordering
compareSSubstElem ℓvar substE e₁ e₂ = interpSSubstElem ℓvar substE e₁ ⋚ interpSSubstElem ℓvar substE e₂

introSSubstElem ∷ (Ord s) ⇒ s → s ⇰ ℕ64 → SSubstElem s e → SSubstElem s e
introSSubstElem s ιs = \case
  Var_SSE n → Var_SSE $ n + ifNone 0 (ιs ⋕? s)
  Trm_SSE e → Trm_SSE $ introSubstElem ιs e

substSSubstElem ∷ e ⌲ ℕ64 → (s ⇰ ℕ64 → e → 𝑂 e) → SSubstElem s e → SSubstElem s e
substSSubstElem ℓvar substE = \case
  Var_SSE n → mkSSubstElem ℓvar $ substE null $ construct ℓvar n
  Trm_SSE e → mkSSubstElem ℓvar $ substSubstElemE substE e

-------------
-- FUNCTOR --
-------------

instance Functor (SSubstElem s) where
  map f = \case
    Var_SSE n → Var_SSE n
    Trm_SSE e → Trm_SSE $ map f e

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

---------------
-- SHRINKING --
---------------

instance (Shrinky e) ⇒ Shrinky (SSubstElem s e) where
  shrink = \case
    Var_SSE _i → null
    Trm_SSE e → Trm_SSE ^$ shrink e
