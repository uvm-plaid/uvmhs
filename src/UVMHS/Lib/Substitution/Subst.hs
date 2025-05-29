module UVMHS.Lib.Substitution.Subst where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Fuzzy
import UVMHS.Lib.Shrinky

import UVMHS.Lib.Substitution.Name
import UVMHS.Lib.Substitution.SubstElem
import UVMHS.Lib.Substitution.SubstScoped
import UVMHS.Lib.Substitution.SubstSpaced
import UVMHS.Lib.Substitution.Var

-- ===== --
-- SUBST --
-- ===== --

--------------------------------------------------------------------------------
-- Instances for `Null`, `Append` and `Monoid` are defined in the `Substy`
-- module.
--
-- If semantically you want to achieve "apply substitution 𝓈₁ to substitution
-- 𝓈₂", this can be done using those instances, simply by `𝓈₁ ⧺ 𝓈₂`, which is
-- also semantically equivalent to `subst 𝓈₁ ∘ subst 𝓈₂`.
--
-- See documentation for `SubstSpaced` for notes about how it is instantiated
-- in this context.
--------------------------------------------------------------------------------

newtype Subst s e = Subst { unSubst ∷ SubstSpaced (s ∧ Name) (s ∧ SName) e }
  deriving (Eq,Ord,Show,Fuzzy,Functor,Shrinky)
makeLenses ''Subst

isNullSubst ∷ (Ord s) ⇒ Subst s e → 𝔹
isNullSubst = isNullSubstSpaced ∘ unSubst

wfSubst ∷ (Ord s) ⇒ Subst s e → 𝔹
wfSubst = wfSubstSpaced ∘ unSubst

canonSubstWith ∷ (Ord s,Eq e) ⇒ (s ∧ SName → e ⌲ DVar) → (s ∧ SName ⇰ ℕ64 → e → 𝑂 e) → (e → e) → Subst s e → Subst s e
canonSubstWith ℓvar intro canonE (Subst 𝓈) = Subst $ canonSubstSpaced ℓvar intro canonE 𝓈

---------------------
-- SHIFT DE BRUIJN --
---------------------

dshiftsSubst ∷ (Ord s) ⇒ s ⇰ ℕ64 → Subst s e → Subst s e
dshiftsSubst = alter unSubstL ∘ shiftSubstSpaced ∘ assoc ∘ map (mapFst $ flip (:*) D_SName) ∘ iter

dshiftSubst ∷ (Ord s) ⇒ s → ℕ64 → Subst s e → Subst s e
dshiftSubst x ρ = dshiftsSubst $ x ↦ ρ

--------------------
-- INTRO DE BRUIJN --
--------------------

dintrosSubst ∷ (Ord s) ⇒ s ⇰ ℕ64 → Subst s e
dintrosSubst = Subst ∘ introSubstSpaced ∘ assoc ∘ map (mapFst $ flip (:*) D_SName) ∘ iter

dintroSubst ∷ (Ord s) ⇒ s → ℕ64 → Subst s e
dintroSubst s ι = dintrosSubst $ s ↦ ι

--------------------
-- BIND DE BRUIJN --
--------------------

dbindsSubst ∷ (Ord s) ⇒ s ⇰ 𝕍 e → Subst s e
dbindsSubst = Subst ∘ sbindsSubstSpaced ∘ assoc ∘ map (mapFst $ flip (:*) D_SName) ∘ iter

dbindSubst ∷ (Ord s) ⇒ s → e → Subst s e
dbindSubst s e = dbindsSubst $ s ↦ single e

-----------------
-- SHIFT NAMED --
-----------------

nshiftsSubst ∷ (Ord s) ⇒ s ⇰ Name ⇰ ℕ64 → Subst s e → Subst s e
nshiftsSubst ρ = alter unSubstL $ shiftSubstSpaced $ assoc $ do
  s :* xns ← iter ρ
  x :* n ← iter xns
  return $ s :* N_SName x :* n

nshiftSubst ∷ (Ord s) ⇒ s → Name → ℕ64 → Subst s e → Subst s e
nshiftSubst s x ρ = nshiftsSubst $ s ↦ x ↦ ρ

-----------------
-- INTRO NAMED --
-----------------

nintrosSubst ∷ (Ord s) ⇒ s ⇰ Name ⇰ ℕ64 → Subst s e
nintrosSubst ι = Subst $ introSubstSpaced $ assoc $ do
  s :* xns ← iter ι
  x :* n ← iter xns
  return $ s :* N_SName x :* n

nintroSubst ∷ (Ord s) ⇒ s → Name → ℕ64 → Subst s e
nintroSubst s x ι = nintrosSubst $ s ↦ x ↦ ι

-------------------
----------------
-- BIND NAMED --
----------------

nbindsSubst ∷ (Ord s) ⇒ s ⇰ Name ⇰ 𝕍 e → Subst s e
nbindsSubst swes = Subst $ sbindsSubstSpaced $ assoc $ do
  s :* xess ← iter swes
  x :* es ← iter xess
  return $ s :* N_SName x :* es

nbindSubst ∷ (Ord s) ⇒ s → Name → e → Subst s e
nbindSubst s x e = nbindsSubst $ s ↦ x ↦ single e

-----------------
-- BIND GLOBAL --
-----------------

gbindsSubst ∷ (Ord s) ⇒ s ⇰ Name ⇰ e → Subst s e
gbindsSubst sxes = Subst $ ubindsSubstSpaced $ assoc $ do
  s :* xes ← iter sxes
  x :* e ← iter xes
  return $ s :* x :* e

gbindSubst ∷ (Ord s) ⇒ s → Name → e → Subst s e
gbindSubst s x e = gbindsSubst $ s ↦ x ↦ e

------------
-- PRETTY --
------------

instance ∀ s e. (Ord s,Pretty s,Pretty e) ⇒ Pretty (Subst s e) where
  pretty ∷ Subst s e → Doc
  pretty (Subst (SubstSpaced 𝓈G 𝓈S)) = 
    let sD ∷ s ∧ SName ⇰ ℕ64 → Doc
        sD sιs = pretty $ map ppSet $ concat $ mapOn (iter sιs) $ \ (s :* xO :* n) → 
          (↦♭) s $ single𝐼 $ 
            case xO of
            D_SName → concat [ppPun "⇈",pretty n]
            N_SName x → concat [ppBdr $ ppshow x,ppPun "⇈",pretty n]
        xD ∷ SName → DVarInf → Doc
        xD xO n = case xO of
          D_SName → pretty n
          N_SName x → pretty $ NVarInf n x
    in 
    pretty $ map ppDict $ concat
      [ if csize 𝓈G ≡ 0 then null else 
          concat $ mapOn (iter 𝓈G) $ \ ((s :* x) :* e) →
            (↦♭) s $ single𝐼 $ 
              (concat [ppBdr $ ppshow x,ppPun ":",ppPun "g"]) 
              :* 
              (ppSubstElemNamed sD e)
      , if csize 𝓈S ≡ 0 then null else 
          concat $ mapOn (iter 𝓈S) $ \ (s :* xO :* 𝓈) →
            (↦♭) s $ ppSubstScopedWith sD (xD xO) 𝓈
      ]

-- ========== --
-- META SUBST --
-- ========== --

newtype MetaSubst s e = MetaSubst { unMetaSubst ∷ (s ∧ Name) ⇰ SubstElem (s ∧ SName) e }
  deriving (Eq,Ord,Show,Pretty,Fuzzy,Shrinky)
makeLenses ''MetaSubst

----------
-- BIND --
----------

mbindsSubst ∷ (Ord s) ⇒ s ⇰ Name ⇰ e → MetaSubst s e
mbindsSubst sxes = MetaSubst $ assoc $ do
  s :* xes ← iter sxes
  x :* e ← iter xes
  return $ (:*) (s :* x) $ SubstElem null $ Some e

mbindSubst ∷ (Ord s) ⇒ s → Name → e → MetaSubst s e
mbindSubst s x e = mbindsSubst $ s ↦ x ↦ e
