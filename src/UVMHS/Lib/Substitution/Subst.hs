module UVMHS.Lib.Substitution.Subst where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Fuzzy
import UVMHS.Lib.Shrinky

import UVMHS.Lib.Substitution.SubstElem
import UVMHS.Lib.Substitution.SubstSpaced
import UVMHS.Lib.Substitution.SubstScoped
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

newtype Subst s e = Subst { unSubst ∷ SubstSpaced (s ∧ 𝕎) (s ∧ 𝑂 𝕎) e }
  deriving (Eq,Ord,Show,Fuzzy,Functor)
makeLenses ''Subst

canonSubstWith ∷ (Ord s,Eq e) ⇒ (s ∧ 𝑂 𝕎 → e ⌲ ℕ64) → (s ∧ 𝑂 𝕎 ⇰ ℕ64 → e → 𝑂 e) → Subst s e → Subst s e
canonSubstWith ℓvar intro (Subst 𝓈) = Subst $ canonSubstSpaced ℓvar intro 𝓈

--------------------
-- SHIFT NAMELESS --
--------------------

-- s     = (name)spaced
-- d     = nameless (scoped) (i.e., de bruijn)
-- shift = "going under a binder"
sdshiftSubst ∷ (Ord s) ⇒ s ⇰ ℕ64 → Subst s e → Subst s e
sdshiftSubst = alter unSubstL ∘ shiftSubstSpaced ∘ assoc ∘ map (mapFst $ flip (:*) None) ∘ iter

-- d     = nameless (scoped) (i.e., de bruijn)
-- shift = "going under a binder"
dshiftSubst ∷ ℕ64 → Subst () e → Subst () e
dshiftSubst = sdshiftSubst ∘ (↦) ()

-----------------
-- SHIFT NAMED --
-----------------

-- s     = (name)spaced
-- n     = named (scoped)
-- shift = "going under a binder"
snshiftSubst ∷ (Ord s) ⇒ s ⇰ 𝕎 ⇰ ℕ64 → Subst s e → Subst s e
snshiftSubst 𝑠 = alter unSubstL $ shiftSubstSpaced $ assoc $ do
  s :* xns ← iter 𝑠
  x :* n ← iter xns
  return $ s :* Some x :* n

-- n     = named (scoped)
-- shift = "going under a binder"
nshiftSubst ∷ 𝕎 ⇰ ℕ64 → Subst () e → Subst () e
nshiftSubst = snshiftSubst ∘ (↦) ()

--------------------
-- INTRO NAMELESS --
--------------------

-- s     = (name)spaced
-- d     = nameless (scoped) (i.e., de bruijn)
-- intro = "a new variable has been introduced"
sdintroSubst ∷ (Ord s) ⇒ s ⇰ ℕ64 → Subst s e
sdintroSubst = Subst ∘ introSubstSpaced ∘ assoc ∘ map (mapFst $ flip (:*) None) ∘ iter

-- d     = nameless (scoped) (i.e., de bruijn)
-- intro = "a new variable has been introduced"
dintroSubst ∷ ℕ64 → Subst () e
dintroSubst = sdintroSubst ∘ (↦) ()

-- s     = (name)spaced
-- d     = nameless (scoped) (i.e., de bruijn)
-- z     = allow negative increment
-- intro = "a new variable has been introduced"
sdzintroSubst ∷ (Ord s) ⇒ s ⇰ ℤ64 → Subst s e
sdzintroSubst = Subst ∘ zintroSubstSpaced ∘ assoc ∘ map (mapFst $ flip (:*) None) ∘ iter

-- d     = nameless (scoped) (i.e., de bruijn)
-- z     = allow negative increment
-- intro = "a new variable has been introduced"
-- intro = "a new variable has been introduced"
dzintroSubst ∷ ℤ64 → Subst () e
dzintroSubst = sdzintroSubst ∘ (↦) ()

-----------------
-- INTRO NAMED --
-----------------

-- s     = (name)spaced
-- d     = named (scoped)
-- intro = "a new variable has been introduced"
snintroSubst ∷ (Ord s) ⇒ s ⇰ 𝕎 ⇰ ℕ64 → Subst s e
snintroSubst 𝑠 = Subst $ introSubstSpaced $ assoc $ do
  s :* xns ← iter 𝑠
  x :* n ← iter xns
  return $ s :* Some x :* n

-- d     = named (scoped)
-- intro = "a new variable has been introduced"
nintroSubst ∷ 𝕎 ⇰ ℕ64 → Subst () e
nintroSubst = snintroSubst ∘ (↦) ()

-- s     = (name)spaced
-- d     = named (scoped)
-- z     = allow negative increment
-- intro = "a new variable has been introduced"
snzintroSubst ∷ (Ord s) ⇒ s ⇰ 𝕎 ⇰ ℤ64 → Subst s e
snzintroSubst 𝑠 = Subst $ zintroSubstSpaced $ assoc $ do
  s :* xis ← iter 𝑠
  x :* i ← iter xis
  return $ s :* Some x :* i

-- d     = named (scoped)
-- z     = allow negative increment
-- intro = "a new variable has been introduced"
nzintroSubst ∷ 𝕎 ⇰ ℤ64 → Subst () e
nzintroSubst = snzintroSubst ∘ (↦) ()

----------
-- BIND --
----------

-- s     = (name)spaced
-- d     = nameless (scoped) (i.e., de bruijn)
-- binds = "substitute indices 0..n with elements from this vector"
sdbindsSubst ∷ (Ord s) ⇒ s ⇰ 𝕍 e → Subst s e
sdbindsSubst = Subst ∘ sbindsSubstSpaced ∘ assoc ∘ map (mapFst $ flip (:*) None) ∘ iter

-- d     = nameless (scoped) (i.e., de bruijn)
-- binds = "substitute indices 0..n with elements from this vector"
dbindsSubst ∷ 𝕍 e → Subst () e
dbindsSubst = sdbindsSubst ∘ (↦) ()

-- s    = (name)spaced
-- d    = nameless (scoped) (i.e., de bruijn)
-- bind = "substitute index 0 with this element
sdbindSubst ∷ (Ord s) ⇒ s → e → Subst s e
sdbindSubst s e = sdbindsSubst $ s ↦ single e

-- d    = nameless (scoped) (i.e., de bruijn)
-- bind = "substitute index 0 with this element
dbindSubst ∷ e → Subst () e
dbindSubst = sdbindSubst ()

-- s     = (name)spaced
-- n     = named (scoped)
-- binds = "substitute variables with elements from this map"
snbindsSubst ∷ (Ord s) ⇒ s ⇰ 𝕎 ⇰ 𝕍 e → Subst s e
snbindsSubst 𝑠 = Subst $ sbindsSubstSpaced $ assoc $ do
  s :* xess ← iter 𝑠
  x :* es ← iter xess
  return $ s :* Some x :* es

-- n     = named (scoped)
-- binds = "substitute variables with elements from this map"
nbindsSubst ∷ 𝕎 ⇰ 𝕍 e → Subst () e
nbindsSubst = snbindsSubst ∘ (↦) ()

-- s    = (name)spaced
-- n    = named (scoped)
-- bind = "substitute this variable with this element
snbindSubst ∷ (Ord s) ⇒ s → 𝕎 → e → Subst s e
snbindSubst s x e = snbindsSubst $ s ↦ x ↦ single e

-- n    = named (scoped)
-- bind = "substitute this variable with this element
nbindSubst ∷ 𝕎 → e → Subst () e
nbindSubst = snbindSubst ()

-- s     = (name)spaced
-- g     = global (unscoped)
-- binds = "substitute variables with elements from this vector"
sgbindsSubst ∷ (Ord s) ⇒ s ⇰ 𝕎 ⇰ e → Subst s e
sgbindsSubst sxes = Subst $ ubindsSubstSpaced $ assoc $ do
  s :* xes ← iter sxes
  x :* e ← iter xes
  return $ s :* x :* e

-- g     = global (unscoped)
-- binds = "substitute variables with elements from this vector"
gbindsSubst ∷ 𝕎 ⇰ e → Subst () e
gbindsSubst = sgbindsSubst ∘ (↦) ()

-- s    = (name)spaced
-- g    = global (unscoped)
-- bind = "substitute this variable with this element
sgbindSubst ∷ (Ord s) ⇒ s → 𝕎 → e → Subst s e
sgbindSubst s x e = sgbindsSubst $ s ↦ x ↦ e

-- g    = global (unscoped)
-- bind = "substitute this variable with this element
gbindSubst ∷ 𝕎 → e → Subst () e
gbindSubst = sgbindSubst ()

------------
-- PRETTY --
------------

instance ∀ s e. (Ord s,Pretty s,Pretty e) ⇒ Pretty (Subst s e) where
  pretty ∷ Subst s e → Doc
  pretty (Subst (SubstSpaced 𝓈U 𝓈S)) = 
    let sD ∷ s ∧ 𝑂 𝕎 ⇰ ℕ64 → Doc
        sD sιs = pretty $ map ppSet $ concat $ mapOn (iter sιs) $ \ (s :* xO :* n) → 
          (↦♭) s $ single𝐼 $ case xO of
            Some x → concat [ppBdr $ ppshow x,ppPun "⇈",pretty n]
            None → concat [ppPun "⇈",pretty n]
        xD ∷ 𝑂 𝕎 → 𝕊 → Doc
        xD xO n = case xO of
          None → ppBdr n
          Some x → concat [ppBdr (ppshow x),ppPun ":",ppBdr n]
    in 
    ppDict $ concat
      [ if csize 𝓈U ≡ 0 then null𝐼 else 
          single𝐼 $ (:*) (ppCon "𝐔") $ pretty $ map ppDict $ concat $ mapOn (iter 𝓈U) $ \ ((s :* x) :* e) →
            (↦♭) s $ single𝐼 $ (ppBdr $ ppshow x) :* (ppSubstElemNamed sD e)
      , if csize 𝓈S ≡ 0 then null𝐼 else 
          single𝐼 $ (:*) (ppCon "𝐒") $ pretty $ concat $ mapOn (iter 𝓈S) $ \ (s :* xO :* 𝓈) →
            (↦♭) s $ ppSubstScoped sD (xD xO) 𝓈
      ]

-- ========== --
-- META SUBST --
-- ========== --

newtype MetaSubst s e = MetaSubst { unMetaSubst ∷ (s ∧ 𝕎) ⇰ SubstElem (s ∧ 𝑂 𝕎) e }
  deriving (Eq,Ord,Show,Pretty,Fuzzy,Shrinky)
makeLenses ''MetaSubst

----------
-- BIND --
----------

-- s     = (name)spaced
-- m     = metavar (unscoped)
-- binds = "substitute variables with elements from this vector"
smbindsSubst ∷ (Ord s) ⇒ s ⇰ 𝕎 ⇰ e → MetaSubst s e
smbindsSubst sxes = MetaSubst $ assoc $ do
  s :* xes ← iter sxes
  x :* e ← iter xes
  return $ (:*) (s :* x) $ SubstElem null $ Some e

-- m     = metavar (unscoped)
-- binds = "substitute variables with elements from this vector"
mbindsSubst ∷ 𝕎 ⇰ e → MetaSubst () e
mbindsSubst = smbindsSubst ∘ (↦) ()

-- s    = (name)spaced
-- m    = metavar (unscoped)
-- bind = "substitute this variable with this element
smbindSubst ∷ (Ord s) ⇒ s → 𝕎 → e → MetaSubst s e
smbindSubst s x e = smbindsSubst $ s ↦ x ↦ e

-- m    = metavar (unscoped)
-- bind = "substitute this variable with this element
mbindSubst ∷ 𝕎 → e → MetaSubst () e
mbindSubst = smbindSubst ()

