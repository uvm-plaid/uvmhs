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

newtype Subst s e = Subst { unSubst ∷ SubstSpaced (s ∧ Name) (s ∧ 𝑂 Name) e }
  deriving (Eq,Ord,Show,Fuzzy,Functor,Shrinky)
makeLenses ''Subst

wfSubst ∷ (Ord s) ⇒ Subst s e → 𝔹
wfSubst = wfSubstSpaced ∘ unSubst

canonSubstWith ∷ (Ord s,Eq e) ⇒ (s ∧ 𝑂 Name → e ⌲ ℕ64) → (s ∧ 𝑂 Name ⇰ ℕ64 → e → 𝑂 e) → (e → e) → Subst s e → Subst s e
canonSubstWith ℓvar intro canonE (Subst 𝓈) = Subst $ canonSubstSpaced ℓvar intro canonE 𝓈

--------------------
-- SHIFT NAMELESS --
--------------------

-- shift = "going under binders"
-- D     = de bruijn (nameless)
-- Ss    = many scopes
shiftDSsSubst ∷ (Ord s) ⇒ s ⇰ ℕ64 → Subst s e → Subst s e
shiftDSsSubst = alter unSubstL ∘ shiftSubstSpaced ∘ assoc ∘ map (mapFst $ flip (:*) None) ∘ iter

-- shift = "going under binders"
-- D     = de bruijn (nameless)
-- S     = one scope
shiftDSSubst ∷ (Ord s) ⇒ s → ℕ64 → Subst s e → Subst s e
shiftDSSubst = shiftDSsSubst ∘∘ (↦)

-- shift = "going under binders"
-- D     = de bruijn (nameless)
shiftDSubst ∷ ℕ64 → Subst () e → Subst () e
shiftDSubst = shiftDSSubst ()

-----------------
-- SHIFT NAMED --
-----------------

-- shift = "going under binders"
-- Ns    = many names
-- Ss    = many scopes
shiftNsSsSubst ∷ (Ord s) ⇒ s ⇰ Name ⇰ ℕ64 → Subst s e → Subst s e
shiftNsSsSubst ρ = alter unSubstL $ shiftSubstSpaced $ assoc $ do
  s :* xns ← iter ρ
  x :* n ← iter xns
  return $ s :* Some x :* n

-- shift = "going under binders"
-- Ns    = many names
-- S     = one scope
shiftNsSSubst ∷ (Ord s) ⇒ s → Name ⇰ ℕ64 → Subst s e → Subst s e
shiftNsSSubst = shiftNsSsSubst ∘∘ (↦)

-- shift = "going under binders"
-- Ns    = many names
shiftNsSubst ∷ Name ⇰ ℕ64 → Subst () e → Subst () e
shiftNsSubst = shiftNsSSubst ()

-- shift = "going under binders"
-- N     = one name
-- S     = one scope
shiftNSSubst ∷ (Ord s) ⇒ s → Name → ℕ64 → Subst s e → Subst s e
shiftNSSubst s = shiftNsSSubst s ∘∘ (↦)

-- shift = "going under binders"
-- N     = one name
shiftNSubst ∷ Name → ℕ64 → Subst () e → Subst () e
shiftNSubst = shiftNSSubst ()

--------------------
-- INTRO NAMELESS --
--------------------

-- intro = "new variables have been introduced"
-- D      = de bruijn (nameless)
-- Ss     = many scopes
introDSsSubst ∷ (Ord s) ⇒ s ⇰ ℕ64 → Subst s e
introDSsSubst = Subst ∘ introSubstSpaced ∘ assoc ∘ map (mapFst $ flip (:*) None) ∘ iter

-- intro = "new variables have been introduced"
-- D      = de bruijn (nameless)
-- S      = one scope
introDSSubst ∷ (Ord s) ⇒ s → ℕ64 → Subst s e
introDSSubst = introDSsSubst ∘∘ (↦)

-- intro = "new variables have been introduced"
-- D     = de bruijn (nameless)
introDSubst ∷ ℕ64 → Subst () e
introDSubst = introDSSubst ()

-----------------
-- INTRO NAMED --
-----------------

-- intro = "new variables have been introduced"
-- Ns    = many names
-- Ss    = many scopes
introNsSsSubst ∷ (Ord s) ⇒ s ⇰ Name ⇰ ℕ64 → Subst s e
introNsSsSubst ι = Subst $ introSubstSpaced $ assoc $ do
  s :* xns ← iter ι
  x :* n ← iter xns
  return $ s :* Some x :* n

-- intro = "new variables have been introduced"
-- Ns    = many names
-- S     = one scope
introNsSSubst ∷ (Ord s) ⇒ s → Name ⇰ ℕ64 → Subst s e
introNsSSubst = introNsSsSubst ∘∘ (↦)

-- intro = "new variables have been introduced"
-- Ns    = many names
introNsSubst ∷ Name ⇰ ℕ64 → Subst () e
introNsSubst = introNsSSubst ()

-- intro = "new variables have been introduced"
-- N     = many names
-- S     = one scope
introNSSubst ∷ (Ord s) ⇒ s → Name → ℕ64 → Subst s e
introNSSubst s = introNsSSubst s ∘∘ (↦)

-- intro = "new variables have been introduced"
-- N     = many names
introNSubst ∷ Name → ℕ64 → Subst () e
introNSubst = introNSSubst ()

-------------------
-- BIND NAMELESS --
-------------------

-- binds = "substitute many elements"
-- D     = de bruijn (nameless)
-- Ss    = many scopes
bindsDSsSubst ∷ (Ord s) ⇒ s ⇰ 𝕍 e → Subst s e
bindsDSsSubst = Subst ∘ sbindsSubstSpaced ∘ assoc ∘ map (mapFst $ flip (:*) None) ∘ iter

-- binds = "substitute many elements"
-- D     = de bruijn (nameless)
-- S     = one scope
bindsDSSubst ∷ (Ord s) ⇒ s → 𝕍 e → Subst s e
bindsDSSubst = bindsDSsSubst ∘∘ (↦)

-- binds = "substitute many elements"
-- D     = de bruijn (nameless)
bindsDSubst ∷ 𝕍 e → Subst () e
bindsDSubst = bindsDSSubst ()

-- bind = "substitute an element"
-- D    = de bruijn (nameless)
-- Ss   = many scopes
bindDSsSubst ∷ (Ord s) ⇒ s ⇰ e → Subst s e
bindDSsSubst = bindsDSsSubst ∘ map single

-- bind = "substitute an element"
-- D    = de bruijn (nameless)
-- S    = one scope
bindDSSubst ∷ (Ord s) ⇒ s → e → Subst s e
bindDSSubst = bindDSsSubst ∘∘ (↦)

-- bind = "substitute an element"
-- D    = de bruijn (nameless)
bindDSubst ∷ e → Subst () e
bindDSubst = bindDSSubst ()

----------------
-- BIND NAMED --
----------------

-- binds = "substitute many elements"
-- Ns    = many names
-- Ss    = many scopes
bindsNsSsSubst ∷ (Ord s) ⇒ s ⇰ Name ⇰ 𝕍 e → Subst s e
bindsNsSsSubst swes = Subst $ sbindsSubstSpaced $ assoc $ do
  s :* xess ← iter swes
  x :* es ← iter xess
  return $ s :* Some x :* es

-- binds = "substitute many elements"
-- Ns    = many names
-- S     = one scopes
bindsNsSSubst ∷ (Ord s) ⇒ s → Name ⇰ 𝕍 e → Subst s e
bindsNsSSubst = bindsNsSsSubst ∘∘ (↦)

-- binds = "substitute many elements"
-- Ns    = many names
bindsNsSubst ∷ Name ⇰ 𝕍 e → Subst () e
bindsNsSubst = bindsNsSSubst ()

-- binds = "substitute many elements"
-- N     = one name
-- S     = one scopes
bindsNSSubst ∷ (Ord s) ⇒ s → Name → 𝕍 e → Subst s e
bindsNSSubst s = bindsNsSSubst s ∘∘ (↦)

-- binds = "substitute many elements"
-- N     = one name
bindsNSubst ∷ Name → 𝕍 e → Subst () e
bindsNSubst = bindsNSSubst ()

-- bind = "substitute an element"
-- Ns   = many names
-- Ss   = many scopes
bindNsSsSubst ∷ (Ord s) ⇒ s ⇰ Name ⇰ e → Subst s e
bindNsSsSubst = bindsNsSsSubst ∘ mapp single

-- bind  = "substitute an element"
-- Ns    = many names
-- S     = one scopes
bindNsSSubst ∷ (Ord s) ⇒ s → Name ⇰ e → Subst s e
bindNsSSubst = bindNsSsSubst ∘∘ (↦)

-- bind  = "substitute an element"
-- Ns    = many names
bindNsSubst ∷ Name ⇰ e → Subst () e
bindNsSubst = bindNsSSubst ()

-- bind  = "substitute an element"
-- N     = one name
-- S     = one scopes
bindNSSubst ∷ (Ord s) ⇒ s → Name → e → Subst s e
bindNSSubst s = bindNsSSubst s ∘∘ (↦)

-- bind  = "substitute an element"
-- N     = one name
bindNSubst ∷ Name → e → Subst () e
bindNSubst = bindNSSubst ()

-----------------
-- BIND GLOBAL --
-----------------

-- bind = "substitute an element"
-- Gs   = many global names
-- Ss   = many scopes
bindGsSsSubst ∷ (Ord s) ⇒ s ⇰ Name ⇰ e → Subst s e
bindGsSsSubst sxes = Subst $ ubindsSubstSpaced $ assoc $ do
  s :* xes ← iter sxes
  x :* e ← iter xes
  return $ s :* x :* e

-- bind = "substitute an element"
-- Gs   = many global names
-- S    = one scope
bindGsSSubst ∷ (Ord s) ⇒ s → Name ⇰ e → Subst s e
bindGsSSubst = bindGsSsSubst ∘∘ (↦)

-- bind = "substitute an element"
-- Gs   = many global names
bindGsSubst ∷ Name ⇰ e → Subst () e
bindGsSubst = bindGsSsSubst ∘ (↦) ()

-- bind = "substitute an element"
-- G    = one global name
-- S    = one scope
bindGSSubst ∷ (Ord s) ⇒ s → Name → e → Subst s e
bindGSSubst s x e = bindGsSsSubst $ s ↦ x ↦ e

-- bind = "substitute an element"
-- G    = one global name
bindGSubst ∷ Name → e → Subst () e
bindGSubst = bindGSSubst ()

------------
-- PRETTY --
------------

instance ∀ s e. (Ord s,Pretty s,Pretty e) ⇒ Pretty (Subst s e) where
  pretty ∷ Subst s e → Doc
  pretty (Subst (SubstSpaced 𝓈U 𝓈S)) = 
    let sD ∷ s ∧ 𝑂 Name ⇰ ℕ64 → Doc
        sD sιs = pretty $ map ppSet $ concat $ mapOn (iter sιs) $ \ (s :* xO :* n) → 
          (↦♭) s $ single𝐼 $ case xO of
            Some x → concat [ppBdr $ ppshow x,ppPun "⇈",pretty n]
            None → concat [ppPun "⇈",pretty n]
        xD ∷ 𝑂 Name → 𝕊 → Doc
        xD xO n = concat
          [ elim𝑂 (const null) (ppBdr ∘ ppshow) xO
          , ppPun ":"
          , ppBdr n
          ]
    in 
    pretty $ map ppDict $ concat
      [ if csize 𝓈U ≡ 0 then null else 
          concat $ mapOn (iter 𝓈U) $ \ ((s :* x) :* e) →
            (↦♭) s $ single𝐼 $ 
              (concat [ppBdr $ ppshow x,ppPun ":",ppPun "g"]) 
              :* 
              (ppSubstElemNamed sD e)
      , if csize 𝓈S ≡ 0 then null else 
          concat $ mapOn (iter 𝓈S) $ \ (s :* xO :* 𝓈) →
            (↦♭) s $ ppSubstScoped sD (xD xO) 𝓈
      ]

-- ========== --
-- META SUBST --
-- ========== --

newtype MetaSubst s e = MetaSubst { unMetaSubst ∷ (s ∧ Name) ⇰ SubstElem (s ∧ 𝑂 Name) e }
  deriving (Eq,Ord,Show,Pretty,Fuzzy,Shrinky)
makeLenses ''MetaSubst

----------
-- BIND --
----------

-- bind = "substitute an element"
-- Ms   = many meta names
-- Ss   = many scopes
bindMsSsSubst ∷ (Ord s) ⇒ s ⇰ Name ⇰ e → MetaSubst s e
bindMsSsSubst sxes = MetaSubst $ assoc $ do
  s :* xes ← iter sxes
  x :* e ← iter xes
  return $ (:*) (s :* x) $ SubstElem null $ Some e

-- bind = "substitute an element"
-- Ms   = many meta names
-- S    = one scope
bindMsSSubst ∷ (Ord s) ⇒ s → Name ⇰ e → MetaSubst s e
bindMsSSubst = bindMsSsSubst ∘∘ (↦)

-- bind = "substitute an element"
-- Ms   = many meta names
bindMsSubst ∷ Name ⇰ e → MetaSubst () e
bindMsSubst = bindMsSSubst ()

-- bind = "substitute an element"
-- M    = one meta name
-- S    = one scope
bindMSSubst ∷ (Ord s) ⇒ s → Name → e → MetaSubst s e
bindMSSubst s x e = bindMsSsSubst $ s ↦ x ↦ e

-- bind = "substitute an element"
-- M    = one meta name
bindMSubst ∷ Name → e → MetaSubst () e
bindMSubst = bindMSSubst ()
