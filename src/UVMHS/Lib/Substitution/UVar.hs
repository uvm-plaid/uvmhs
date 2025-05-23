module UVMHS.Lib.Substitution.UVar where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Parser
import UVMHS.Lib.Rand
import UVMHS.Lib.Fuzzy
import UVMHS.Lib.Shrinky

import UVMHS.Lib.Substitution.Var
import UVMHS.Lib.Substitution.Subst

-- =================== --
-- UNIFIABLE VARIABLES --
-- =================== --

data 𝕐 s e =
    S_UVar 𝕏              -- scoped variable
  | M_UVar 𝕎 (Subst s e)  -- meta variable
  deriving (Eq,Ord,Show)
makePrisms ''𝕐

wfUVar ∷ (Ord s) ⇒ 𝕐 s e → 𝔹
wfUVar = \case
  S_UVar _x → True
  M_UVar _w 𝓈 → wfSubst 𝓈

duvarL ∷ 𝕐 s e ⌲ ℕ64
duvarL = d_SVarL ⊚ s_UVarL

duvar ∷ ℕ64 → 𝕐 s e
duvar = construct duvarL

nuvarL ∷ 𝕐 s e ⌲ ℕ64 ∧ 𝕎
nuvarL = n_SVarL ⊚ s_UVarL

nuvar ∷ ℕ64 → 𝕎 → 𝕐 s e
nuvar = uncurry $ construct nuvarL

znuvarL ∷ 𝕐 s e ⌲ 𝕎
znuvarL = znsvarL ⊚ s_UVarL

znuvar ∷ 𝕎 → 𝕐 s e
znuvar = construct znuvarL

guvarL ∷ 𝕐 s e ⌲ 𝕎
guvarL = g_SVarL ⊚ s_UVarL

guvar ∷ 𝕎 → 𝕐 s e
guvar = construct guvarL

gensymUVar ∷ (Monad m,MonadState s m) ⇒ s ⟢ ℕ64 → 𝕊 → m (𝕐 s e)
gensymUVar ℓ s = S_UVar ^$ gensymSVar ℓ s

-------------
-- FUNCTOR --
-------------

instance Functor (𝕐 s) where
  map _ (S_UVar x) = S_UVar x
  map f (M_UVar x 𝓈) = M_UVar x $ map f 𝓈

---------------------
-- PRETTY PRINTING --
---------------------

instance (Ord s,Pretty e, Pretty s) ⇒ Pretty (𝕐 s e) where
  pretty = \case
    S_UVar x → pretty x
    M_UVar x 𝓈 → concat [pretty x,ppPun ":m",ppGA $ pretty 𝓈]

-------------------------
-- FUZZY for Variables --
-------------------------

instance (Pretty e,Pretty s,Ord s,Fuzzy s,Fuzzy e) ⇒ Fuzzy (𝕐 s e) where
  fuzzy = do
    d ← askL fuzzyEnvDepthL
    wrchoose
      [ (:*) one $ \ () → S_UVar ^$ fuzzy
      , (:*) d $ \ () → return M_UVar ⊡ fuzzy ⊡ fuzzyRec fuzzy
      ]

