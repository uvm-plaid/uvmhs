module UVMHS.Lib.Substitution.UVar where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Rand
import UVMHS.Lib.Fuzzy
import UVMHS.Lib.Shrinky

import UVMHS.Lib.Substitution.Name
import UVMHS.Lib.Substitution.Subst
import UVMHS.Lib.Substitution.Var

---------------------------------------------------------------------
-- ==== --
-- MVar --
-- ==== --
---------------------------------------------------------------------

data MVar s e = MVar
  { mvarSubst ∷ Subst s e
  , mvarName  ∷ Name
  } deriving (Eq,Ord,Show)
makeLenses ''MVar

-------------
-- FUNCTOR --
-------------

instance Functor (MVar s) where
  map f (MVar 𝓈 x) = MVar (map f 𝓈) x

------------------
-- WELL FOUNDED --
------------------

wfMVar ∷ (Ord s) ⇒ MVar s e → 𝔹
wfMVar = wfSubst ∘ mvarSubst

-----------
-- FUZZY --
-----------

instance (Ord s,Fuzzy s,Fuzzy e) ⇒ Fuzzy (MVar s e) where
  fuzzy = return MVar ⊡ fuzzy ⊡ fuzzy

instance (Ord s,Shrinky e) ⇒ Shrinky (MVar s e) where
  shrink (MVar 𝓈 x) = do
    (𝓈',x') ← shrink (𝓈,x)
    return $ MVar 𝓈' x'

---------------------
-- PRETTY PRINTING --
---------------------

instance (Ord s,Pretty s,Pretty e) ⇒ Pretty (MVar s e) where
  pretty (MVar 𝓈 x) = concat 
    [ pretty x
    , ppPun ":m"
    , if isNullSubst 𝓈 then null else ppGA $ pretty 𝓈
    ]

---------------------------------------------------------------------
-- ==== --
-- UVar --
-- ==== --
---------------------------------------------------------------------

data UVar s e =
    D_UVar DVar
  | N_UVar NVar
  | G_UVar GVar
  | M_UVar (MVar s e)
  deriving (Eq,Ord,Show)
makePrisms ''UVar
makePrettyUnion ''UVar

name_UVarL ∷ UVar s e ⌲ Name
name_UVarL = name_NVarL ⊚ n_UVarL

uvar_Name ∷ Name → UVar s e
uvar_Name = construct name_UVarL

uvar_SVar ∷ SVar → UVar s e
uvar_SVar = \case
  D_SVar x → D_UVar x
  N_SVar x → N_UVar x

svar_UVarL ∷ UVar s e ⌲ SVar
svar_UVarL = prism uvar_SVar $ \case
  D_UVar x → Some $ D_SVar x
  N_UVar x → Some $ N_SVar x
  _        → None

uvar_Var ∷ Var → UVar s e
uvar_Var = \case
  D_Var x → D_UVar x
  N_Var x → N_UVar x
  G_Var x → G_UVar x

var_UVarL ∷ UVar s e ⌲ Var
var_UVarL = prism uvar_Var $ \case
  D_UVar x → Some $ D_Var x
  N_UVar x → Some $ N_Var x
  G_UVar x → Some $ G_Var x
  _        → None
  

gensymUVar ∷ (Monad m,MonadState s m) ⇒ s ⟢ ℕ64 → 𝕊 → m (UVar s e)
gensymUVar ℓ s = N_UVar ^$ gensymNVar ℓ s

-------------
-- FUNCTOR --
-------------

instance Functor (UVar s) where
  map f = \case
    D_UVar x → D_UVar x
    N_UVar x → N_UVar x
    G_UVar x → G_UVar x
    M_UVar x → M_UVar $ map f x

------------------
-- WELL FOUNDED --
------------------

wfUVar ∷ (Ord s) ⇒ UVar s e → 𝔹
wfUVar = \case
  D_UVar _ → True
  N_UVar _ → True
  G_UVar _ → True
  M_UVar x → wfMVar x

---------------------
-- PRETTY PRINTING --
---------------------

-------------------------
-- FUZZY for Variables --
-------------------------

instance (Pretty e,Pretty s,Ord s,Fuzzy s,Fuzzy e) ⇒ Fuzzy (UVar s e) where
  fuzzy = rchoose
    [ \ () → D_UVar ^$ fuzzy
    , \ () → N_UVar ^$ fuzzy
    , \ () → G_UVar ^$ fuzzy
    , \ () → M_UVar ^$ fuzzy
    ]

