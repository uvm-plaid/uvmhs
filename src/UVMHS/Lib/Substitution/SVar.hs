module UVMHS.Lib.Substitution.SVar where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Parser
import UVMHS.Lib.Rand

import UVMHS.Lib.Substitution.SubstNameless
import UVMHS.Lib.Substitution.GSubst
import UVMHS.Lib.Substitution.Var

-- ========= --
-- VARIABLES --
-- ========= --


-- fancy variables
data 𝕐 s e =
    DVar ℕ64            -- de bruijn variable
  | NVar ℕ64 𝕎          -- named (+ de bruijn index for that name)
                        -- λ x. λ x. x↑0
                        --        └───┘
                        -- λ x. λ x. x↑1
                        --   └────────┘
  | GVar 𝕎              -- global variable
  | MVar 𝕎 (Subst s e)  -- meta variable
  deriving (Eq,Ord,Show)
makePrisms ''𝕐

instance Functor (𝕐 s) where
  map _ (DVar n) = DVar n
  map _ (NVar n x) = NVar n x
  map _ (GVar n) = GVar n
  map f (MVar x s) = MVar x (map f s)

nvar ∷ 𝕎 → 𝕐 s e
nvar = NVar 0

nvarL ∷ 𝕐 s e ⌲ 𝕎
nvarL = prism nvar $ \case
  NVar n x | n≡0 → Some x
  _ → None

gensymVar ∷ (Monad m,MonadState s m) ⇒ s ⟢ ℕ64 → 𝕊 → m 𝕎
gensymVar ℓ s = do
  n ← nextL ℓ
  return $ 𝕎 (Some n) s

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
