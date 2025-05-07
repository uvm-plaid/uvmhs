module UVMHS.Lib.Substitution.Var where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Parser
import UVMHS.Lib.Rand
import UVMHS.Lib.Fuzzy

-- =============== --
-- SIMPLE VARIABLE --
-- =============== --

data 𝕎 = 𝕎
  { markVar ∷ 𝑂 ℕ64
  , nameVar ∷ 𝕊
  } deriving (Eq,Ord,Show)
makeLenses ''𝕎

var ∷ 𝕊 → 𝕎
var = 𝕎 None

gensymVar ∷ (Monad m,MonadState s m) ⇒ s ⟢ ℕ64 → 𝕊 → m 𝕎
gensymVar ℓ s = do
  n ← nextL ℓ
  return $ 𝕎 (Some n) s

-------------
-- PARSING --
-------------

cpVar ∷ CParser TokenBasic 𝕎
cpVar = var ^$ cpShaped $ view nameTBasicL

cpVarWS ∷ CParser TokenWSBasic 𝕎
cpVarWS = var ^$ cpShaped $ view nameTWSBasicL

---------------------
-- PRETTY PRINTING --
---------------------

instance Pretty 𝕎 where
  pretty (𝕎 nO x) = concat
    [ ppString x
    , elim𝑂 null (\ n → ppPun $ concat ["#",show𝕊 n]) nO
    ]

-------------
-- FUZZING --
-------------

instance Fuzzy 𝕎 where
  fuzzy = do
    nO ← fuzzy
    return $ 𝕎 nO "x"

-- =============== --
-- SCOPED VARIABLE --
-- =============== --

data 𝕏 =
    D_SVar ℕ64    -- nameless variable
  | N_SVar ℕ64 𝕎  -- named (+ nameless index for that name)
                 -- λ x. λ x. x↑0
                 --        └───┘
                 -- λ x. λ x. x↑1
                 --   └────────┘
  | G_SVar 𝕎      -- global variable
  deriving (Eq,Ord,Show)
makePrisms ''𝕏

znsvar ∷ 𝕎 → 𝕏
znsvar = N_SVar 0

znsvarL ∷ 𝕏 ⌲ 𝕎
znsvarL = prism znsvar $ \case
  N_SVar n x | n≡0 → Some x
  _ → None

gensymSVar ∷ (Monad m,MonadState s m) ⇒ s ⟢ ℕ64 → 𝕊 → m 𝕏
gensymSVar ℓ s = znsvar ^$ gensymVar ℓ s

-------------
-- PARSING --
-------------

cpZNSVar ∷ CParser TokenBasic 𝕏
cpZNSVar = znsvar ∘ var ^$ cpShaped $ view nameTBasicL

cpGSVar ∷ CParser TokenBasic 𝕏
cpGSVar = G_SVar ∘ var ^$ cpShaped $ view nameTBasicL

cpNSVarWS ∷ CParser TokenWSBasic 𝕏
cpNSVarWS = znsvar ∘ var ^$ cpShaped $ view nameTWSBasicL

cpGSVarWS ∷ CParser TokenWSBasic 𝕏
cpGSVarWS = G_SVar ∘ var ^$ cpShaped $ view nameTWSBasicL

---------------------
-- PRETTY PRINTING --
---------------------

ppDVar ∷ ℕ64 → Doc
ppDVar n = concat [ppPun "⌊",pretty n,ppPun "⌋"]

ppNVar ∷ Doc → Doc → Doc
ppNVar n x = concat [x,ppPun "@",n]

instance Pretty 𝕏 where
  pretty = \case
    N_SVar n x → if n ≡ 0 then pretty x else ppNVar (pretty n) $ pretty x
    D_SVar n → ppDVar n
    G_SVar x → pretty x

-------------
-- FUZZING --
-------------

instance Fuzzy 𝕏 where
  fuzzy = wrchoose
    [ (:*) one $ \ () → D_SVar ^$ fuzzy
    , (:*) one $ \ () → return N_SVar ⊡ fuzzy ⊡ fuzzy
    , (:*) one $ \ () → G_SVar ^$ fuzzy
    ]

-- ======== --
-- SVarView --
-- ======== --

class SVarView s e | e→s where
  svarL ∷ s → e ⌲ 𝕏

svarScopeL ∷ ∀ s e. (SVarView s e) ⇒ s → 𝑂 𝕎 → e ⌲ ℕ64
svarScopeL s xO = 
  let ctor ∷ ℕ64 → e
      ctor = case xO of
        None → \ n → construct (svarL s) $ D_SVar n
        Some x → \ n → construct (svarL s) $ N_SVar n x
      dtor ∷ e → 𝑂 ℕ64
      dtor = case xO of
        None → \ e → view (d_SVarL ⊚ svarL s) e
        Some x → \ e → do
          n :* x' ← view (n_SVarL ⊚ svarL s) e
          guard $ x ≡ x'
          return n
  in prism ctor dtor
    
