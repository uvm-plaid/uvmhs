module UVMHS.Lib.Substitution.Var where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Parser
import UVMHS.Lib.Rand
import UVMHS.Lib.Fuzzy
import UVMHS.Lib.Shrinky

import UVMHS.Lib.Substitution.Name

---------------------------------------------------------------------
-- ==== --
-- DVar --
-- ==== --
---------------------------------------------------------------------

-- De Bruijn Index
newtype DVar = DVar { unDVar ∷ ℕ64 }
  deriving (Eq,Ord,Show,Fuzzy,Shrinky)
makeLenses ''DVar

instance Pretty DVar where
  pretty (DVar n) = ppPun $ concat ["•:",show𝕊 n]

syntaxDVar ∷ LexerBasicSyntax
syntaxDVar = concat
  [ null { lexerBasicSyntaxPuns = pow ["•",":"] }
  ]

pDVarTail ∷ CParser TokenBasic DVar
pDVarTail = DVar ^$ cpNat64

pDVar ∷ CParser TokenBasic DVar
pDVar = do 
  void $ cpSyntax "•"
  void $ cpSyntax ":"
  pDVarTail

---------------------------------------------------------------------
-- ==== --
-- NVar --
-- ==== --
---------------------------------------------------------------------

-- Named variables with a De Bruijn index
-- λ x. λ x. x:0
--        └───┘
-- λ x. λ x. x:1
--   └────────┘
data NVar = NVar
  { nvarIndex ∷ DVar
  , nvarName  ∷ Name
  } deriving (Eq,Ord,Show)
makeLenses ''NVar

nvar_Name ∷ Name → NVar
nvar_Name = NVar $ DVar 0

name_NVarL ∷ NVar ⌲ Name
name_NVarL = prism nvar_Name $ \ (NVar n x) → if n ≡ DVar 0 then Some x else None

gensymNVar ∷ (Monad m,MonadState s m) ⇒ s ⟢ ℕ64 → 𝕊 → m NVar
gensymNVar ℓ s = nvar_Name ^$ gensymName ℓ s

instance Fuzzy NVar where 
  fuzzy = return NVar ⊡ fuzzy ⊡ fuzzy

instance Shrinky NVar where 
  shrink (NVar n x) = do
    (n',x') ← shrink (n,x)
    return $ NVar n' x'

instance Pretty NVar where
  pretty (NVar n x) = concat
    [ pretty x
    , if n ≡ DVar 0 then null else ppPun $ concat [":",show𝕊 $ unDVar n]
    ]

syntaxNVar ∷ LexerBasicSyntax
syntaxNVar = concat
  [ syntaxName
  , syntaxDVar
  ]

pNVarTail ∷ Name → CParser TokenBasic NVar
pNVarTail x = do
  n ← ifNone (DVar 0) ^$ cpOptional $ do
    void $ cpSyntax ":"
    pDVarTail
  return $ NVar n x

pNVar ∷ CParser TokenBasic NVar
pNVar = do
  x ← pName
  pNVarTail x

---------------------------------------------------------------------
-- ==== --
-- GVar --
-- ==== --
---------------------------------------------------------------------

-- Global Variables
newtype GVar = GVar { unGVar ∷ Name }
  deriving (Eq,Ord,Show,Fuzzy,Shrinky)
makeLenses ''GVar

instance Pretty GVar where
  pretty (GVar x) = concat [pretty x,ppPun ":g"]

syntaxGVar ∷ LexerBasicSyntax
syntaxGVar = concat
  [ syntaxName
  , null { lexerBasicSyntaxPuns = pow [":g"] }
  ]

pGVarTail ∷ Name → CParser TokenBasic GVar
pGVarTail x = do
  void $ cpSyntax ":g"
  return $ GVar x

pGVar ∷ CParser TokenBasic GVar
pGVar = do
  x ← pName
  pGVarTail x

---------------------------------------------------------------------
-- ==== --
-- SVar --
-- ==== --
---------------------------------------------------------------------

-- Scoped Variables: either De Bruijn scoped or Named scoped
data SVar =
    D_SVar DVar
  | N_SVar NVar
  deriving (Eq,Ord,Show)
makePrisms ''SVar
makePrettyUnion ''SVar

mkSVar ∷ SName → DVar → SVar
mkSVar xO n = case xO of
  D_SName → D_SVar n
  N_SName x → N_SVar $ NVar n x

svarName ∷ SVar → SName
svarName = \case
  D_SVar _          → D_SName
  N_SVar (NVar _ x) → N_SName x

svarLevel ∷ SVar → DVar
svarLevel = \case
  D_SVar n → n
  N_SVar (NVar n _x) → n

instance Fuzzy SVar where 
  fuzzy = rchoose
    [ \ () → D_SVar ^$ fuzzy
    , \ () → N_SVar ^$ fuzzy
    ]

instance Shrinky SVar where 
  shrink = \case
    D_SVar x → D_SVar ^$ shrink x
    N_SVar x → N_SVar ^$ shrink x

---------------------------------------------------------------------
-- === --
-- Var --
-- === --
---------------------------------------------------------------------

-- Variables: either De Bruijn, Named or Global
data Var =
    D_Var DVar
  | N_Var NVar
  | G_Var GVar
  deriving (Eq,Ord,Show)
makePrisms ''Var
makePrettyUnion ''Var

var_Name ∷ Name → Var
var_Name = N_Var ∘ nvar_Name

name_VarL ∷ Var ⌲ Name
name_VarL = name_NVarL ⊚ n_VarL

gensymVar ∷ (Monad m,MonadState s m) ⇒ s ⟢ ℕ64 → 𝕊 → m Var
gensymVar ℓ s = N_Var ^$ gensymNVar ℓ s

instance Fuzzy Var where
  fuzzy = rchoose
    [ \ () → D_Var ^$ fuzzy
    , \ () → N_Var ^$ fuzzy
    , \ () → G_Var ^$ fuzzy
    ]

instance Shrinky Var where
  shrink = \case
    D_Var x → D_Var ^$ shrink x
    N_Var x → N_Var ^$ shrink x
    G_Var x → G_Var ^$ shrink x

syntaxVar ∷ LexerBasicSyntax
syntaxVar = concat
  [ syntaxName
  , syntaxDVar
  , syntaxNVar
  , syntaxGVar
  ]

pVar ∷ CParser TokenBasic Var
pVar = concat
  [ D_Var ^$ pDVar
  , do x ← pName
       concat 
         [ N_Var ^$ pNVarTail x
         , G_Var ^$ pGVarTail x
         ]
  ]

---------------------------------------------------------------------
-- ======= --
-- DVarInf --
-- ======= --
---------------------------------------------------------------------

-- De Bruijn Variables with an extra "∞" element
data DVarInf =
    Var_DVI DVar
  | Inf_DVI
  deriving (Eq,Ord,Show)
makePrisms ''DVarInf

instance Pretty DVarInf where
  pretty = \case
    Var_DVI x → pretty x
    Inf_DVI   → ppPun "•:∞"

syntaxDVarInf ∷ LexerBasicSyntax
syntaxDVarInf = concat
  [ syntaxDVar
  , null { lexerBasicSyntaxPuns = pow ["INF","∞"] }
  ]

pDVarInfTail ∷ CParser  TokenBasic DVarInf
pDVarInfTail = concat
  [ Var_DVI ^$ pDVarTail
  , do void $ concat $ map cpSyntax ["INF","∞"]
       return Inf_DVI
  ]

pDVarInf ∷ CParser TokenBasic DVarInf
pDVarInf = do
  void $ cpSyntax "•"
  void $ cpSyntax ":"
  pDVarInfTail

---------------------------------------------------------------------
-- ======= --
-- NVarInf --
-- ======= --
---------------------------------------------------------------------

-- Named Variables where indices have an extra ∞ element
data NVarInf = NVarInf
  { nvarInfIndex ∷ DVarInf
  , nvarInfName  ∷ Name
  } deriving (Eq,Ord,Show)
makeLenses ''NVarInf

instance Pretty NVarInf where
  pretty (NVarInf n x) = concat
    [ pretty x
    , case n of
        Var_DVI n' | n' ≢ DVar 0 → ppBdr $ concat [":",show𝕊 $ unDVar n']
        _ → null
    ]

syntaxNVarInf ∷ LexerBasicSyntax
syntaxNVarInf = concat
  [ syntaxName
  , syntaxDVarInf
  ]

pNVarInfTail ∷ Name → CParser TokenBasic NVarInf
pNVarInfTail x = do
  void $ cpSyntax ":"
  n ← pDVarInfTail
  return $ NVarInf n x

pNVarInf ∷ CParser TokenBasic NVarInf
pNVarInf = do
  x ← pName
  pNVarInfTail x

---------------------------------------------------------------------
-- ====== --
-- VarInf --
-- ====== --
---------------------------------------------------------------------

data VarInf =
    D_VarInf DVarInf
  | N_VarInf NVarInf
  | G_VarInf GVar
  deriving (Eq,Ord,Show)
makePrisms ''VarInf
makePrettyUnion ''VarInf

syntaxVarInf ∷ LexerBasicSyntax
syntaxVarInf = concat
  [ syntaxDVarInf
  , syntaxNVarInf
  , syntaxGVar
  ]

pVarInf ∷ CParser TokenBasic VarInf
pVarInf = concat
  [ D_VarInf ^$ pDVarInf
  , do x ← pName
       concat
         [ N_VarInf ^$ pNVarInfTail x
         , G_VarInf ^$ pGVarTail x
         ]
  ]

---------------------------------------------------------------------
-- ======== --
-- SVarView --
-- ======== --
---------------------------------------------------------------------

class SVarView s e | e→s where
  svarL ∷ s → e ⌲ SVar

svarScopeL ∷ ∀ s e. (SVarView s e) ⇒ s → SName → e ⌲ DVar
svarScopeL s xO = 
  let ctor ∷ DVar → e
      ctor = case xO of
        D_SName → \ n → construct (svarL s) $ D_SVar n
        N_SName x → \ n → construct (svarL s) $ N_SVar $ NVar n x
      dtor ∷ e → 𝑂 DVar
      dtor = case xO of
        D_SName → \ e → view (d_SVarL ⊚ svarL s) e
        N_SName x → \ e → do
          NVar n x' ← view (n_SVarL ⊚ svarL s) e
          guard $ x ≡ x'
          return n
  in prism ctor dtor
