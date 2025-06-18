module UVMHS.Lib.Substitution.Name where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Parser
import UVMHS.Lib.Fuzzy
import UVMHS.Lib.Rand
import UVMHS.Lib.Shrinky

---------------------------------------------------------------------
-- ==== --
-- Name --
-- ==== --
---------------------------------------------------------------------

data Name = Name
  { nameMark ∷ 𝑂 ℕ64
  , nameName ∷ 𝕊
  } deriving (Eq,Ord,Show)
makeLenses ''Name

mkName ∷ 𝕊 → Name
mkName = Name None

gensymName ∷ (Monad m,MonadState s m) ⇒ s ⟢ ℕ64 → 𝕊 → m Name
gensymName ℓ s = do
  n ← nextL ℓ
  return $ Name (Some n) s

instance Fuzzy Name where
  fuzzy = do 
    nO ← fuzzy 
    return $ Name nO "x"

instance Shrinky Name where
  shrink = const null

instance Pretty Name where
  pretty (Name nO x) = concat
    [ ppString x
    , case nO of
        None → null
        Some n → ppPun $ concat ["#",show𝕊 n]
    ]

syntaxName ∷ Syntax
syntaxName = syntaxPuns ["#"]

pName ∷ Parser Name
pName = do
  x ← pTokName
  nO ← optional $ do
    pTokSyntax "#"
    pTokNat64
  return $ Name nO x

---------------------------------------------------------------------
-- ===== --
-- SName --
-- ===== --
---------------------------------------------------------------------

data SName =
    D_SName       -- De Bruijn Scope
  | N_SName Name  -- Named Scope
  deriving (Eq,Ord,Show)

instance Fuzzy SName where 
  fuzzy = rchoose
    [ \ () → return D_SName
    , \ () → N_SName ^$ fuzzy
    ]
instance Shrinky SName where 
  shrink = \case
    D_SName → null
    N_SName x → N_SName ^$ shrink x

instance Pretty SName where
  pretty = \case
    D_SName   → ppPun "•"
    N_SName x → pretty x

---------------------------------------------------------------------
-- ====== --
-- SGName --
-- ====== --
---------------------------------------------------------------------

data SGName =
    D_SGName       -- De Bruijn Scope
  | N_SGName Name  -- Named Scope
  | G_SGName Name  -- Global Scope
  deriving (Eq,Ord,Show)

instance Fuzzy SGName where 
  fuzzy = rchoose
    [ \ () → return D_SGName
    , \ () → N_SGName ^$ fuzzy
    , \ () → G_SGName ^$ fuzzy
    ]

instance Shrinky SGName where 
  shrink = \case
    D_SGName → null
    N_SGName x → N_SGName ^$ shrink x
    G_SGName x → G_SGName ^$ shrink x

instance Pretty SGName where
  pretty = \case
    D_SGName   → ppPun "•"
    N_SGName x → pretty x
    G_SGName x → pretty x
