module UVMHS.Lib.Substitution.Var where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Parser
import UVMHS.Lib.Rand

----------------------
-- SIMPLE VARIABLES --
----------------------

-- simple variables
data 𝕏 = 𝕏
  { 𝕩mark ∷ 𝑂 ℕ64
  , 𝕩name ∷ 𝕊
  } deriving (Eq,Ord,Show)
makeLenses ''𝕏

var ∷ 𝕊 → 𝕏
var = 𝕏 None

cpVar ∷ CParser TokenBasic 𝕏
cpVar = var ^$ cpShaped $ view nameTBasicL

cpVarWS ∷ CParser TokenWSBasic 𝕏
cpVarWS = var ^$ cpShaped $ view nameTWSBasicL

instance Pretty 𝕏 where
  pretty (𝕏 nO x) = concat
    [ ppString x
    , elim𝑂 null (\ n → ppPun $ concat ["#",show𝕊 n]) nO
    ]

instance Fuzzy 𝕏 where
  fuzzy = do
    nO ← fuzzy
    return $ 𝕏 nO "x"

ppDVar ∷ ℕ64 → Doc
ppDVar n = concat [ppPun "⌊",pretty n,ppPun "⌋"]

