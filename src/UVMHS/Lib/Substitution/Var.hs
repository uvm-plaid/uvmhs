module UVMHS.Lib.Substitution.Var where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Parser
import UVMHS.Lib.Rand

----------------------
-- SIMPLE VARIABLES --
----------------------

data 𝕎 = 𝕎
  { 𝕩mark ∷ 𝑂 ℕ64
  , 𝕩name ∷ 𝕊
  } deriving (Eq,Ord,Show)
makeLenses ''𝕎

var ∷ 𝕊 → 𝕎
var = 𝕎 None

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


