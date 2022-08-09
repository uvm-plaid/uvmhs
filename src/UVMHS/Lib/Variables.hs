module UVMHS.Lib.Variables where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Parser

-- ========= --
-- VARIABLES --
-- ========= --

-- simple variables
data 𝕏 = 𝕏
  { 𝕩mark ∷ 𝑂 ℕ64
  , 𝕩name ∷ 𝕊
  } deriving (Eq,Ord,Show)
makeLenses ''𝕏

-- fancy variables
data 𝕐 = 
    NVar ℤ64 𝕏
  | DVar ℤ64
  | MVar 𝕏
  deriving (Eq,Ord,Show)
makePrisms ''𝕐

var ∷ 𝕊 → 𝕏
var = 𝕏 None

svar ∷ 𝕏 → 𝕐
svar = NVar 0

svarL ∷ 𝕐 ⌲ 𝕏
svarL = prism svar $ \case
  NVar n x | n≡0 → Some x
  _ → None

instance Pretty 𝕏 where
  pretty (𝕏 nO x) = concat
    [ ppString x
    , elim𝑂 null (\ n → ppPun $ concat ["#",show𝕊 n]) nO
    ]

instance Pretty 𝕐 where
  pretty = \case
    NVar n x → concat [pretty x,if n ≡ 0 then null else ppPun $ concat ["↑",show𝕊 n]]
    DVar n → concat [ppPun "⌊",pretty n,ppPun "⌋"]
    MVar x → concat [pretty x,ppPun "†"]

cpVar ∷ CParser TokenBasic 𝕏
cpVar = var ^$ cpShaped $ view nameTBasicL

cpSVar ∷ CParser TokenBasic 𝕐
cpSVar = svar ∘ var ^$ cpShaped $ view nameTBasicL

cpMVar ∷ CParser TokenBasic 𝕐
cpMVar = MVar ∘ var ^$ cpShaped $ view nameTBasicL

cpVarWS ∷ CParser TokenWSBasic 𝕏
cpVarWS = var ^$ cpShaped $ view nameTWSBasicL

cpSVarWS ∷ CParser TokenWSBasic 𝕐
cpSVarWS = svar ∘ var ^$ cpShaped $ view nameTWSBasicL

cpMVarWS ∷ CParser TokenWSBasic 𝕐
cpMVarWS = MVar ∘ var ^$ cpShaped $ view nameTWSBasicL
