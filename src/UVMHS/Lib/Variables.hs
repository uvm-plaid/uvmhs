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
    DVar ℕ64
  | NVar ℕ64 𝕏
  | MVar 𝕏
  deriving (Eq,Ord,Show)
makePrisms ''𝕐

var ∷ 𝕊 → 𝕏
var = 𝕏 None

nvar ∷ 𝕏 → 𝕐
nvar = NVar 0

nvarL ∷ 𝕐 ⌲ 𝕏
nvarL = prism nvar $ \case
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

cpNVar ∷ CParser TokenBasic 𝕐
cpNVar = nvar ∘ var ^$ cpShaped $ view nameTBasicL

cpMVar ∷ CParser TokenBasic 𝕐
cpMVar = MVar ∘ var ^$ cpShaped $ view nameTBasicL

cpVarWS ∷ CParser TokenWSBasic 𝕏
cpVarWS = var ^$ cpShaped $ view nameTWSBasicL

cpNVarWS ∷ CParser TokenWSBasic 𝕐
cpNVarWS = nvar ∘ var ^$ cpShaped $ view nameTWSBasicL

cpMVarWS ∷ CParser TokenWSBasic 𝕐
cpMVarWS = MVar ∘ var ^$ cpShaped $ view nameTWSBasicL
