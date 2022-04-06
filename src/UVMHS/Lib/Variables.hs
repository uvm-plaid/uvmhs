module UVMHS.Lib.Variables where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Parser

---------------
-- VARIABLES --
---------------

data 𝕏 = 𝕏
  { 𝕩name ∷ 𝕊
  , 𝕩mark ∷ 𝑂 ℕ64
  } deriving (Eq,Ord,Show)
makeLenses ''𝕏

var ∷ 𝕊 → 𝕏
var x = 𝕏 x None

instance Pretty 𝕏 where
  pretty (𝕏 x nO) = concat
    [ ppString x
    , elim𝑂 null (\ n → concat [ppPun "#",ppPun $ show𝕊 n]) nO
    ]

cpVar ∷ CParser TokenBasic 𝕏
cpVar = var ^$ cpShaped $ view nameTBasicL

cpVarWS ∷ CParser TokenWSBasic 𝕏
cpVarWS = var ^$ cpShaped $ view nameTWSBasicL

----------------------
-- LOCALLY NAMELESS --
----------------------

data 𝕐 =
    NamedVar 𝕏
  | BoundVar ℕ64
  deriving (Eq,Ord,Show)
makePrisms ''𝕐

instance Pretty 𝕐 where
  pretty = \case
    NamedVar x → pretty x
    BoundVar n → concat [ppPun "!",ppString $ show𝕊 n]

-------------------------
-- FREE AND BOUND VARS --
-------------------------

data FBV = FBV
  { fbvBound ∷ 𝑃 𝕏
  , fbvFree ∷ 𝑃 𝕏
  }

instance Null FBV where null = FBV null null
instance Append FBV where FBV bv₁ fv₁ ⧺ FBV bv₂ fv₂ = FBV (bv₁ ⧺ bv₂) $ fv₁ ⧺ fv₂
instance Monoid FBV

instance Bot FBV where bot = FBV bot bot
instance Join FBV where FBV bv₁ fv₁ ⊔ FBV bv₂ fv₂ = FBV (bv₁ ⊔ bv₂) $ fv₁ ⊔ fv₂
instance JoinLattice FBV

class HasFBV a where
  fbv ∷ a → FBV

fv ∷ (HasFBV a) ⇒ a → 𝑃 𝕏
fv = fbvFree ∘ fbv

bv ∷ (HasFBV a) ⇒ a → 𝑃 𝕏
bv = fbvBound ∘ fbv

scopeFBV ∷ FBV → FBV → FBV
scopeFBV (FBV bv₁ fv₁) (FBV bv₂ fv₂) = FBV bv₂ $ fv₁ ⊔ (fv₂ ∖ bv₁)

varBoundFBV ∷ 𝕏 → FBV
varBoundFBV x = FBV (single x) null

varFreeFBV ∷ 𝕏 → FBV
varFreeFBV x = FBV null $ single x

instance HasFBV 𝕐 where 
  fbv = \case
    NamedVar x → varFreeFBV x
    BoundVar _ → null
