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

---------------
-- FREE VARS --
---------------

class HasFV a where
  fv ∷ a → 𝑃 𝕏

fvVar ∷ 𝕐 → 𝑃 𝕏
fvVar = elim𝑂 null single ∘ view namedVarL

instance HasFV 𝕐 where fv = fvVar
