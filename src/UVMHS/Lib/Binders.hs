module UVMHS.Lib.Binders where

import UVMHS.Core
import UVMHS.Lib.Pretty

-- variables --

data 𝕏 = 𝕏
  { 𝕩name ∷ 𝕊
  , 𝕩Gen ∷ 𝑂 ℕ
  } deriving (Eq,Ord,Show)

var ∷ 𝕊 → 𝕏
var x = 𝕏 x None

makeLenses ''𝕏

instance Pretty 𝕏 where
  pretty (𝕏 x nO) = concat
    [ ppString x
    , case nO of
        None → null
        Some n → concat [ppPun "#",pretty n]
    ]
