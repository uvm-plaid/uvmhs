module UVMHS.Lib.Pretty.Class where

import UVMHS.Core
import UVMHS.Lib.Pretty.Core
import UVMHS.Lib.Pretty.NoFormat

-- # Class

class Pretty a where
  pretty ∷ a → Doc

ppshow ∷ (Pretty a) ⇒ a → 𝕊
ppshow = noFormatOutput ∘ output ∘ execPrettyM ∘ runDoc ∘ pretty

instance Pretty Doc where 
  pretty = id

instance Pretty 𝔹 where pretty = ppCon ∘ show𝕊
instance Pretty ℕ where pretty = ppLit ∘ show𝕊
instance Pretty ℕ64 where pretty = ppLit ∘ show𝕊
instance Pretty ℕ32 where pretty = ppLit ∘ show𝕊
instance Pretty ℕ16 where pretty = ppLit ∘ show𝕊
instance Pretty ℕ8 where pretty = ppLit ∘ show𝕊
instance Pretty ℤ where pretty = ppLit ∘ show𝕊
instance Pretty ℤ64 where pretty = ppLit ∘ show𝕊
instance Pretty ℤ32 where pretty = ppLit ∘ show𝕊
instance Pretty ℤ16 where pretty = ppLit ∘ show𝕊
instance Pretty ℤ8 where pretty = ppLit ∘ show𝕊
instance Pretty 𝔻  where pretty = ppLit ∘ show𝕊
instance Pretty () where pretty = ppCon ∘ show𝕊

escape ∷ ℂ → 𝐼 ℂ
escape = \case
  '"' → iter "\\\""
  '\\' → iter "\\\\"
  '\n' → iter "\\n"
  '\t' → iter "\\t"
  '\r' → iter "\\r"
  '\b' → iter "\\b"
  '\f' → iter "\\f"
  c' → single c'

instance Pretty ℂ where 
  pretty c = ppLit $ string $ concat
    [ iter "'"
    , escape c
    , iter "'"
    ]

instance Pretty 𝕊 where 
  pretty s = ppLit $ string $ concat
    [ iter "\""
    , escape *$ iter s
    , iter "\""
    ]

instance (Pretty a,Pretty b) ⇒ Pretty (a,b) where
  pretty (a,b) = ppCollection "(" ")" "," $ list [pretty a, pretty b]
instance (Pretty a,Pretty b) ⇒ Pretty (a ∧ b) where
  pretty (a :* b) = ppCollection "⟨" "⟩" "," $ list [pretty a, pretty b]

instance (Pretty a) ⇒ Pretty (𝑆 a) where pretty xs = ppApp (ppText "𝑆") $ list [pretty $ list xs]
instance (Pretty a) ⇒ Pretty (𝐼 a) where pretty xs = ppApp (ppText "𝐼") $ list [pretty $ list xs]
instance (Pretty a) ⇒ Pretty (𝐿 a) where pretty = ppCollection "[" "]" "," ∘ map pretty
instance (Pretty a) ⇒ Pretty [a] where pretty = ppCollection "[" "]" "," ∘ map pretty ∘ list
instance (Pretty a) ⇒ Pretty (𝑄 a) where pretty xs = ppApp (ppText "𝑄") $ list [pretty $ list xs]
instance (Pretty a) ⇒ Pretty (𝑃 a) where pretty = ppCollection "{" "}"  "," ∘ map pretty ∘ list
instance (Pretty k,Pretty v) ⇒ Pretty (k ⇰ v) where pretty = ppRecord "↦" ∘ map (mapPair pretty pretty) ∘ list

instance (Pretty a) ⇒ Pretty (AddNull a) where
  pretty Null = ppCon "•"
  pretty (AddNull x) = pretty x

instance (Pretty a) ⇒ Pretty (AddBot a) where
  pretty Bot = ppCon "⊥"
  pretty (AddBot x) = pretty x

instance (Pretty a) ⇒ Pretty (AddTop a) where
  pretty Top = ppCon "⊤"
  pretty (AddTop x) = pretty x

instance (Pretty a) ⇒ Pretty (AddBT a) where
  pretty BotBT = ppCon "⊥"
  pretty TopBT = ppCon "⊤"
  pretty (AddBT x) = pretty x

-- instance Pretty 𝕋ᴰ where pretty = ppLit ∘ show𝕊
