module UVMHS.Lib.Parser.ParserError where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Parser.ParserContext
import UVMHS.Lib.Parser.Loc

data ParserErrorStackTraces = ParserErrorStackTraces 
  { parserErrorStackTracesMessages ∷ 𝑃 𝕊
  , parserErrorStackTracesChain ∷ 𝕊 ⇰ ParserErrorStackTraces
  } deriving (Eq, Ord)
makeLenses ''ParserErrorStackTraces
makePrettyRecord ''ParserErrorStackTraces

instance Bot ParserErrorStackTraces where
  bot = ParserErrorStackTraces bot bot
instance Join ParserErrorStackTraces where
  ParserErrorStackTraces fin₁ ch₁ ⊔ ParserErrorStackTraces fin₂ ch₂ = ParserErrorStackTraces (fin₁ ⊔ fin₂) (ch₁ ⊔ ch₂)
instance JoinLattice ParserErrorStackTraces

makeStackTraces ∷ 𝕊 → 𝐿 𝕊 → ParserErrorStackTraces
makeStackTraces fin Nil = ParserErrorStackTraces (single fin) bot
makeStackTraces fin (msg :& msgs) = ParserErrorStackTraces bot $ dict [msg ↦ makeStackTraces fin msgs]

data ParserError = ParserError
  { parserErrorToken ∷ ParserContext
  , parserErrorSuffix ∷ ParserContextDoc
  , parserErrorFailures ∷ ExpressionContext ⇰ InputContext ∧ ParserErrorStackTraces
  }

instance Append ParserError where
  ParserError tok₁ suf₁ fail₁ ⧺ ParserError tok₂ suf₂ fail₂ =
    case (compare `on` (map locRangeEnd ∘ parserContextLocRange)) tok₁ tok₂ of
      LT → ParserError tok₂ suf₂ fail₂
      EQ → ParserError tok₁ suf₁ $ unionWith (\ (ic₁ :* pest₁) (_ic₂ :* pest₂) → (ic₁ :* pest₁ ⊔ pest₂)) fail₁ fail₂
      GT → ParserError tok₁ suf₁ fail₁

