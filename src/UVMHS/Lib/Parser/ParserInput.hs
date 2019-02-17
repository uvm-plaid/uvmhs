module UVMHS.Lib.Parser.ParserInput where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Parser.Loc
import UVMHS.Lib.Parser.ParserContext
import UVMHS.Lib.Parser.Sep

-- # ParserToken

data ParserToken t = ParserToken
  { parserTokenValue ∷ t
  , parserTokenContext ∷ ParserContext
  }
makeLenses ''ParserToken
makePrettySum ''ParserToken

renderNL ∷ ParserContextDoc
renderNL = ParserContextDoc $ do
  mode ← ask
  tell $ case mode ≡ ParserContextError  of
    True → concat [ppErr "\\n",ppText"\n"]
    False → ppText "\n"

renderEOF ∷ ParserContextDoc
renderEOF = ParserContextDoc $ do
  mode ← ask
  tell $ case mode ≡ ParserContextError of
    True → ppErr "EOF"
    False → null

renderEOFContext ∷ AddBot Loc → ParserContext
renderEOFContext lM = parserContextFromLines $ eSepR $ ParserContextChunk (map (\ l → LocRange l l) lM) 0 $ mkParserContextDocCached renderEOF

tokens ∷ 𝕊 → 𝑆 (ParserToken ℂ)
tokens (stream → 𝑆 s₀ g) = 𝑆 (bot :* s₀) $ \ (loc :* s) → do
  (c :* s') ← g s
  let isNL = c ≡ '\n'
      pc = parserContextFromLines $ case isNL of
        True → iSepR $ ParserContextChunk (AddBot $ LocRange loc loc) (nat 1) $ mkParserContextDocCached renderNL
        False → eSepR $ ParserContextChunk (AddBot $ LocRange loc loc) (nat 0) $ mkParserContextDocCached $ ParserContextDoc $ tell $ ppText $ single c
  let loc' = case isNL of 
        True → bumpRow loc 
        False → bumpCol loc
  return $ ParserToken c pc:*(loc':*s')

renderParserInput ∷ 𝑆 (ParserToken t) → ParserContextDoc
renderParserInput = concat ∘ map (execParserContext ∘ parserTokenContext)

-- # ParserInput

data ParserInput t = ParserInput
  { parserInputStream ∷ 𝑆 (ParserToken t)
  , parserInputEndPos ∷ AddBot Loc
  }
makeLenses ''ParserInput
makePrettySum ''ParserInput

parserInput₀ ∷ 𝑆 (ParserToken t) → ParserInput t
parserInput₀ xs = ParserInput xs $ AddBot $ Loc bot bot bot

advanceInput ∷ ParserInput t → 𝑂 (ParserToken t,ParserInput t)
advanceInput (ParserInput ts _) = do
  (t :* ts') ← uncons𝑆 ts
  let endPos = map (bumpCol ∘ locRangeEnd) $ parserContextLocRange $ parserTokenContext t
  return (t,ParserInput ts' endPos)

-- Full Context --

data FullContext = FullContext
  { withContextPrefix ∷ InputContext
  , withContextDisplay ∷ ExpressionContext
  , withContextSuffix ∷ ParserContextDoc
  }

instance Pretty FullContext where
  pretty (FullContext pre d _pi) = concat
    [ ppPun "⟬"
    , ppAlign $ 
        (execParserContextDoc $ execParserContext $ unInputContext pre) 
        ⧺ (ppUT '^' green $ execParserContextDoc $ execParserContext $ unExpressionContext d)
    , ppPun "⟭"
    ]

-- Annotated --

data Annotated e a = Annotated
  { annotatedTag ∷ e
  , annotatedElem ∷ a
  } deriving (Show)
makeLenses ''Annotated
makePrettySum ''Annotated

instance (Eq a) ⇒ Eq (Annotated t a) where (==) = (≡) `on` annotatedElem
instance (Ord a) ⇒ Ord (Annotated t a) where compare = compare `on` annotatedElem
instance Extract (Annotated t) where extract = annotatedElem
instance Cobind (Annotated t) where Annotated e x =≫ f = Annotated e $ f $ Annotated e x
instance Functor (Annotated t) where map = wmap
instance Comonad (Annotated t)