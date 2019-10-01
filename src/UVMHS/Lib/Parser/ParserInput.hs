module UVMHS.Lib.Parser.ParserInput where

import UVMHS.Core

import UVMHS.Lib.Pretty
import UVMHS.Lib.Window
import UVMHS.Lib.IterS

import UVMHS.Lib.Parser.Loc
import UVMHS.Lib.Parser.ParserContext

-- # ParserToken

data ParserToken t = ParserToken
  { parserTokenValue ∷ t
  , parserTokenSkip ∷ 𝔹
  , parserTokenContext ∷ ParserContext
  , parserTokenSuffix ∷ WindowL Doc Doc
  }
makeLenses ''ParserToken
makePrettySum ''ParserToken

renderNLDisplay ∷ Doc
renderNLDisplay = ppString "\n"

renderNLError ∷ Doc
renderNLError = concat [ppErr "\\n",ppString "\n"]

renderEOFDisplay ∷ Doc
renderEOFDisplay = null

renderEOFError ∷ Doc
renderEOFError = ppErr "EOF"

eofContext ∷ AddBot Loc → ParserContext
eofContext lM = 
  let lr = map (\ l → LocRange l l) lM
  in ParserContext lr (eWindowL renderEOFDisplay) (eWindowR renderEOFDisplay) (eWindowR renderEOFError)

nlContext ∷ Loc → ParserContext
nlContext l =
  let lr = AddBot $ LocRange l l
  in ParserContext lr (iWindowL renderNLDisplay) (iWindowR renderNLDisplay) (iWindowR renderNLError)

charContext ∷ Loc → ℂ → ParserContext
charContext l c =
  let lr = AddBot $ LocRange l l
      d = ppString $ single c
  in ParserContext lr (eWindowL d) (eWindowR d) (eWindowR d)

tokens ∷ 𝕊 → 𝕍 (ParserToken ℂ)
tokens cs = 
  vecS $ fst $ snd $ foldbpOnFrom cs bot (null @ (𝐼S _) :* null) $ \ c loc →
    let (loc',pc) = 
          if c ≡ '\n'
            then (bumpRow loc,nlContext loc)
            else (bumpCol loc,charContext loc c)
    in (:*) loc' $ \ (ts :* ps) →
      let t = ParserToken c False pc ps
      in (single t ⧺ ts) :* (parserContextDisplayL pc ⧺ ps)

-----------------
-- ParserInput --
-----------------

data ParserInput t = ParserInput
  { parserInputStream ∷ 𝑆 (ParserToken t)
  , parserInputEndPos ∷ AddBot Loc
  }
makeLenses ''ParserInput
makePrettySum ''ParserInput

parserInput₀ ∷ 𝑆 (ParserToken t) → ParserInput t
parserInput₀ xs = ParserInput xs $ AddBot $ Loc bot bot bot

advanceInput ∷ ParserInput t → 𝑂 (ParserToken t ∧ ParserInput t)
advanceInput (ParserInput ts _) = do
  (t :* ts') ← uncons𝑆 ts
  let endPos = map (bumpCol ∘ locRangeEnd) $ parserContextLocRange $ parserTokenContext t
  return (t :* ParserInput ts' endPos)
