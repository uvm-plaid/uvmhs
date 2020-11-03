module UVMHS.Lib.Parser.ParserInput where

import UVMHS.Core

import UVMHS.Lib.Pretty
import UVMHS.Lib.Window
import UVMHS.Lib.IterS

import UVMHS.Lib.Parser.Loc
import UVMHS.Lib.Parser.ParserContext

data PreParserToken t = PreParserToken
  { preParserTokenValue ∷ t
  , preParserTokenSkip ∷ 𝔹
  , preParserTokenContext ∷ ParserContext
  }
makeLenses ''PreParserToken
makePrettySum ''PreParserToken

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

eofContext ∷ AddBT Loc → ParserContext
eofContext l = 
  let lr = LocRange l l
  in ParserContext lr (eWindowL renderEOFDisplay) (eWindowR renderEOFDisplay) $ eWindowR renderEOFError

nlContext ∷ Loc → ParserContext
nlContext l =
  let lr = LocRange (AddBT l) $ AddBT l
  in ParserContext lr (iWindowL renderNLDisplay) (iWindowR renderNLDisplay) $ iWindowR renderNLError

charContext ∷ Loc → ℂ → ParserContext
charContext l c =
  let lr = LocRange (AddBT l) $ AddBT l
      d = ppString $ single c
  in ParserContext lr (eWindowL d) (eWindowR d) $ eWindowR d

tokens ∷ 𝕊 → 𝕍 (PreParserToken ℂ)
tokens cs = 
  vecS $ snd $ foldOnFrom cs (bot :* null @ (𝐼S _)) $ \ c (loc :* ts) →
    let (loc',pc) = 
          if c ≡ '\n'
            then (bumpRow₁ loc,nlContext loc)
            else (bumpCol₁ loc,charContext loc c)
        t = PreParserToken c False pc
    in loc' :* (ts ⧺ single t)

prepTokens ∷ 𝕍 (PreParserToken t) → 𝕍 (ParserToken t)
prepTokens ts₀ = vecS $ fst $ foldrOnFrom ts₀ (null @ (𝐼S _) :* null) $ \ (PreParserToken x sk pc) (ts :* ps) →
  let t = ParserToken x sk pc ps
  in
  (single t ⧺ ts) :* (parserContextDisplayL pc ⧺ ps)
