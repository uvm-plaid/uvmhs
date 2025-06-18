module UVMHS.Lib.Parser.GenLexer where

import UVMHS.Core

import UVMHS.Lib.Window
import UVMHS.Lib.Pretty

import UVMHS.Lib.Parser.Loc
import UVMHS.Lib.Parser.ParserContext
import UVMHS.Lib.Parser.ParserError
import UVMHS.Lib.Parser.ParserInput
import UVMHS.Lib.Parser.Regex

data GenLexState t = GenLexState
  { genLexStatePrefix ∷ WindowR Doc Doc
  , genLexStateContext ∷ ParserContext
  , genLexStateInput ∷ 𝑆 (ParserToken t)
  , genLexStateTokens ∷ 𝐼C t
  }
makePrettySum ''GenLexState

data GenLexer c t o u w = GenLexer
  { genLexerDFA ∷ u → DFA c t o u
  , genLexerMkToken ∷ 𝐼C t → 𝑂 o → 𝔹 ∧ w
  , genLexerInitState ∷ u
  }

glex ∷
  ∀ c t o u w. (Show u,Ord c,Ord t,Pretty t,Classified c t,Eq o,Eq u,Plus u)
  ⇒ GenLexer c t o u w → 𝕊 → 𝕍 (ParserToken t) → Doc ∨ 𝕍 (ParserToken w)
glex (GenLexer dfas f u₀) so ts₀ = finalizeTokens ^$ vecC ^$ oloop u₀ (dfas u₀) null $ stream ts₀
  where
  oloop ∷ u → DFA c t o u → WindowR Doc Doc → 𝑆 (ParserToken t) → Doc ∨ 𝐼C (PreParserToken w)
  oloop u (DFA lits n₀ δt δs δd) pp₀ pi₀' = iloop n₀ (GenLexState pp₀ null pi₀' null) None None
    where
      success ∷ RegexResult o u → GenLexState t → Doc ∨ 𝐼C (PreParserToken w)
      success (RegexResult _ fm oO u') (GenLexState pp pc pi ts) = do
        let u'' = u + u'
            pc' = formatParserContext fm pc
        wts ← oloop u'' (dfas u'') (pp ⧺ parserContextDisplayR pc') pi
        let sk :* w = f ts oO
            wt = PreParserToken w sk pc'
        return $ (single wt ⧺ wts)
      failure ∷ GenLexState t → ParserToken t → Doc
      failure (GenLexState pp pc _ _) (ParserToken _ _ tc s) =
        let le = locRangeEnd $ parserContextLocRange tc
            d = parserContextError tc
        in displaySourceError so $ AddNull $ ParserError le d s $ single $ ParserErrorInfo pp (parserContextDisplayR pc) "<token>" null
      iloop ∷ ℕ64 → GenLexState t → 𝑂 (ParserToken t ∧ GenLexState t) → 𝑂 (RegexResult o u ∧ GenLexState t) → Doc ∨ 𝐼C (PreParserToken w)
      iloop n σ@(GenLexState pp pc pi ts) tO rO = case un𝑆 pi () of
        -- end of stream
        None → case rO of
          -- end of stream
          -- no results to report
          None → case tO of
            -- end of stream
            -- no results to report
            -- no prior token
            -- DONE
            None → return $ null -- :* null
            -- end of stream
            -- no results to report
            -- yes prior token
            -- ERROR
            Some (t :* σ') → throw $ failure σ' t
          -- end of stream
          -- results to report
          -- SUCCESS
          Some (r :* σ') → success r σ'
        -- middle of stream
        Some (t@(ParserToken x _ tc _) :* pi') → do
          if δd ⋕! n
            -- middle of stream
            -- parser is dead
            then case rO of
              -- middle of stream
              -- parser is dead
              -- no results to report
              -- ERROR
              None → case tO of
                None → error "lexer was dead before it even tried to read input :("
                Some (t' :* σ'') → throw $ failure σ'' t'
              -- middle of stream
              -- parser is dead
              -- a result to report
              -- SUCCESS
              Some (r :* σ'') → success r σ''
            -- middle of stream
            -- parser is not dead
            -- KEEP GOING
            else do
              let n' = if x ∈ lits then δt ⋕! (Inl x) ⋕! n else δt ⋕! (Inr $ classify x) ⋕! n
                  σ' = GenLexState pp (pc ⧺ tc) pi' (ts ⧺ single x)
                  rO' = case δs ⋕! n' of
                    None → rO
                    Some r → Some (r :* σ')
              iloop n' σ' (Some (t :* σ)) rO'

glexIO ∷
  ∀ c t o u w. (Show u,Ord c,Ord t,Pretty t,Classified c t,Eq o,Eq u,Plus u)
  ⇒ GenLexer c t o u w → 𝕊 → 𝕍 (ParserToken t) → IO (𝕍 (ParserToken w))
glexIO l so ts = case glex l so ts of
  Inl err → do 
    pprint $ ppVertical
      [ ppErr "[Lexing Failure]"
      , err
      ]
    abortIO
  Inr ts' → return ts'

glexIOMain ∷
  ∀ c t o u w. (Show u,Ord c,Ord t,Pretty t,Classified c t,Eq o,Eq u,Plus u)
  ⇒ GenLexer c t o u w → 𝕊 → 𝕍 (ParserToken t) → IO ()
glexIOMain l so ts = do
  xs ← glexIO l so ts
  pprint $ ppVertical
    [ ppHeader "[Lexing Success]"
    , pretty $ renderParserTokens xs
    ]
