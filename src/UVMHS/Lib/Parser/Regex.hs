module UVMHS.Lib.Parser.Regex where

import UVMHS.Core

import UVMHS.Lib.Annotated
import UVMHS.Lib.Pretty

----------------
-- Classified --
----------------

class Classified c t | t → c where classify ∷ t → c

-----------------
-- RegexResult --
-----------------

data RegexResult o u = RegexResult
  { regexResultLevel ∷ ℕ64
  , regexResultFormats ∷ Formats
  , regexResultOutput ∷ 𝑂 o
  , regexResultUpdate ∷ u
  } deriving (Eq,Ord,Show)
makePrettyRecord ''RegexResult
instance (Zero u) ⇒ Null (RegexResult o u) where
  null = RegexResult zero null None zero
instance (Ord u) ⇒ Append (RegexResult o u) where
  r₁ ⧺ r₂ = if regexResultLevel r₁ ≥ regexResultLevel r₂ then r₁ else r₂
instance (Ord u,Zero u) ⇒ Monoid (RegexResult o u)

instance (Zero u) ⇒ Eps (RegexResult o u) where
  eps = RegexResult zero null None zero
instance (Ord u,Plus u) ⇒ Seq (RegexResult o u) where
  RegexResult l₁ fm₁ o₁ u₁ ▷ RegexResult l₂ fm₂ o₂ u₂ =
    RegexResult (l₁ ⩏ l₂) (fm₁ ⧺ fm₂) (first𝑂 o₁ o₂) (u₁ + u₂)
instance (Ord u,Additive u) ⇒ Seqoid (RegexResult o u)

---------------
-- RegexInfo --
---------------

newtype RegexInfo o u = RegexInfo
  { regexInfoResult ∷ 𝑂 (RegexResult o u)
  } deriving (Eq,Ord,Show)
makePrettySum ''RegexInfo

instance (Zero u) ⇒ Null (RegexInfo o u) where
  null = RegexInfo None
instance (Ord u) ⇒ Append (RegexInfo o u) where
  RegexInfo rO₁ ⧺ RegexInfo rO₂ = RegexInfo $ case (rO₁,rO₂) of
    (None,None) → None
    (None,Some r₂) → Some r₂
    (Some r₁,None) → Some r₁
    (Some r₁,Some r₂) → Some $ r₁ ⧺ r₂
instance (Ord u,Zero u) ⇒ Monoid (RegexInfo o u)

instance (Zero u) ⇒ Eps (RegexInfo o u) where
  eps = RegexInfo $ Some null
instance (Ord u,Plus u) ⇒ Seq (RegexInfo o u) where
  RegexInfo rO₁ ▷ RegexInfo rO₂ = RegexInfo $ case (rO₁,rO₂) of
    (Some r₁,Some r₂) → Some $ r₁ ▷ r₂
    _ → None
instance (Ord u,Additive u) ⇒ Seqoid (RegexInfo o u)

-----------
-- Regex --
-----------

newtype Regex c t o u = Regex { unRegex ∷ 𝐴 (RegexInfo o u) (RegexU c t o u) }
  deriving (Eq,Ord,Show)
data RegexU c t o u =
    NullR
  | ResR (RegexResult o u)
  | AtomR (RegexResult o u) (RegexAtom c t o u)
  | SumsR (𝑃 (Regex c t o u))
  | SeqsR (𝐿 (Regex c t o u))
  | StarR (RegexResult o u) (Regex c t o u)
  deriving (Eq,Ord,Show)
data RegexAtom c t o u =
    TokRA t
  | NTokRA (𝑃 t)
  | ClassRA c
  deriving (Eq,Ord,Show)
makePrettySum ''RegexU
makePrettySum ''RegexAtom

-- Construction --

instance (Zero u)                             ⇒ Null   (Regex c t o u) where null = nullRegex
instance (Ord c,Ord t,Ord o,Ord u,Plus u)     ⇒ Append (Regex c t o u) where (⧺) = sumRegex
instance (Ord c,Ord t,Ord o,Ord u,Zero u)     ⇒ Eps    (Regex c t o u) where eps = epsRegex
instance (Ord c,Ord t,Ord o,Ord u,Additive u) ⇒ Seq    (Regex c t o u) where (▷) = seqRegex
instance (Ord c,Ord t,Ord o,Ord u,Zero u)     ⇒ Star   (Regex c t o u) where star = starRegex

instance (Ord c,Ord t,Ord o,Ord u,Additive u) ⇒ Monoid (Regex c t o u)
instance (Ord c,Ord t,Ord o,Ord u,Additive u) ⇒ Seqoid (Regex c t o u)
instance (Ord c,Ord t,Ord o,Ord u,Additive u) ⇒ Kleene (Regex c t o u)

nullRegex ∷ (Zero u) ⇒ Regex c t o u
nullRegex = Regex $ 𝐴 null NullR

resRegex ∷ (Ord c,Ord t,Ord o,Ord u,Zero u) ⇒ RegexResult o u → Regex c t o u
resRegex r = Regex $ 𝐴 (RegexInfo $ Some r) $ ResR r

epsRegex ∷ (Ord c,Ord t,Ord o,Ord u,Zero u) ⇒ Regex c t o u
epsRegex = resRegex null

retRegex ∷ (Ord c,Ord t,Ord o,Ord u,Zero u) ⇒ ℕ64 → Formats → 𝑂 o → u → Regex c t o u
retRegex n fm oO u = resRegex $ RegexResult n fm oO u

outRegex ∷ (Ord c,Ord t,Ord o,Ord u,Zero u) ⇒ ℕ64 → Formats → o → Regex c t o u
outRegex n fm o = retRegex n fm (Some o) zero

lepsRegex ∷ (Ord c,Ord t,Ord o,Ord u,Zero u) ⇒ ℕ64 → Regex c t o u
lepsRegex n = retRegex n null None zero

fepsRegex ∷ (Ord c,Ord t,Ord o,Ord u,Zero u) ⇒ Formats → Regex c t o u
fepsRegex fm = retRegex zero fm None zero

oepsRegex ∷ (Ord c,Ord t,Ord o,Ord u,Zero u) ⇒ o → Regex c t o u
oepsRegex o = retRegex zero null (Some o) zero

uepsRegex ∷ (Ord c,Ord t,Ord o,Ord u,Zero u) ⇒ u → Regex c t o u
uepsRegex u = retRegex zero null None u

atomRegex ∷ (Ord c,Ord t,Ord o,Ord u,Zero u) ⇒ RegexAtom c t o u → Regex c t o u
atomRegex = Regex ∘ 𝐴 null ∘ AtomR eps

tokRegex ∷ (Ord c,Ord t,Ord o,Ord u,Zero u) ⇒ t → Regex c t o u
tokRegex t = atomRegex $ TokRA t

ntokRegex ∷ (Ord c,Ord t,Ord o,Ord u,Zero u) ⇒ 𝑃 t → Regex c t o u
ntokRegex t = atomRegex $ NTokRA t

classRegex ∷ (Ord c,Ord t,Ord o,Ord u,Zero u) ⇒ c → Regex c t o u
classRegex c = atomRegex $ ClassRA c

consEpsRegex ∷ (Ord c,Ord t,Ord o,Ord u,Plus u) ⇒ RegexResult o u → Regex c t o u → Regex c t o u
consEpsRegex r (Regex (𝐴 i e)) = Regex $ 𝐴 (RegexInfo (Some r) ▷ i) $ consEpsRegexU r e

consEpsRegexU ∷ (Ord c,Ord t,Ord o,Ord u,Plus u) ⇒ RegexResult o u → RegexU c t o u → RegexU c t o u
consEpsRegexU r = \case
  NullR → NullR
  ResR r' → ResR $ r ▷ r'
  AtomR r' a → AtomR (r ▷ r') a
  SumsR es → SumsR $ pow $ map (consEpsRegex r) $ iter es
  SeqsR Nil → NullR
  SeqsR (e :& es) → SeqsR $ consEpsRegex r e :& es
  StarR r' e → StarR (r ▷ r') e

snocEpsRegex ∷ (Ord c,Ord t,Ord o,Ord u,Plus u) ⇒ RegexResult o u → Regex c t o u → Regex c t o u
snocEpsRegex r (Regex (𝐴 i e)) = Regex $ 𝐴 (i ▷ RegexInfo (Some r)) $ snocEpsRegexU r e

snocEpsRegexU ∷ (Ord c,Ord t,Ord o,Ord u,Plus u) ⇒ RegexResult o u → RegexU c t o u → RegexU c t o u
snocEpsRegexU r = \case
  NullR → NullR
  ResR r' → ResR $ r' ▷ r
  AtomR r' a → AtomR (r' ▷ r) a
  SumsR es → SumsR $ pow $ map (consEpsRegex r) $ iter es
  SeqsR Nil → NullR
  SeqsR (e :& es) →
    let (es' :* e') = swivelR e es
        e'' = snocEpsRegex r e'
        (e''' :* es'') = swivelL es' e''
    in SeqsR $ e''' :& es''
  StarR r' e → StarR (r' ▷ r) e

sumRegex ∷ (Ord c,Ord t,Ord o,Ord u,Plus u) ⇒ Regex c t o u → Regex c t o u → Regex c t o u
sumRegex e₁@(Regex (𝐴 i₁ e₁')) e₂@(Regex (𝐴 i₂ e₂')) = Regex $ 𝐴 (i₁ ⧺ i₂) $ case (e₁',e₂') of
  (NullR,_) → e₂'
  (_,NullR) → e₁'
  (ResR r₁,ResR r₂) → ResR $ r₁ ⧺ r₂
  (SumsR es₁,SumsR es₂) → SumsR $ es₁ ∪ es₂
  (SumsR es₁,_) → SumsR $ es₁ ∪ single e₂
  (_,SumsR es₂) → SumsR $ single e₁ ∪ es₂
  _ → SumsR $ pow [e₁,e₂]

seqRegex ∷ (Ord c,Ord t,Ord o,Ord u,Additive u) ⇒ Regex c t o u → Regex c t o u → Regex c t o u
seqRegex e₁@(Regex (𝐴 i₁ e₁')) e₂@(Regex (𝐴 i₂ e₂')) = Regex $ 𝐴 (i₁ ▷ i₂) $ case (e₁',e₂') of
  (NullR,_) → NullR
  (_,NullR) → NullR
  (ResR r₁,_) → consEpsRegexU r₁ e₂'
  (_,ResR r₂) → snocEpsRegexU r₂ e₁'
  (SeqsR es₁,SeqsR es₂) → SeqsR $ es₁ ⧺ es₂
  (SeqsR es₁,_) → SeqsR $ es₁ ⧺ single e₂
  (_,SeqsR es₂) → SeqsR $ single e₁ ⧺ es₂
  (AtomR r₁ a₁,SumsR es₂) → SumsR $ pow $ map (\ e → consEpsRegex r₁ (atomRegex a₁) ▷ e) $ iter es₂
  (SumsR es₁,AtomR r₂ a₂) → SumsR $ pow $ map (\ e → e ▷ consEpsRegex r₂ (atomRegex a₂)) $ iter es₁
  _ → SeqsR $ list [e₁,e₂]

starRegex ∷ (Ord c,Ord t,Ord o,Ord u,Zero u) ⇒ Regex c t o u → Regex c t o u
starRegex e@(Regex (𝐴 i e')) = case e' of
  NullR → nullRegex
  ResR r → resRegex r
  StarR _ _ → e
  _ → Regex $ 𝐴 (eps ⧺ i) $ StarR eps e

-- Derivative --

derRegex ∷ (Ord c,Ord t,Classified c t,Ord o,Ord u,Additive u) ⇒ t ∨ c → Regex c t o u → Regex c t o u
derRegex xc e₀ = case extract $ unRegex e₀ of
  NullR → null
  ResR _ → null
  AtomR r a → consEpsRegex r $ derRegexAtom xc a
  SumsR es → concat $ map (derRegex xc) $ iter es
  SeqsR es → derRegexSequence xc es
  StarR r e → consEpsRegex r (derRegex xc e) ▷ star e

derRegexAtom ∷ (Ord c,Ord t,Classified c t,Ord o,Ord u,Additive u) ⇒ t ∨ c → RegexAtom c t o u → Regex c t o u
derRegexAtom xc = \case
  TokRA t → case xc of
    Inl t'
      | t ≡ t' → eps
      | otherwise → null
    Inr _ → null
  NTokRA ts → case xc of
    Inl t'
      | not $ t' ∈ ts → eps
      | otherwise → null
    Inr _ → eps
  ClassRA c → case xc of
    Inl t
      | classify t ≡ c → eps
      | otherwise → null
    Inr c'
      | c ≡ c' → eps
      | otherwise → null

derRegexSequence ∷ (Ord t,Ord c,Classified c t,Ord o,Ord u,Additive u) ⇒ t ∨ c → 𝐿 (Regex c t o u) → Regex c t o u
derRegexSequence _ Nil = null
derRegexSequence xc (e@(Regex (𝐴 i _)) :& es) = case regexInfoResult i of
  None → derRegex xc e ▷ sequence es
  Some r → concat
    [ derRegex xc e ▷ sequence es
    , resRegex r ▷ derRegexSequence xc es
    ]

-- Literals --

regexLits ∷ (Ord t) ⇒ Regex c t o u → 𝑃 t
regexLits e₀ = case extract $ unRegex e₀ of
  NullR → pø
  ResR _ → pø
  AtomR _ a → regexLitsAtom a
  SumsR es → joins $ map regexLits $ iter es
  SeqsR es → joins $ map regexLits $ iter es
  StarR _ e → regexLits e

regexLitsAtom ∷ (Ord t) ⇒ RegexAtom c t o u → 𝑃 t
regexLitsAtom = \case
  TokRA t → single t
  NTokRA ts → ts
  ClassRA _ → pø

----------------------
-- DFA Construction --
----------------------

data RegexState c t o u = RegexState
  { regexStateNextID ∷ ℕ64
  , regexStateMap ∷ Regex c t o u ⇰ ℕ64
  , regexStateTransitions ∷ (t ∨ c) ⇰ (ℕ64 ⇰ ℕ64)
  , regexStateResults ∷ ℕ64 ⇰ 𝑂 (RegexResult o u)
  , regexStateDead ∷ ℕ64 ⇰ 𝔹
  }
makeLenses ''RegexState

regexState₀ ∷ RegexState c t o u
regexState₀ = RegexState zero dø𝐷 dø𝐷 dø𝐷 dø𝐷

data DFA c t o u = DFA
  { dfaLits ∷ 𝑃 t
  , dfaStart ∷ ℕ64
  , dfaTransitions ∷ (t ∨ c) ⇰ 𝕍 ℕ64
  , dfaSuccess ∷ 𝕍 (𝑂 (RegexResult o u))
  , dfaDead ∷ 𝕍 𝔹
  }
makePrettySum ''DFA

compileRegex ∷ ∀ c t o u. (Pretty t,Pretty o,Pretty u,Ord c,Ord t,Classified c t,All c,Ord o,Ord u,Additive u) ⇒ Regex c t o u → DFA c t o u
compileRegex e₀ =
  let RegexState _ _ tr re de :* n = runState regexState₀ $ compile e₀
  in DFA lits n (map vecDΩ tr) (vecDΩ re) $ vecDΩ de
  where
    lits ∷ 𝑃 t
    lits = regexLits e₀
    codes ∷ 𝑃 (t ∨ c)
    codes = pow $ map Inl (iter lits) ⧺ map Inr all
    compile ∷ Regex c t o u → State (RegexState c t o u) ℕ64
    compile e = do
      m ← getL regexStateMapL
      case m ⋕? e of
        Some n → return n
        None → do
          n ← newRegexEntry e
          modifyL regexStateResultsL $ (⩌) $ n ↦ regexInfoResult (atag $ unRegex e)
          modifyL regexStateDeadL $ (⩌) $ n ↦ (extract (unRegex e) ≡ NullR)
          eachOn codes $ \ xc → do
            n' ← compile $ derRegex xc e
            modifyL regexStateTransitionsL $ dunionBy (⩌) $ xc ↦ (n ↦ n')
          return n
    newRegexEntry ∷ Regex c t o u → State (RegexState c t o u) ℕ64
    newRegexEntry e = do
      n ← getL regexStateNextIDL
      putL regexStateNextIDL $ n + one
      modifyL regexStateMapL $ (⩌) $ e ↦ n
      return n

-- API --

-- --------------------------
-- -- Basic Language Lexer --
-- --------------------------
-- 
-- data TokenClassBasic =
--     SpaceCBasic
--   | CommentCBasic
--   | SyntaxCBasic
--   | StringCBasic
--   | NameCBasic
--   | NaturalCBasic
--   | IntegerCBasic
--   | DoubleCBasic
--   | CharCBasic
--   deriving (Eq,Ord,Show)
-- makePrisms ''TokenClassBasic
-- makePrettySum ''TokenClassBasic
-- 
-- data TokenBasic =
--     SpaceTBasic 𝕊
--   | CommentTBasic 𝕊
--   | SyntaxTBasic 𝕊
--   | StringTBasic 𝕊
--   | NameTBasic 𝕊
--   | NaturalTBasic ℕ
--   | IntegerTBasic ℤ
--   | DoubleTBasic 𝔻
--   | CharTBasic ℂ
--   deriving (Eq,Ord,Show)
-- makePrisms ''TokenBasic
-- makePrettySum ''TokenBasic
-- 
-- mkTokenBasic ∷ 𝐼C ℂ → 𝑂 TokenClassBasic → 𝔹 ∧ TokenBasic
-- mkTokenBasic cs = \case
--   None → error "no token class"
--   Some SpaceCBasic → (:*) True $ SpaceTBasic $ stringCS cs
--   Some CommentCBasic → (:*) True $ CommentTBasic $ stringCS cs
--   Some SyntaxCBasic → (:*) False $ SyntaxTBasic $ stringCS cs
--   Some StringCBasic → (:*) False $ StringTBasic $ read𝕊 $ stringCS cs
--   Some NameCBasic → (:*) False $ NameTBasic $ stringCS cs
--   Some NaturalCBasic → (:*) False $ NaturalTBasic $ read𝕊 $ string $ filter (\ c → c ∉ pow𝑃 ['_','n']) cs
--   Some IntegerCBasic → (:*) False $ IntegerTBasic $ read𝕊 $ string $ filter ((≢) '_') cs
--   Some DoubleCBasic → (:*) False $ DoubleTBasic $ read𝕊 $ string $ filter ((≢) '_') cs
--   Some CharCBasic → (:*) False $ CharTBasic $ read𝕊 $ stringCS cs
-- 
-- data LexerBasicSyntax = LexerBasicSyntax
--   { lexerBasicSyntaxPuns ∷ 𝑃 𝕊 -- ^ punctuation (default color gray)
--   , lexerBasicSyntaxKeys ∷ 𝑃 𝕊 -- ^ keywords    (default color bold yellow)
--   , lexerBasicSyntaxPrms ∷ 𝑃 𝕊 -- ^ primitives  (default color blue)
--   , lexerBasicSyntaxOprs ∷ 𝑃 𝕊 -- ^ operators   (default color teal)
--   } deriving (Eq,Ord,Show)
-- makeLenses ''LexerBasicSyntax
-- 
-- instance Null LexerBasicSyntax where 
--   null = LexerBasicSyntax null null null null
-- instance Append LexerBasicSyntax where 
--   LexerBasicSyntax puns₁ keys₁ prms₁ oprs₁ ⧺ LexerBasicSyntax puns₂ keys₂ prms₂ oprs₂ =
--     LexerBasicSyntax (puns₁ ⧺ puns₂) (keys₁ ⧺ keys₂) (prms₁ ⧺ prms₂) $ oprs₁ ⧺ oprs₂
-- instance Monoid LexerBasicSyntax
-- 
-- 
-- lTokenBasic ∷ LexerBasicSyntax → Regex CharClass ℂ TokenClassBasic ℕ64
-- lTokenBasic syntax = concat
--   [ lNatCoded           ▷ oepsRegex NaturalCBasic
--   , lInt                ▷ oepsRegex IntegerCBasic
--   , lDbl                ▷ oepsRegex DoubleCBasic
--   , lSyntaxBasic syntax ▷ oepsRegex SyntaxCBasic
--   , lString             ▷ oepsRegex StringCBasic
--   , lName               ▷ oepsRegex NameCBasic
--   , lSpaceOrNl          ▷ oepsRegex SpaceCBasic
--   , lComment            ▷ oepsRegex CommentCBasic
--   , lCommentMLOpen      ▷ oepsRegex CommentCBasic
--   ]
-- 
-- lCommentMLBasic ∷ Regex CharClass ℂ TokenClassBasic ℕ64
-- lCommentMLBasic = lCommentMLBody ▷ oepsRegex CommentCBasic
-- 
-- dfaBasic ∷ LexerBasicSyntax → ℕ64 → DFA CharClass ℂ TokenClassBasic ℕ64
-- dfaBasic syntax =
--   let dfaTokenBasic = compileRegex $ lTokenBasic syntax
--       dfaCommentMLBasic = compileRegex lCommentMLBasic
--       dfa n | n ≡ 𝕟64 0 = dfaTokenBasic
--             | otherwise = dfaCommentMLBasic
--   in dfa
-- 
-- lexerBasic ∷ LexerBasicSyntax → Lexer CharClass ℂ TokenClassBasic ℕ64 TokenBasic
-- lexerBasic syntax = Lexer (dfaBasic syntax) mkTokenBasic zero

-----------------------------------------------
-- Basic Whitespace-sensitive Language Lexer --
-----------------------------------------------

-- Blockifying Tokens --

-- data BlockifyTokensEnv t = BlockifyTokensEnv
--   { blockifyTokensEnvIsNewline     ∷ t → 𝔹
--   , blockifyTokensEnvIsBlock       ∷ t → 𝔹
--   , blockifyTokensEnvMkIndentToken ∷ IndentCommand → t
--   , blockifyTokensEnvIsLParen      ∷ t → 𝔹
--   , blockifyTokensEnvIsRParen      ∷ t → 𝔹
--   }

-- NOTES
-- things that are tracked:
-- - the prefix (skip tokens)
-- - the location begin/end of the prefix
-- - whether or not the previous non-skip token was a block token
-- - whether or not this token is the first non-skip token after a newline
-- - a stack of anchors
--
-- NEW
-- - anchored mode: a stack of (anchor ∧ open-paren-count)
-- - unanchored mode: open-paren-count
-- - any mode: count of all open parens in stack
-- - if in unanchored mode: just track open/close parens
-- - if in anchored mode:
--   - if you see an open paren, just add it to open-paren-count
--   - if you see a close paren
--     - if open-paren-count is zero, emit a close, pop the anchor stack, and
--       try again recursively
--     - if open-paren-count is larger than zero, just decrement it and
--       continue
--
-- →block⋅
--    thing ( block
--              stuff)
--    thing
--
-- ⇒block
--   →thing⋅( block
--               stuff)
--    thing
--
-- ⇒block
--   →thing→(⋅block
--               stuff)
--    thing

-- ... anchor ->| blah blah blah
--                  blah
--                  ^^^^
-- blockifyTokens ∷ ∀ t. BlockifyTokensEnv t → 𝐿 (AddBT Loc) → 𝕍 (PreParserToken t) → 𝕍 (PreParserToken t)
-- blockifyTokens γ anchors₀ ts₀ = vecC $ loop null bot False False anchors₀ $ stream ts₀
--   where
--     syntheticToken ∷ AddBT Loc → IndentCommand → PreParserToken t
--     syntheticToken loc x =
--       let pcS = case x of
--             OpenIC → ppBG white $ ppFG grayLight $ ppString "{"
--             CloseIC → ppBG white $ ppFG grayLight $ ppString "}"
--             NewlineIC → ppBG white $ ppFG grayLight $ ppString ";"
--           eL = eWindowL pcS
--           eR = eWindowR pcS
--           pc = ParserContext (LocRange loc loc) eL eR eR
--       in
--       PreParserToken (blockifyTokensEnvMkIndentToken γ x) False pc
--     loop ∷ 𝐼C (PreParserToken t) → LocRange → 𝔹 → 𝔹 → 𝐿 (AddBT Loc) → 𝑆 (PreParserToken t) → 𝐼C (PreParserToken t)
--     loop prefix prefixLocRangeBumped isFreshBlock isAfterNewline = \case
--       Nil → loopUnanchored prefix prefixLocRangeBumped isFreshBlock
--       anchor :& anchors → loopAnchored prefix prefixLocRangeBumped isFreshBlock isAfterNewline anchor anchors
--     loopUnanchored ∷ 𝐼C (PreParserToken t) → LocRange → 𝔹 → 𝑆 (PreParserToken t) → 𝐼C (PreParserToken t)
--     loopUnanchored prefix prefixLocRangeBumped isFreshBlock ts = case un𝑆 ts () of
--       None → prefix
--       Some (t :* ts') →
--         let locₜ = locRangeBegin $ parserContextLocRange $ preParserTokenContext t
--             prefixLocRangeBumpedEnd = locRangeEnd prefixLocRangeBumped
--         in
--         if
--         | preParserTokenSkip t →
--           loopUnanchored (prefix ⧺ single t)
--                          (prefixLocRangeBumped ⊔ bumpColEnd₂ (parserContextLocRange $ preParserTokenContext t))
--                          isFreshBlock
--                          ts'
--         | {- not (parserTokenSkip t) ⩓ -}
--           isFreshBlock → concat
--             --
--             --     ... <block> <token>
--             --                 ^^^^^^^
--             [ single $ syntheticToken prefixLocRangeBumpedEnd OpenIC
--             , prefix
--             , single t
--             , loopAnchored null
--                            (LocRange prefixLocRangeBumpedEnd prefixLocRangeBumpedEnd)
--                            (blockifyTokensEnvIsBlock γ $ preParserTokenValue t)
--                            False
--                            locₜ
--                            null
--                            ts'
--             ]
--         | {- not (parserTokenSkip t) ⩓ not (isFreshBlock t) ⩓ -}
--           otherwise → concat
--           --
--           --     ... <token>
--           --         ^^^^^^^
--           [ prefix
--           , single t
--           , loopUnanchored null
--                            (LocRange prefixLocRangeBumpedEnd prefixLocRangeBumpedEnd)
--                            (blockifyTokensEnvIsBlock γ $ preParserTokenValue t)
--                            ts'
--           ]
--     loopAnchored ∷ 𝐼C (PreParserToken t) → LocRange → 𝔹 → 𝔹 → AddBT Loc → 𝐿 (AddBT Loc) → 𝑆 (PreParserToken t) → 𝐼C (PreParserToken t)
--     loopAnchored prefix prefixLocRangeBumped isFreshBlock isAfterNewline anchor anchors ts = case un𝑆 ts () of
--       None →
--         let loop' ∷ 𝐿 (AddBT Loc) → 𝐼C (PreParserToken t)
--             loop' anchors' =
--               if anchors' ≡ anchors₀
--               then null
--               else case anchors' of
--                 Nil → null
--                 _ :& anchors'' → concat
--                   [ single $ syntheticToken (locRangeBegin prefixLocRangeBumped) CloseIC
--                   , loop' anchors''
--                   ]
--         in concat
--           [ if isFreshBlock
--               then concat
--                 [ single $ syntheticToken (locRangeBegin prefixLocRangeBumped) OpenIC
--                 , single $ syntheticToken (locRangeBegin prefixLocRangeBumped) CloseIC
--                 ]
--               else
--               null
--           , loop' (anchor :& anchors)
--           , prefix
--           ]
--       Some (t :* ts') →
--         let locₜ = locRangeBegin $ parserContextLocRange $ preParserTokenContext t
--             prefixLocRangeBumpedEnd = locRangeEnd prefixLocRangeBumped
--             prefixLocRangeBumpedBegin = locRangeBegin prefixLocRangeBumped
--             recordTokenKeepGoing ∷ 𝐼C (PreParserToken t) → LocRange → 𝔹 → 𝐼C (PreParserToken t)
--             recordTokenKeepGoing prefix' prefixLocRangeBumped' weHaveANewAnchor =
--               let prefixLocRangeBumpedEnd' = locRangeEnd prefixLocRangeBumped'
--                   anchor' :* anchors' =
--                     if weHaveANewAnchor
--                     --
--                     --     anchor ->| <block> <token>
--                     --                        ^^^^^^^
--                     --                        (new anchor)
--                     --
--                     then locₜ :* (anchor :& anchors)
--                     --
--                     --     anchor ->|... <token>
--                     --                   ^^^^^^^
--                     else anchor :* anchors
--               in concat
--                 -- record an “open” if we have a new anchor
--                 [ if weHaveANewAnchor then single $ syntheticToken prefixLocRangeBumpedEnd' OpenIC else null
--                 -- record the prefix
--                 , prefix'
--                 -- record the token
--                 , single t
--                 -- keep going with new anchor
--                 , loopAnchored null
--                                (LocRange prefixLocRangeBumpedEnd' prefixLocRangeBumpedEnd')
--                                (blockifyTokensEnvIsBlock γ $ preParserTokenValue t)
--                                False
--                                anchor'
--                                anchors'
--                                ts'
--                 ]
--         in
--         if
--         | preParserTokenSkip t →
--          -- this is a skip token; add it to the list
--          loopAnchored (prefix ⧺ single t)
--                       (prefixLocRangeBumped ⊔ bumpColEnd₂ (parserContextLocRange $ preParserTokenContext t))
--                       isFreshBlock
--                       (isAfterNewline ⩔ blockifyTokensEnvIsNewline γ (preParserTokenValue t))
--                       anchor
--                       anchors
--                       ts'
--         | {- not (parserTokenSkip t) ⩓ -}
--           not isAfterNewline →
--             --
--             --     anchor ->|... <token>
--             --                   ^^^^^^^
--             --     OR
--             --
--             --     anchor ->|...
--             --         ... ... <token>
--             --                 ^^^^^^^
--             -- continue as normal
--             recordTokenKeepGoing prefix prefixLocRangeBumped isFreshBlock
--         | {- not (parserTokenSkip t) ⩓ isAfterNewline ⩓ -}
--           map locCol locₜ > map locCol anchor →
--             --
--             --     anchor ->|...
--             --                  <token>
--             --                  ^^^^^^^
--             -- continue as normal
--             recordTokenKeepGoing prefix prefixLocRangeBumped isFreshBlock
--         | {- not (parserTokenSkip t) ⩓ isAfterNewline ⩓ -}
--           map locCol locₜ ≡ map locCol anchor → concat
--           --
--           --     anchor ->|...
--           --               <token>
--           --               ^^^^^^^
--           -- this is logically a “newline”
--           -- if we just opened a new block, open and close it
--           [ if isFreshBlock
--             then concat
--               [ single $ syntheticToken prefixLocRangeBumpedBegin OpenIC
--               , single $ syntheticToken prefixLocRangeBumpedBegin CloseIC
--               ]
--             else null
--           -- record a “newline”
--           , single $ syntheticToken prefixLocRangeBumpedEnd NewlineIC
--           -- record the prefix
--           , prefix
--           -- keep going
--           , recordTokenKeepGoing null (LocRange prefixLocRangeBumpedEnd prefixLocRangeBumpedEnd) False
--           ]
--         | {- not (parserTokenSkip t) ⩓ isAfterNewline ⩓ -}
--           map locCol locₜ < map locCol anchor → concat
--           --
--           --     anchor ->|...
--           --         <token>
--           --         ^^^^^^^
--           -- this is logically a “close”
--           -- if we just opened a new block, close it
--           [ if isFreshBlock
--             then concat
--               [ single $ syntheticToken prefixLocRangeBumpedBegin OpenIC
--               , single $ syntheticToken prefixLocRangeBumpedBegin CloseIC
--               ]
--             else null
--           -- record a “close”
--           , single $ syntheticToken prefixLocRangeBumpedBegin CloseIC
--           -- restart this token with new anchor
--           , loop prefix prefixLocRangeBumped False isAfterNewline anchors ts
--           ]
--         | otherwise → error "impossible"
-- 
-- blockifyTokensTLAnchored ∷ BlockifyTokensEnv t → 𝕍 (PreParserToken t) → 𝕍 (PreParserToken t)
-- blockifyTokensTLAnchored γ = blockifyTokens γ $ single $ AddBT bot
-- 
-- blockifyTokensTLUnanchored ∷ BlockifyTokensEnv t → 𝕍 (PreParserToken t) → 𝕍 (PreParserToken t)
-- blockifyTokensTLUnanchored γ = blockifyTokens γ null

-- The Language --

