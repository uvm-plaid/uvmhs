module UVMHS.Lib.Parser.Regex where

import UVMHS.Core

import UVMHS.Lib.Annotated
import UVMHS.Lib.Pretty
import UVMHS.Lib.Window
import UVMHS.Lib.IterS

import UVMHS.Lib.Parser.Loc
import UVMHS.Lib.Parser.ParserContext
import UVMHS.Lib.Parser.ParserError
import UVMHS.Lib.Parser.ParserInput

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
    RegexResult (l₁ ⩏ l₂) (fm₁ ⧺ fm₂) (first o₁ o₂) (u₁ + u₂)
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

type Regex c t o u = Annotated (RegexInfo o u) (RegexU c t o u)
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
nullRegex = Annotated null NullR

resRegex ∷ (Ord c,Ord t,Ord o,Ord u,Zero u) ⇒ RegexResult o u → Regex c t o u
resRegex r = Annotated (RegexInfo $ Some r) $ ResR r

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
atomRegex = Annotated null ∘ AtomR eps

tokRegex ∷ (Ord c,Ord t,Ord o,Ord u,Zero u) ⇒ t → Regex c t o u
tokRegex t = atomRegex $ TokRA t

ntokRegex ∷ (Ord c,Ord t,Ord o,Ord u,Zero u) ⇒ 𝑃 t → Regex c t o u
ntokRegex t = atomRegex $ NTokRA t

classRegex ∷ (Ord c,Ord t,Ord o,Ord u,Zero u) ⇒ c → Regex c t o u
classRegex c = atomRegex $ ClassRA c

consEpsRegex ∷ (Ord c,Ord t,Ord o,Ord u,Plus u) ⇒ RegexResult o u → Regex c t o u → Regex c t o u
consEpsRegex r (Annotated i e) = Annotated (RegexInfo (Some r) ▷ i) $ consEpsRegexU r e

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
snocEpsRegex r (Annotated i e) = Annotated (i ▷ RegexInfo (Some r)) $ snocEpsRegexU r e

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
sumRegex e₁@(Annotated i₁ e₁') e₂@(Annotated i₂ e₂') = Annotated (i₁ ⧺ i₂) $ case (e₁',e₂') of
  (NullR,_) → e₂'
  (_,NullR) → e₁'
  (ResR r₁,ResR r₂) → ResR $ r₁ ⧺ r₂
  (SumsR es₁,SumsR es₂) → SumsR $ es₁ ∪ es₂
  (SumsR es₁,_) → SumsR $ es₁ ∪ single e₂
  (_,SumsR es₂) → SumsR $ single e₁ ∪ es₂
  _ → SumsR $ pow [e₁,e₂]
  
seqRegex ∷ (Ord c,Ord t,Ord o,Ord u,Additive u) ⇒ Regex c t o u → Regex c t o u → Regex c t o u
seqRegex e₁@(Annotated i₁ e₁') e₂@(Annotated i₂ e₂') = Annotated (i₁ ▷ i₂) $ case (e₁',e₂') of
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
starRegex e@(Annotated i e') = case e' of
  NullR → nullRegex
  ResR r → resRegex r
  StarR _ _ → e
  _ → Annotated (eps ⧺ i) $ StarR eps e

-- Derivative --

derRegex ∷ (Ord c,Ord t,Classified c t,Ord o,Ord u,Additive u) ⇒ t ∨ c → Regex c t o u → Regex c t o u
derRegex xc e₀ = case extract e₀ of
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
derRegexSequence xc (e@(Annotated i _) :& es) = case regexInfoResult i of
  None → derRegex xc e ▷ sequence es
  Some r → concat
    [ derRegex xc e ▷ sequence es
    , resRegex r ▷ derRegexSequence xc es
    ]

-- Literals --
  
regexLits ∷ (Ord t) ⇒ Regex c t o u → 𝑃 t
regexLits e₀ = case extract e₀ of
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
regexState₀ = RegexState zero dø dø dø dø

data DFA c t o u = DFA
  { dfaLits ∷ 𝑃 t
  , dfaStart ∷ ℕ64
  , dfaTransitions ∷ (t ∨ c) ⇰ 𝕍 ℕ64
  , dfaSuccess ∷ 𝕍 (𝑂 (RegexResult o u))
  , dfaDead ∷ 𝕍 𝔹
  }
makePrettySum ''DFA

compileRegex ∷ ∀ c t o u. (Pretty c,Pretty t,Pretty o,Pretty u,Ord c,Ord t,Classified c t,All c,Ord o,Ord u,Additive u) ⇒ Regex c t o u → DFA c t o u
compileRegex e₀ =
  let RegexState _ _ tr re de :* n = runState regexState₀ $ loop e₀
  in DFA lits n (map vecD tr) (vecD re) $ vecD de
  where 
    lits ∷ 𝑃 t
    lits = regexLits e₀
    codes ∷ 𝑃 (t ∨ c)
    codes = pow $ map Inl (iter lits) ⧺ map Inr all
    loop ∷ Regex c t o u → State (RegexState c t o u) ℕ64
    loop e = do
      m ← getL regexStateMapL
      case m ⋕? e of
        Some n → return n
        None → do
          n ← newRegexEntry e
          modifyL regexStateResultsL $ (⩌) $ n ↦ regexInfoResult (annotatedTag e)
          modifyL regexStateDeadL $ (⩌) $ n ↦ (extract e ≡ NullR)
          eachOn codes $ \ xc → do
            n' ← loop $ derRegex xc e
            modifyL regexStateTransitionsL $ unionWith (⩌) $ xc ↦ (n ↦ n')
          return n
    newRegexEntry ∷ Regex c t o u → State (RegexState c t o u) ℕ64
    newRegexEntry e = do
      n ← getL regexStateNextIDL
      putL regexStateNextIDL $ n + one
      modifyL regexStateMapL $ (⩌) $ e ↦ n
      return n

data LexDFAState t = LexDFAState
  { lexDFAStatePrefix ∷ WindowR Doc Doc
  , lexDFAStateContext ∷ ParserContext
  , lexDFAStateInput ∷ ParserInput t
  , lexDFAStateTokens ∷ 𝐼S t
  }
makePrettySum ''LexDFAState

data Lexer c t o u w = Lexer
  { lexerDFA ∷ u → DFA c t o u
  , lexerMkToken ∷ 𝐼S t → 𝑂 o → 𝔹 ∧ w
  , lexerInitState ∷ u
  }

tokenize ∷ 
  ∀ c t o u w. (Show u,Ord c,Ord t,Pretty t,Classified c t,Eq o,Eq u,Plus u) 
  ⇒ Lexer c t o u w → 𝕊 → 𝕍 (ParserToken t) → Doc ∨ 𝕍 (ParserToken w)
tokenize (Lexer dfas f u₀) so ts₀ = vecS ∘ fst ^$ oloop u₀ (dfas u₀) null $ parserInput₀ $ stream ts₀
  where
  oloop ∷ u → DFA c t o u → WindowR Doc Doc → ParserInput t → Doc ∨ 𝐼S (ParserToken w) ∧ WindowL Doc Doc
  oloop u (DFA lits n₀ δt δs δd) pp₀ pi₀' = iloop n₀ (LexDFAState pp₀ null pi₀' null) None None
    where
      success ∷ RegexResult o u → LexDFAState t → Doc ∨ 𝐼S (ParserToken w) ∧ WindowL Doc Doc
      success (RegexResult _ fm oO u') (LexDFAState pp pc pi ts) = do
        let u'' = u + u'
            pc' = formatParserContext fm pc
        wts :* wps ← oloop u'' (dfas u'') (pp ⧺ parserContextDisplayR pc') pi
        let sk :* w = f ts oO
            wt = ParserToken w sk pc' wps
        return $ (single wt ⧺ wts) :* (parserContextDisplayL pc' ⧺ wps)
      failure ∷ LexDFAState t → ParserToken t → Doc
      failure (LexDFAState pp pc _ _) (ParserToken _ _ tc s) =
        let le = map locRangeEnd $ parserContextLocRange tc
            d = parserContextError tc
        in displaySourceError so $ AddNull $ ParserError le d s $ single $ ParserErrorInfo pp (parserContextDisplayR pc) "<token>" null
      iloop ∷ ℕ64 → LexDFAState t → 𝑂 (ParserToken t ∧ LexDFAState t) → 𝑂 (RegexResult o u ∧ LexDFAState t) → Doc ∨ 𝐼S (ParserToken w) ∧ WindowL Doc Doc
      iloop n σ@(LexDFAState pp pc pi ts) tO rO = case advanceInput pi of
        -- end of stream
        None → case rO of
          -- end of stream
          -- no results to report
          None → case tO of
            -- end of stream
            -- no results to report
            -- no prior token
            -- DONE
            None → return $ null :* null
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
                  σ' = LexDFAState pp (pc ⧺ tc) pi' (ts ⧺ single x)
                  rO' = case δs ⋕! n' of
                    None → rO
                    Some r → Some (r :* σ')
              iloop n' σ' (Some (t :* σ)) rO'

tokenizeIO ∷
  ∀ c t o u w. (Show u,Ord c,Ord t,Pretty t,Classified c t,Eq o,Eq u,Plus u) 
  ⇒ Lexer c t o u w → 𝕊 → 𝕍 (ParserToken t) → IO (𝕍 (ParserToken w))
tokenizeIO l so pi = case tokenize l so pi of
  Inl d → pprint d ≫ abortIO
  Inr a → return a

tokenizeIOMain ∷
  ∀ c t o u w. (Show u,Ord c,Ord t,Pretty t,Classified c t,Eq o,Eq u,Plus u,Pretty w) 
  ⇒ Lexer c t o u w → 𝕊 → 𝕍 (ParserToken t) → IO ()
tokenizeIOMain l so pi = do
  x ← tokenizeIO l so pi
  pprint $ ppVertical 
    [ ppHeader "Success"
    , pretty $ map parserTokenValue x
    ]

-- API --

data CharClass = LetterClass | NumberClass | SpaceClass | OtherClass
  deriving (Eq,Ord,Show)
makePrettySum ''CharClass

instance All CharClass where all = iter [LetterClass,NumberClass,SpaceClass,OtherClass]

instance Classified CharClass ℂ where
  classify c
    | isLetter c = LetterClass
    | isNumber c = NumberClass
    | isSpace c = SpaceClass
    | otherwise = OtherClass

lWord ∷ (Zero u,Ord o,Ord u,Additive u) ⇒ 𝕊 → Regex CharClass ℂ o u
lWord = fold eps $ \ c r → r ▷ tokRegex c

lSpace ∷ (Zero u,Ord o,Ord u,Additive u) ⇒ Regex CharClass ℂ o u
lSpace = oom $ classRegex SpaceClass

lName ∷ (Zero u,Ord u,Ord o,Additive u) ⇒ Regex CharClass ℂ o u
lName = 
  let begTok = concat
        [ classRegex LetterClass
        , concat $ map tokRegex $ iter "_'′"
        ]
      endTok = concat
        [ begTok
        , classRegex NumberClass
        ]
      midTok = begTok ⧺ endTok ⧺ tokRegex '-'
  in 
  sequence
    [ classRegex LetterClass
    , begTok
    , opt $ sequence
        [ star $ midTok
        , endTok
        ]
    ]

lNatPre ∷ (Zero u,Ord u,Ord o,Additive u) ⇒ Regex CharClass ℂ o u
lNatPre = sequence
  [ concat $ map tokRegex ['0'..'9']
  , star $ concat
      [ concat $ map tokRegex ['0'..'9']
      , tokRegex '_'
      ]
  , fepsRegex $ formats [FG darkRed]
  ]

lNat ∷ (Zero u,Ord u,Ord o,Additive u) ⇒ Regex CharClass ℂ o u
lNat = sequence
  [ lNatPre
  , lepsRegex $ 𝕟64 102
  ]

lNatCoded ∷ (Zero u,Ord u,Ord o,Additive u) ⇒ Regex CharClass ℂ o u
lNatCoded = sequence
  [ lNatPre
  , tokRegex 'n'
  , lepsRegex $ 𝕟64 102
  ]

lIntPre ∷ (Zero u,Ord o,Ord u,Additive u) ⇒ Regex CharClass ℂ o u
lIntPre = sequence
  [ opt $ tokRegex '-'
  , lNatPre
  ]

lInt ∷ (Zero u,Ord o,Ord u,Additive u) ⇒ Regex CharClass ℂ o u
lInt = sequence
  [ lIntPre
  , lepsRegex $ 𝕟64 101
  ]

lDbl ∷ (Zero u,Ord o,Ord u,Additive u) ⇒ Regex CharClass ℂ o u
lDbl = sequence
  [ lIntPre
  , opt $ sequence
    [ tokRegex '.'
    , lNatPre
    ]
  , opt $ sequence
    [ tokRegex 'e'
    , lIntPre
    ]
  , lepsRegex $ 𝕟64 100
  ]

lString ∷ (Zero u,Ord o,Ord u,Additive u) ⇒ Regex CharClass ℂ o u
lString = sequence
  [ tokRegex '"'
  , star $ concat
      [ ntokRegex $ pow ['\\','"']
      , lWord "\\\\"
      , lWord "\\\""
      , lWord "\\n"
      ]
  , tokRegex '"'
  , fepsRegex $ formats [FG darkRed]
  ]

lComment ∷ (Ord o) ⇒ Regex CharClass ℂ o ℕ64
lComment = sequence
  [ lWord "--"
  , star $ ntokRegex $ single '\n'
  , opt $ tokRegex '\n'
  , fepsRegex $ formats [IT,FG gray]
  , lepsRegex $ 𝕟64 100
  ]

-- lCommentOpen ∷ (Ord o) ⇒ Regex CharClass ℂ o ℤ64
-- lCommentOpen = sequence
--   [ lWord "--"
--   , uepsRegex $ neg one
--   , fepsRegex $ formats [IT,FG gray]
--   , lepsRegex $ 𝕟64 100
--   ]
-- 
-- lCommentBody ∷ (Ord o) ⇒ Regex CharClass ℂ o ℤ64
-- lCommentBody = sequence
--   [ star $ ntokRegex $ single '\n'
--   , optRegex $ tokRegex '\n'
--   , uepsRegex one
--   , fepsRegex $ formats [IT,FG gray]
--   ]

lCommentMLOpen ∷ (Ord o) ⇒ Regex CharClass ℂ o ℕ64
lCommentMLOpen = sequence
  [ lWord "{-" 
  , uepsRegex one
  , fepsRegex $ formats [IT,FG gray]
  , lepsRegex $ 𝕟64 100
  ]

lCommentMLBodyOpen ∷ (Ord o) ⇒ Regex CharClass ℂ o ℕ64
lCommentMLBodyOpen = sequence
  [ oom (tokRegex '{') ▷ tokRegex '-'
  , uepsRegex one
  ]

lCommentMLBodyClose ∷ (Ord o) ⇒ Regex CharClass ℂ o ℕ64
lCommentMLBodyClose = sequence
  [ oom (tokRegex '-') ▷ tokRegex '}'
  , uepsRegex (neg one)
  ]

lCommentMLBody ∷ (Ord o) ⇒ Regex CharClass ℂ o ℕ64
lCommentMLBody = sequence
  [ star $ concat
      [ ntokRegex $ pow ['-','{']
      , oom (tokRegex '-') ▷ ntokRegex (pow ['-','}'])
      , oom (tokRegex '{') ▷ ntokRegex (pow ['{','-'])
      ]
  , lCommentMLBodyOpen ⧺ lCommentMLBodyClose
  , fepsRegex $ formats [IT,FG gray]
  ]

--------------------------
-- Basic Language Lexer --
--------------------------

data TokenClassBasic =
    SpaceCBasic
  | CommentCBasic
  | SyntaxCBasic
  | StringCBasic
  | NameCBasic
  | NaturalCBasic
  | IntegerCBasic
  | DoubleCBasic
  deriving (Eq,Ord,Show)
makePrisms ''TokenClassBasic
makePrettySum ''TokenClassBasic

data TokenBasic =
    SpaceTBasic 𝕊
  | CommentTBasic 𝕊
  | SyntaxTBasic 𝕊
  | StringTBasic 𝕊
  | NameTBasic 𝕊
  | NaturalTBasic ℕ
  | IntegerTBasic ℤ
  | DoubleTBasic 𝔻
  deriving (Eq,Ord,Show)
makePrisms ''TokenBasic
makePrettySum ''TokenBasic

mkTokenBasic ∷ 𝐼S ℂ → 𝑂 TokenClassBasic → 𝔹 ∧ TokenBasic
mkTokenBasic cs = \case
  None → error "no token class"
  Some SpaceCBasic → (:*) True $ SpaceTBasic $ stringS cs
  Some CommentCBasic → (:*) True $ CommentTBasic $ stringS cs
  Some SyntaxCBasic → (:*) False $ SyntaxTBasic $ stringS cs
  Some StringCBasic → (:*) False $ StringTBasic $ read𝕊 $ stringS cs
  Some NameCBasic → (:*) False $ NameTBasic $ stringS cs
  Some NaturalCBasic → (:*) False $ NaturalTBasic $ read𝕊 $ string $ filter (\ c → c ∉ pow ['_','n']) cs
  Some IntegerCBasic → (:*) False $ IntegerTBasic $ read𝕊 $ string $ filter ((≢) '_') cs
  Some DoubleCBasic → (:*) False $ DoubleTBasic $ read𝕊 $ string $ filter ((≢) '_') cs

lSyntaxBasic ∷ (Ord u,Additive u) ⇒ 𝐿 𝕊 → 𝐿 𝕊 → 𝐿 𝕊 → 𝐿 𝕊 → Regex CharClass ℂ TokenClassBasic u
lSyntaxBasic puns kws prims ops = concat
  -- punctuation
  [ sequence
    [ concat $ map lWord puns
    , fepsRegex $ formats [FG darkGray]
    ]
  -- keywords
  , sequence
    [ concat $ map lWord kws
    , fepsRegex $ formats [FG darkYellow,BD]
    ]
  -- primitives
  , sequence
    [ concat $ map lWord prims
    , fepsRegex $ formats [FG darkBlue]
    ]
  -- operators
  , sequence
    [ concat $ map lWord ops
    , fepsRegex $ formats [FG darkTeal]
    ]
  ]

lTokenBasic ∷ 𝐿 𝕊 → 𝐿 𝕊 → 𝐿 𝕊 → 𝐿 𝕊 → Regex CharClass ℂ TokenClassBasic ℕ64
lTokenBasic puns kws prims ops = concat
  [ lNatCoded                       ▷ oepsRegex NaturalCBasic
  , lInt                            ▷ oepsRegex IntegerCBasic
  , lDbl                            ▷ oepsRegex DoubleCBasic
  , lSyntaxBasic puns kws prims ops ▷ oepsRegex SyntaxCBasic
  , lString                         ▷ oepsRegex StringCBasic
  , lName                           ▷ oepsRegex NameCBasic
  , lSpace                          ▷ oepsRegex SpaceCBasic
  , lComment                        ▷ oepsRegex CommentCBasic
  , lCommentMLOpen                  ▷ oepsRegex CommentCBasic
  ]

-- lCommentBasic ∷ Regex CharClass ℂ TokenClassBasic ℕ64
-- lCommentBasic = lCommentBody ▷ oepsRegex CommentCBasic

lCommentMLBasic ∷ Regex CharClass ℂ TokenClassBasic ℕ64
lCommentMLBasic = lCommentMLBody ▷ oepsRegex CommentCBasic

dfaBasic ∷ 𝐿 𝕊 → 𝐿 𝕊 → 𝐿 𝕊 → 𝐿 𝕊 → ℕ64 → DFA CharClass ℂ TokenClassBasic ℕ64
dfaBasic puns kws prims ops =
  let dfaTokenBasic = compileRegex $ lTokenBasic puns kws prims ops
      -- dfaCommentBasic = compileRegex lCommentBasic
      dfaCommentMLBasic = compileRegex lCommentMLBasic
      dfa n | n ≡ 𝕟64 0 = dfaTokenBasic
            | otherwise = dfaCommentMLBasic
  in dfa

lexerBasic ∷ 𝐿 𝕊 → 𝐿 𝕊 → 𝐿 𝕊 → 𝐿 𝕊 → Lexer CharClass ℂ TokenClassBasic ℕ64 TokenBasic
lexerBasic puns kws prims ops = Lexer (dfaBasic puns kws prims ops) mkTokenBasic zero

------------------------
-- TLC Language Lexer --
------------------------

--
-- WORK IN PROGRESS...
--

-- data TokenClassTLC =
--     SpaceCTLC
--   | NewlineCTLC
--   | CommentCTLC
--   | SyntaxCTLC
--   | StringCTLC
--   | NameCTLC
--   | IntegerCTLC
--   | DoubleCTLC
--   deriving (Eq,Ord,Show)
-- makePrisms ''TokenClassTLC
-- makePrettySum ''TokenClassTLC
-- 
-- data TokenTLC =
--     SpaceTTLC 𝕊
--   | NewlineTTLC 𝕊
--   | CommentTTLC 𝕊
--   | SyntaxTTLC 𝕊
--   | StringTTLC 𝕊
--   | NameTTLC 𝕊
--   | IntegerTTLC ℤ
--   | DoubleTTLC 𝔻
--   deriving (Eq,Ord,Show)
-- makePrisms ''TokenTLC
-- makePrettySum ''TokenTLC
-- 
-- mkTokenTLC ∷ 𝐼S ℂ → 𝑂 TokenClassTLC → 𝔹 ∧ TokenTLC
-- mkTokenTLC cs = \case
--   None → error "no token class"
--   Some SpaceCTLC → (:*) True $ SpaceTTLC $ stringS cs
--   Some NewlineCTLC → (:*) False $ NewlineTTLC $ stringS cs
--   Some CommentCTLC → (:*) True $ CommentTTLC $ stringS cs
--   Some SyntaxCTLC → (:*) False $ SyntaxTTLC $ stringS cs
--   Some StringCTLC → (:*) False $ StringTTLC $ read𝕊 $ stringS cs
--   Some NameCTLC → (:*) False $ NameTTLC $ stringS cs
--   Some IntegerCTLC → (:*) False $ IntegerTTLC $ read𝕊 $ stringS cs
--   Some DoubleCTLC → (:*) False $ DoubleTTLC $ read𝕊 $ stringS cs
-- 
-- lSyntaxTLC ∷ (Ord u,Additive u) ⇒ 𝐿 𝕊 → 𝐿 𝕊 → 𝐿 𝕊 → Regex CharClass ℂ TokenClassTLC u
-- lSyntaxTLC puns ops kws = concat
--   -- punctuation
--   [ sequence
--     [ concat $ map lWord puns
--     , fepsRegex $ formats [FG darkGray]
--     ]
--   -- operators
--   , sequence
--     [ concat $ map lWord ops
--     , fepsRegex $ formats [FG darkYellow,BD]
--     ]
--   -- keywords
--   , sequence
--     [ concat $ map lWord kws
--     , fepsRegex $ formats [FG darkYellow,BD,UL]
--     ]
--   ]
-- 
-- lTokenTLC ∷ 𝐿 𝕊 → 𝐿 𝕊 → 𝐿 𝕊 → Regex CharClass ℂ TokenClassTLC ℤ64
-- lTokenTLC puns ops kws = concat
--   [ lInt                    ▷ oepsRegex IntegerCTLC
--   , lDbl                    ▷ oepsRegex DoubleCTLC
--   , lSyntaxTLC puns ops kws ▷ oepsRegex SyntaxCTLC
--   , lString                 ▷ oepsRegex StringCTLC
--   , lName                   ▷ oepsRegex NameCTLC
--   , lSpace                  ▷ oepsRegex SpaceCTLC
--   , lCommentOpen            ▷ oepsRegex CommentCTLC
--   , lCommentMLOpen          ▷ oepsRegex CommentCTLC
--   ]
-- 
-- lCommentTLC ∷ Regex CharClass ℂ TokenClassTLC ℤ64
-- lCommentTLC = lCommentBody ▷ oepsRegex CommentCTLC
-- 
-- lCommentMLTLC ∷ Regex CharClass ℂ TokenClassTLC ℤ64
-- lCommentMLTLC = lCommentMLBody ▷ oepsRegex CommentCTLC
-- 
-- dfaTLC ∷ 𝐿 𝕊 → 𝐿 𝕊 → 𝐿 𝕊 → ℤ64 → DFA CharClass ℂ TokenClassTLC ℤ64
-- dfaTLC puns ops kws =
--   let dfaTokenTLC = compileRegex $ lTokenTLC puns ops kws
--       dfaCommentTLC = compileRegex lCommentTLC
--       dfaCommentMLTLC = compileRegex lCommentMLTLC
--       dfa n | n ≡ 𝕫64 0 = dfaTokenTLC
--             | n < 𝕫64 0 = dfaCommentTLC
--             | n > 𝕫64 0 = dfaCommentMLTLC
--             | otherwise = error "impossible"
--   in dfa
-- 
-- lexerTLC ∷ 𝐿 𝕊 → 𝐿 𝕊 → 𝐿 𝕊 → Lexer CharClass ℂ TokenClassTLC ℤ64 TokenTLC
-- lexerTLC puns ops kws = Lexer (dfaTLC puns ops kws) mkTokenTLC zero
-- 
