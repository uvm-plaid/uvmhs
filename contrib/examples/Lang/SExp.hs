module Lang.SExp where

import UVMHS

data SNumber = 
    SNInteger ℤ
  | SNDouble 𝔻
  deriving (Eq,Ord)
makePrettySum ''SNumber

data SLit =
    SLNumber SNumber
  | SLString 𝕊
  deriving (Eq,Ord)
makePrettySum ''SLit

data SToken =
    STLParen
  | STRParen
  | STLit SLit
  | STSymbol 𝕊
  | STWhitespace 𝕊
  deriving (Eq,Ord)
makePrettySum ''SToken
makePrisms ''SToken

lparenTok ∷ Parser ℂ ()
lparenTok = pRender (list [FG darkGray]) $ void $ pLit '(' 

rparenTok ∷ Parser ℂ ()
rparenTok = pRender (list [FG darkGray]) $ void $ pLit ')' 

litTok ∷ Parser ℂ SLit
litTok = pRender (list [FG darkRed]) $ tries
  [ SLNumber ^$ pErr "number" numberTok
  , SLString ^$ pErr "string" stringTok
  ]
  where
    numberTok ∷ Parser ℂ SNumber
    numberTok = do
      di ← pNumber
      return $ case di of
        Inl i → SNInteger i
        Inr d → SNDouble d
    stringTok ∷ Parser ℂ 𝕊
    stringTok = do
      void $ pLit '"'
      s ← concat ^$ pMany $ tries
        [ single ^$ pSatisfies "anything but '\"' or '\\'" $ \ c → not $ (c ≡ '"') ⩔ (c ≡ '\\')
        , pNew "escape sequence" $ do
            bslash ← single ^$ pLit '\\'
            c ← single ^$ pLit '\\' ⎅ pLit 'n'
            return $ bslash ⧺ c
        ]
      void $ pLit '"'
      return s

symbolTok ∷ Parser ℂ 𝕊
symbolTok = string ^$ pOneOrMore $ pSatisfies "letter" isLetter

whitespaceTok ∷ Parser ℂ 𝕊
whitespaceTok = string ^$ pOneOrMore $ pSatisfies "space" isSpace

tok ∷ 𝐿 (Parser ℂ SToken)
tok = list
  [ const STLParen ^$ pNew "lparen"     lparenTok 
  , const STRParen ^$ pNew "rparen"     rparenTok 
  , STLit          ^$ pNew "lit"        litTok
  , STSymbol       ^$ pNew "symbol"     symbolTok
  , STWhitespace   ^$ pNew "whitespace" whitespaceTok
  ]

testSExpTokenizerSuccess ∷ IO ()
testSExpTokenizerSuccess = tokenizeIOMain tok $ tokens "((-1-2-1.42(\"astringwith\\\\stuff\\n\" ( "

testSExpTokenizerFailure1 ∷ IO ()
testSExpTokenizerFailure1 = tokenizeIOMain tok $ tokens "((foo-1and0.01+bar"

testSExpTokenizerFailure2 ∷ IO ()
testSExpTokenizerFailure2 = tokenizeIOMain tok $ tokens "()foo-1\"astring\\badescape\""

data FullContext t = FullContext
  { fullContextInputContext ∷ InputContext
  , fullContextExpressionsConrtext ∷ ExpressionContext
  , fullContextInputStream ∷ ParserInput t
  }
instance Pretty (FullContext t) where
  pretty (FullContext (InputContext ic) (ExpressionContext ec) _pi) = concat
    [ ppPun "⟬"
    , ppAlign $ 
        (execParserContextDoc $ parserContextDocCachedDoc $ execParserContext ic) 
        ⧺ (ppUT '^' green $ execParserContextDoc $ parserContextDocCachedDoc $ execParserContext ec)
    , ppPun "⟭"
    ]

data SAtom =
    SALit SLit
  | SASymbol 𝕊
makePrettySum ''SAtom
data TaggedFix t (f ∷ ★ → ★) = TaggedFix
  { taggedFixContext ∷ FullContext t
  , taggedFixValue ∷ f (TaggedFix t f)
  }
makePrettySum ''TaggedFix
data PreSExp e =
    SEAtom SAtom
  | SEExp (𝐿 e)
makePrettySum ''PreSExp
type SExp = TaggedFix SToken PreSExp

atomPar ∷ Parser SToken SAtom
atomPar = pNew "atom" $ tries
  [ SALit ^$ litPar
  , SASymbol ^$ symbolPar
  ]

litPar ∷ Parser SToken SLit
litPar = pShaped "lit" $ view sTLitL

symbolPar ∷ Parser SToken 𝕊
symbolPar = pShaped "symbol" $ view sTSymbolL

preSExpPar ∷ Parser SToken (PreSExp SExp)
preSExpPar = tries
  [ SEAtom ^$ atomPar
  , SEExp ^$ inParensPar
  ]

inParensPar ∷ Parser SToken (𝐿 SExp)
inParensPar = do
  void $ pLit STLParen
  es ← sexpsPar
  void $ pLit STRParen
  return es

sexpsPar ∷ Parser SToken (𝐿 SExp)
sexpsPar = do
  void $ pOptional $ pSatisfies "whitespace" $ shape sTWhitespaceL
  xs ← pManySepBy (void $ pOptional $ pSatisfies "whitespace" $ shape sTWhitespaceL) sexpPar
  void $ pOptional $ pSatisfies "whitespace" $ shape sTWhitespaceL
  return xs

sexpPar ∷ Parser SToken SExp
sexpPar = do
  (ic :꘍ ec :꘍ s) ← pNewWithContext "sexp" preSExpPar
  pi ← getL parserStateInputL
  return $ TaggedFix (FullContext ic ec pi) s

testSExpParserSuccess ∷ IO ()
testSExpParserSuccess = do
  toks ← tokenizeIO tok input
  parseIOMain sexpsPar $ stream toks
  where
    input ∷ 𝑆 (ParserToken ℂ)
    input = tokens " x y  ( -1-2)  0.0"
