module Examples.Lang.SExp where

import UVMHS

syntax ∷ LexerWSBasicSyntax
syntax = null
  { lexerWSBasicSyntaxPuns = pow ["(",")"]
  , lexerWSBasicSyntaxKeys = pow ["KEY"]
  , lexerWSBasicSyntaxPrms = pow ["PRIM"]
  , lexerWSBasicSyntaxOprs = pow ["+"]
  }

lexer ∷ Lexer CharClass ℂ TokenClassWSBasic ℕ64 TokenWSBasic
lexer = lexerWSBasic syntax

testSExpTokenizerSuccess ∷ IO ()
testSExpTokenizerSuccess =
  tokenizeIOMain lexer "" $ tokens "((-1-2-1.42(\"astringwith\\\\stuff\\n\" ( "

testSExpTokenizerFailure1 ∷ IO ()
testSExpTokenizerFailure1 =
  tokenizeIOMain lexer "" $ tokens "((foo-1and0.01+bar"

testSExpTokenizerFailure2 ∷ IO ()
testSExpTokenizerFailure2 =
  tokenizeIOMain lexer "" $ tokens "()foo-1\"astring\\badescape\""

data Lit =
    IntegerL ℤ
  | DoubleL 𝔻
  | StringL 𝕊
makePrettySum ''Lit

data Atom =
    LitA Lit
  | NameA 𝕊
  | KeyA
  | PrimA
  | PlusA
makePrettySum ''Atom

type Exp = 𝐴 SrcCxt ExpPre
data ExpPre =
    AtomE Atom
  | ListE (𝐿 Exp)
makePrettySum ''ExpPre

------------
-- Parser --
------------

cpLit ∷ Parser TokenWSBasic Lit
cpLit = concat
  [ IntegerL ^$ pTokInt
  , DoubleL ^$ pTokDouble
  , StringL ^$ pTokString
  ]

cpAtom ∷ Parser TokenWSBasic Atom
cpAtom = pNewContext "atom" $ concat
  [ pErr "literal" $ LitA ^$ cpLit
  , pErr "name" $ NameA ^$ pTokName
  , pErr "keyword" $ const KeyA ^$ pTokSyntax "KEY"
  , pErr "primitive" $ const PrimA ^$ pTokSyntax "PRIM"
  , pErr "“+”" $ const PlusA ^$ pTokSyntax "+"
  ]

cpExp ∷ Parser TokenWSBasic Exp
cpExp = pNewContext "expression" $ pWithContextRendered $ concat
  [ AtomE ^$ cpAtom
  , ListE ^$ cpList
  ]

cpList ∷ Parser TokenWSBasic (𝐿 Exp)
cpList = pNewContext "list" $ do
  pErr "“(”" $ pTokSyntax "("
  es ← pMany cpExp
  pErr "“)”" $ pTokSyntax ")"
  return es

testSExpParserSuccess ∷ IO ()
testSExpParserSuccess = do
  toks ← tokenizeIO lexer "<raw input>" input
  parseIOMain cpExp "<tokens input>" toks
  where
    input ∷ 𝕍 (ParserToken ℂ)
    input = tokens " ( PRIM KEY x + y  {- yo -} ( -1-2)  0.0 \n x   y   z \n abc -12  )  "

testSExpParserFailure1 ∷ IO ()
testSExpParserFailure1 = do
  toks ← tokenizeIO lexer "<raw input>" input
  parseIOMain cpExp "<tokens input>" toks
  where
    input ∷ 𝕍 (ParserToken ℂ)
    input = tokens " (( PRIM KEY x + y  {- yo -} ( -1-2)  0.0 \n x   y   z \n abc -12 )  "

testSExpParserFailure2 ∷ IO ()
testSExpParserFailure2 = do
  tokenizeIOMain lexer "" input
  toks ← tokenizeIO lexer "" input
  parseIOMain cpExp "" toks
  where
    input ∷ 𝕍 (ParserToken ℂ)
    input = tokens " )( PRIM KEY x + y  {- yo -} ( -1-2)  0.0 \n x   y   z \n abc -12 )  "

testSExpParserFailure3 ∷ IO ()
testSExpParserFailure3 = do
  tokenizeIOMain lexer "" input
  toks ← tokenizeIO lexer "" input
  parseIOMain cpExp "" toks
  where
    input ∷ 𝕍 (ParserToken ℂ)
    input = tokens " ( PRIM KEY x + y  {- yo -} ( -1-2)  0.0 \n x   y   z \n abc -12 )(  "
