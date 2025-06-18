module Examples.Lang.SExp where

import UVMHS

syntax ∷ Syntax
syntax = concat
  [ syntaxPuns ["(",")"]
  , syntaxKeys ["KEY"]
  , syntaxPrms ["PRIM"]
  , syntaxOprs ["+"]
  ]

lexer ∷ Lexer
lexer = mkLexer $ LexerArgs False syntax

testSExpTokenizerSuccess ∷ IO ()
testSExpTokenizerSuccess = lexIOMain lexer "<>" "((-1-2-1.42(\"astringwith\\\\stuff\\n\" ( "

testSExpTokenizerFailure1 ∷ IO ()
testSExpTokenizerFailure1 = lexIOMain lexer "<>" "((foo-1and0.01+bar"

testSExpTokenizerFailure2 ∷ IO ()
testSExpTokenizerFailure2 = lexIOMain lexer "<>" "()foo-1\"astring\\badescape\""

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

cpLit ∷ Parser Lit
cpLit = concat
  [ IntegerL ^$ pTokInt
  , DoubleL ^$ pTokDouble
  , StringL ^$ pTokString
  ]

cpAtom ∷ Parser Atom
cpAtom = pNewContext "atom" $ concat
  [ pErr "literal" $ LitA ^$ cpLit
  , pErr "name" $ NameA ^$ pTokName
  , pErr "keyword" $ const KeyA ^$ pTokSyntax "KEY"
  , pErr "primitive" $ const PrimA ^$ pTokSyntax "PRIM"
  , pErr "“+”" $ const PlusA ^$ pTokSyntax "+"
  ]

cpExp ∷ Parser Exp
cpExp = pNewContext "expression" $ pWithContextRendered $ concat
  [ AtomE ^$ cpAtom
  , ListE ^$ cpList
  ]

cpList ∷ Parser (𝐿 Exp)
cpList = pNewContext "list" $ do
  pErr "“(”" $ pTokSyntax "("
  es ← many cpExp
  pErr "“)”" $ pTokSyntax ")"
  return es

testSExpParserSuccess ∷ IO ()
testSExpParserSuccess = do
  let input = " ( PRIM KEY x + y  {- yo -} ( -1-2)  0.0 \n x   y   z \n abc -12  )  "
  toks ← lexIO lexer "<raw input>" input
  parseIOMain cpExp "<tokens input>" toks

testSExpParserFailure1 ∷ IO ()
testSExpParserFailure1 = do
  let input = " (( PRIM KEY x + y  {- yo -} ( -1-2)  0.0 \n x   y   z \n abc -12 )  "
  toks ← lexIO lexer "<raw input>" input
  parseIOMain cpExp "<tokens input>" toks

testSExpParserFailure2 ∷ IO ()
testSExpParserFailure2 = do
  let input = " )( PRIM KEY x + y  {- yo -} ( -1-2)  0.0 \n x   y   z \n abc -12 )  "
  lexIOMain lexer "" input

testSExpParserFailure3 ∷ IO ()
testSExpParserFailure3 = do
  let input = " ( PRIM KEY x + y  {- yo -} ( -1-2)  0.0 \n x   y   z \n abc -12 )(  "
  lexIOMain lexer "" input
