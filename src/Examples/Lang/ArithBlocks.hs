module Examples.Lang.ArithBlocks where

import UVMHS

syntax ∷ LexerWSBasicSyntax
syntax = concat
  [ lexerWSBasicSyntaxPunsMk   $ pow ["(",")"]
  , lexerWSBasicSyntaxOprsMk   $ pow ["==","+","*","-","^","!"]
  , lexerWSBasicSyntaxBlocksMk $ pow ["local"]
  ]

lexer ∷ Lexer CharClass ℂ TokenClassWSBasic ℕ64 TokenWSBasic
lexer = lexerWSBasic syntax

testTokenizerSuccess ∷ IO ()
testTokenizerSuccess =
  tokenizeWSAnchoredIOMain lexer "" $ tokens $ concat $ inbetween "\n"
    [ "1 -- blah"
    , "2"
    , "3 4"
    , "  5"
    , "local 6"
    , "7"
    , "local 8"
    , "      9"
    , "local"
    , "10"
    , "local local "
    , "      local"
    ]

data Lit =
    IntegerL ℤ
  | DoubleL 𝔻
  | StringL 𝕊
makePrettySum ''Lit

data Atom =
    LitA Lit
  | NameA 𝕊
makePrettySum ''Atom

type Exp = 𝐴 SrcCxt ExpPre
data ExpPre =
    AtomE Atom
  | PlusE Exp Exp
  | TimesE Exp Exp
  | ExpoE Exp Exp
  | FactE Exp
  | NegateE Exp
  | EqualE Exp Exp
  | BlockE (𝐿 Exp)
makePrisms ''ExpPre
makePrettySum ''ExpPre

cpLit ∷ Parser TokenWSBasic Lit
cpLit = tries
  [ IntegerL ^$ pTokInt
  , DoubleL ^$ pTokDouble
  , StringL ^$ pTokString
  ]

cpAtom ∷ Parser TokenWSBasic Atom
cpAtom = pNewContext "atom" $ tries
  [ LitA ^$ cpLit
  , NameA ^$ pTokName
  ]

cpBlock ∷ Parser TokenWSBasic (𝐿 Exp)
cpBlock = pNewContext "block" $ do
  pTokBlock "local"
  pTokOpen
  es ← cpExpList
  pTokClose
  return es

cpExp ∷ Parser TokenWSBasic Exp
cpExp = fmixfixWithContext "exp" $ concat
  [ fmixTerminal $ do
      pTok $ SyntaxTWSBasic "("
      e ← cpExp
      pTok $ SyntaxTWSBasic ")"
      return $ extract e
  , fmixTerminal       $ AtomE         ^$ cpAtom
  , fmixInfix   pCMP   $ const EqualE  ^$ pTokSyntax "=="
  , fmixInfixR  pPLUS  $ const PlusE   ^$ pTokSyntax "+"
  , fmixInfixR  pTIMES $ const TimesE  ^$ pTokSyntax "*"
  , fmixPrefix  pNEG   $ const NegateE ^$ pTokSyntax "-"
  , fmixInfixL  pPOW   $ const ExpoE   ^$ pTokSyntax "^"
  , fmixPostfix pFAC   $ const FactE   ^$ pTokSyntax "!"
  , fmixTerminal $ BlockE ^$ cpBlock
  ]

cpExpList ∷ Parser TokenWSBasic (𝐿 Exp)
cpExpList = pManySepBy pTokDelim cpExp

testParserSuccess ∷ IO ()
testParserSuccess = do
  parseIOMain cpExpList ""
    *$ tokenizeWSAnchoredIO lexer ""
     $ tokens
     $ concat
     $ inbetween "\n"
    [ "(- 1) + 2"
    , "local 2 + 3"
    , "      local - 2 + 3"
    , "        * 4 ^ 5 ^ 6 !"
    ]

testParserFailure ∷ IO ()
testParserFailure =
  parseIOMain cpExpList ""
    *$ tokenizeWSAnchoredIO lexer ""
     $ tokens
     $ concat
     $ inbetween "\n"
    [ "(- 1) + 2"
    , "local 2 + 3 + 4"
    , "      local - 2 + 3"
    , "      + 4 ^ 5 ^ 6 !"
    ]
