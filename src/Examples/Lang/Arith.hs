module Examples.Lang.Arith where

import UVMHS

syntax ∷ LexerWSBasicSyntax
syntax = null
  { lexerWSBasicSyntaxPuns = pow ["(",")"]
  , lexerWSBasicSyntaxOprs = pow ["==","+","*","-","^","!"]
  }

lexer ∷ Lexer CharClass ℂ TokenClassWSBasic ℕ64 TokenWSBasic
lexer = lexerWSBasic syntax

testTokenizerSuccess ∷ IO ()
testTokenizerSuccess =
  tokenizeIOMain lexer "" $ tokens "1 + 2 - 3 * 4 ^ 5 ! == 1 \n -- blah blah \n {- ml {{- ml --}-} -- blah\nb"

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
  , NameA ^$ pTokShaped $ view nameTWSBasicL
  ]

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
  ]

testParserSuccess ∷ IO ()
testParserSuccess = do
  parseIOMain cpExp "" *$ tokenizeIO lexer "" $ tokens "(- 1) + - 2 + 3 * 4 ^ 5 ^ 6 !"

testParserFailure1 ∷ IO ()
testParserFailure1 = parseIOMain cpExp "" *$ tokenizeIO lexer "" $ tokens "((9 == ((- 1))) + 2 + 3 * 4 ^ 5 ^ 6 !))"

testParserFailure2 ∷ IO ()
testParserFailure2 = parseIOMain cpExp "" *$ tokenizeIO lexer "" $ tokens "(((((- 1))) + 2 + 3 * 4 ^ 5 ^ ! == 0))"
