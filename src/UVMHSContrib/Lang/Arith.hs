module UVMHSContrib.Lang.Arith where

import UVMHS

lexer ∷ Lexer CharClass ℂ TokenClassBasic ℕ64 TokenBasic
lexer = lexerBasic (list ["(",")"]) null null (list ["==","+","*","-","^","!"])

testTokenizerSuccess ∷ IO ()
testTokenizerSuccess = 
  tokenizeIOMain lexer $ tokens "1 + 2 - 3 * 4 ^ 5 ! == 1 \n -- blah blah \n {- ml {{- ml --}-} -- blah\nb" 

data Lit =
    IntegerL ℤ
  | DoubleL 𝔻
  | StringL 𝕊
makePrettySum ''Lit

data Atom =
    LitA Lit
  | NameA 𝕊
makePrettySum ''Atom

type Exp = Annotated FullContext ExpPre
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

cpLit ∷ CParser TokenBasic Lit
cpLit = tries
  [ IntegerL ^$ cpInteger
  , DoubleL ^$ cpDouble
  , StringL ^$ cpString
  ]

cpAtom ∷ CParser TokenBasic Atom
cpAtom = cpNewContext "atom" $ tries
  [ LitA ^$ cpLit
  , NameA ^$ cpShaped $ view nameTBasicL
  ]

cpExp ∷ CParser TokenBasic Exp 
cpExp = fmixfixWithContext "exp" $ concat
  [ fmixTerminal $ do
      cpToken $ SyntaxTBasic "("
      e ← cpExp
      cpToken $ SyntaxTBasic ")"
      return $ extract e
  , fmixTerminal      $ AtomE         ^$ cpAtom
  , fmixInfix   pCMP  $ const EqualE  ^$ cpSyntax "=="
  , fmixInfixR  pSUM  $ const PlusE   ^$ cpSyntax "+"
  , fmixInfixR  pPROD $ const TimesE  ^$ cpSyntax "*"
  , fmixPrefix  pNEG  $ const NegateE ^$ cpSyntax "-"
  , fmixInfixL  pPOW  $ const ExpoE   ^$ cpSyntax "^"
  , fmixPostfix pFAC  $ const FactE   ^$ cpSyntax "!"
  ]

testParserSuccess ∷ IO ()
testParserSuccess = do
  parseIOMain cpExp ∘ stream *$ tokenizeIO lexer $ tokens "(- 1) + - 2 + 3 * 4 ^ 5 ^ 6 !"

testParserFailure1 ∷ IO ()
testParserFailure1 = parseIOMain cpExp ∘ stream *$ tokenizeIO lexer $ tokens "((9 == ((- 1))) + 2 + 3 * 4 ^ 5 ^ 6 !))"

testParserFailure2 ∷ IO ()
testParserFailure2 = parseIOMain cpExp ∘ stream *$ tokenizeIO lexer $ tokens "(((((- 1))) + 2 + 3 * 4 ^ 5 ^ ! == 0))"

