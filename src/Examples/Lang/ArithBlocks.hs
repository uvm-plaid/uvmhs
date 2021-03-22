module Examples.Lang.ArithBlocks where

import UVMHS

lexer ∷ Lexer CharClass ℂ TokenClassWSBasic ℕ64 TokenWSBasic
lexer = lexerWSBasic (list ["(",")"]) null null (list ["==","+","*","-","^","!"]) (list ["local"])

testTokenizerSuccess ∷ IO ()
testTokenizerSuccess = 
  tokenizeFIOMain lexer "" blockifyTokensWSBasic $ tokens $ concat $ inbetween "\n"
    [ "1"
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
  -- where
  --   nospace = not ∘ orf [shape spaceTWSBasicL,shape newlineTWSBasicL] ∘ parserTokenValue

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

cpLit ∷ CParser TokenWSBasic Lit
cpLit = tries
  [ IntegerL ^$ cpIntegerWS
  , DoubleL ^$ cpDoubleWS
  , StringL ^$ cpStringWS
  ]

cpAtom ∷ CParser TokenWSBasic Atom
cpAtom = cpNewContext "atom" $ tries
  [ LitA ^$ cpLit
  , NameA ^$ cpShaped $ view nameTWSBasicL
  ]

cpBlock ∷ CParser TokenWSBasic (𝐿 Exp)
cpBlock = cpNewContext "block" $ do
  void $ cpBlockWS "local"
  void $ cpOpenWS
  es ← cpExpList
  void $ cpCloseWS
  return es

cpExp ∷ CParser TokenWSBasic Exp 
cpExp = fmixfixWithContext "exp" $ concat
  [ fmixTerminal $ do
      void $ cpToken $ SyntaxTWSBasic "("
      e ← cpExp
      void $ cpToken $ SyntaxTWSBasic ")"
      return $ extract e
  , fmixTerminal       $ AtomE         ^$ cpAtom
  , fmixInfix   pCMP   $ const EqualE  ^$ cpSyntaxWS "=="
  , fmixInfixR  pPLUS  $ const PlusE   ^$ cpSyntaxWS "+"
  , fmixInfixR  pTIMES $ const TimesE  ^$ cpSyntaxWS "*"
  , fmixPrefix  pNEG   $ const NegateE ^$ cpSyntaxWS "-"
  , fmixInfixL  pPOW   $ const ExpoE   ^$ cpSyntaxWS "^"
  , fmixPostfix pFAC   $ const FactE   ^$ cpSyntaxWS "!"
  , fmixTerminal $ BlockE ^$ cpBlock
  ]

cpExpList ∷ CParser TokenWSBasic (𝐿 Exp)
cpExpList = cpManySepBy cpDelimWS cpExp

testParserSuccess ∷ IO ()
testParserSuccess = do
  parseIOMain cpExpList "" ∘ stream 
    *$ tokenizeFIO lexer ""  blockifyTokensWSBasic
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
  parseIOMain cpExpList "" ∘ stream 
    *$ tokenizeFIO lexer "" blockifyTokensWSBasic
     $ tokens 
     $ concat 
     $ inbetween "\n"
    [ "(- 1) + 2"
    , "local 2 + 3 + 4"
    , "      local - 2 + 3"
    , "      + 4 ^ 5 ^ 6 !"
    ]
