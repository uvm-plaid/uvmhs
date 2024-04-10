module Examples.Lang.WizPL where

import UVMHS

------------
-- SYNTAX --
------------

type SType = Annotated FullContext SType_R
data SType_R =
    Int_ST                --  int
  | Fun_ST SType SType    --
  deriving (Eq,Ord,Show)
makePrettySum ''SType_R

data Op =
    Plus_O     --  +
  | Minus_O    --  -
  | Times_O    --  *
  | Divide_O   --  /
  | LTE_O      --  <=
  | GTE_O      --  >=
  | LT_O       --  <
  | GT_O       --  >
  | EQ_O       --  ==
  deriving (Eq,Ord,Show)
makePrettySum ''Op

type SExp = Annotated FullContext SExp_R
data SExp_R =
    Var_SE 𝕏             --  x
  | Int_SE ℤ             --  i
  | Op_SE Op (𝐿 SExp)    --  e + e
  | Fun_SE 𝕏 SExp        --  fun x => e
  | App_SE SExp SExp     --  e e
  deriving (Eq,Ord,Show)
makePrettySum ''SExp_R

type SCmd = Annotated FullContext SCmd_R

data SCmd_R =
    Decl_SC 𝕏 SType    --  x : τ
  | Defn_SC 𝕏 SExp     --  x = e
  deriving (Eq,Ord,Show)
makePrettySum ''SCmd_R

-----------
-- LEXER --
-----------

lexer ∷ Lexer CharClass ℂ TokenClassWSBasic ℕ64 TokenWSBasic
lexer = lexerWSBasic
  -- punctuation
  (list ["(",")",":","=","=>","->"])
  -- keywords
  (list ["fun"])
  -- primitives
  (list ["int"])
  -- operations
  (list ["+","-","*","<=","<","==","+","*","-","^","!"])
  -- block
  (list [])

lex ∷ 𝕊 → 𝕊 → IO (𝕍 (ParserToken TokenWSBasic))
lex name = tokenizeFIO lexer name blockifyTokensWSBasic ∘ tokens

------------
-- PARSER --
-----------

data Fixity = NoF | LeftF | RightF

pType ∷ CParser TokenWSBasic SType
pType = fmixfixWithContext "type" $ concat
  -- parens
  [ fmixTerminal $ do
      void $ cpToken $ SyntaxTWSBasic "("
      τ ← pType
      void $ cpToken $ SyntaxTWSBasic ")"
      return $ extract τ
  -- int
  , fmixTerminal $ do
      void $ cpSyntaxWS "int"
      return Int_ST
  -- arrow
  , fmixInfixR pARR $ do
      void $ cpSyntaxWS "->"
      return Fun_ST
  ]

binaryOps ∷ 𝐿 (𝕊 ∧ Op ∧ ℕ64 ∧ Fixity)
binaryOps = frhs
  [ ("+" ,Plus_O  ,pPLUS ,LeftF)
  , ("-" ,Minus_O ,pPLUS ,LeftF)
  , ("*" ,Times_O ,pTIMES,LeftF)
  , ("/" ,Divide_O,pTIMES,LeftF)
  , ("<=",LTE_O   ,pCMP  ,NoF  )
  , (">=",GTE_O   ,pCMP  ,NoF  )
  , ("<" ,LT_O    ,pCMP  ,NoF  )
  , (">" ,GT_O    ,pCMP  ,NoF  )
  , ("==",EQ_O    ,pCMP  ,NoF  )
  ]

pExp ∷ CParser TokenWSBasic SExp
pExp = fmixfixWithContext "exp" $ concat
  -- parens
  [ fmixTerminal $ do
      void $ cpToken $ SyntaxTWSBasic "("
      e ← pExp
      void $ cpToken $ SyntaxTWSBasic ")"
      return $ extract e
  -- var
  , fmixTerminal $ Var_SE ^$ cpNewContext "var" cpNameWS
  -- integer
  , fmixTerminal $ Int_SE ^$ cpIntegerWS
  -- binary ops
  , concat $ mapOn binaryOps $ \ (s :* op :* level :* fy) →
      let mk = case fy of
            NoF → fmixInfix
            LeftF → fmixInfixL
            RightF → fmixInfixR
      in mk level $ do
        void $ cpSyntaxWS s
        return $ \ e₁ e₂ → Op_SE op $ list [e₁,e₂]
  , fmixPrefix pLET $ do
      void $ cpSyntaxWS "fun"
      x ← cpNameWS
      void $ cpSyntaxWS "=>"
      return $ Fun_SE x
  , fmixInfixL pAPP $ return $ App_SE
  ]

pCmd ∷ CParser TokenWSBasic SCmd
pCmd = cpNewWithContextRendered "cmd" $ do
  x ← cpNameWS
  concat
    [ do void $ cpSyntaxWS ":"
         τ ← pType
         return $ Decl_SC x τ
    , do void $ cpSyntaxWS "="
         e ← pExp
         return $ Defn_SC x e
    ]

pCmds ∷ CParser TokenWSBasic (𝐿 SCmd)
pCmds = cpManySepBy cpDelimWS pCmd

parse ∷ 𝕊 → 𝕊 → IO (𝐿 SCmd)
parse name = parseIO pCmds name *∘ lex name

parseMain ∷ 𝕊 → 𝕊 → IO ()
parseMain name = parseIOMain pCmds name *∘ lex name

testParser ∷ IO ()
testParser = parseMain "" $ concat $ inbetween "\n"
  [ "x : int"
  , "x = 1"
  , "y : int"
  , "y = 2"
  , "f : int -> int"
  , "f = fun x => fun y =>"
  , "  x + y"
  ]
