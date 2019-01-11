module UVMHS.Lib.Parser.Core where

import UVMHS.Core
import UVMHS.Lib.Pretty

import UVMHS.Lib.Parser.ParserInput
import UVMHS.Lib.Parser.ParserContext
import UVMHS.Lib.Parser.ParserError
import UVMHS.Lib.Parser.Loc

---------------
-- ParserEnv --
---------------

data ParserEnv t = ParserEnv
  { parserEnvContextPadding ∷ ℕ
  , parserEnvRenderFormat ∷ 𝐿 Format
  , parserEnvErrorStack ∷ 𝐿 𝕊 ∧ 𝕊
  , parserEnvSkip ∷ t → 𝔹
  }
makeLenses ''ParserEnv
makePrettyRecord ''ParserEnv

parserEnv₀ ∷ ParserEnv t
parserEnv₀ = ParserEnv 1 null (null :* "<top level>") (const False)

---------------
-- ParserOut --
---------------

data ParserOut t = ParserOut
  { parserOutError ∷ AddNull ParserError
  }
makeLenses ''ParserOut
makePrettyRecord ''ParserOut

instance Null (ParserOut t) where null = ParserOut null
instance Append (ParserOut t) where ParserOut er₁ ⧺ ParserOut er₂ = ParserOut (er₁ ⧺ er₂)
instance Monoid (ParserOut t)

-----------------
-- ParserState --
-----------------

data ParserState t = ParserState
  { parserStateExpressionContext ∷ ExpressionContext
  , parserStateInputContext ∷ InputContext
  , parserStateInput ∷ ParserInput t
  }
makeLenses ''ParserState
makePrettyRecord ''ParserState

parserState₀ ∷ ParserInput t → ParserState t
parserState₀ = ParserState null null

-- # Parser

newtype Parser t a = Parser { unParser ∷ ReaderT (ParserEnv t) (StateT (ParserState t) (FailT ((∧) (ParserOut t)))) a 
  } deriving 
  ( Functor,Return,Bind,Monad
  , MonadFail
  , MonadReader (ParserEnv t)
  , MonadWriter (ParserOut t)
  , MonadState (ParserState t)
  )

runParser ∷ ParserEnv t → ParserState t → Parser t a → (ParserOut t ∧ 𝑂 (ParserState t ∧ a))
runParser e s = unFailT ∘ runStateT s ∘ runReaderT e ∘ unParser

-------------------------
-- Low Level Interface --
-------------------------

pFail ∷ ParserContext → Parser t a
pFail tc = do
  (es :* e) ← askL parserEnvErrorStackL
  ec ← getL parserStateExpressionContextL
  ic ← getL parserStateInputContextL
  is ← getL parserStateInputL
  cp ← askL parserEnvContextPaddingL
  let sc = renderParserInput $ prefixBeforeN𝑆 (succ cp) (parserContextNewlines ∘ parserTokenContext) $ parserInputStream is
  tellL parserOutErrorL $ AddNull $ ParserError tc sc $ dict [ec ↦ (ic :* makeStackTraces e (list $ reverse es))]
  abort

pErr ∷ 𝕊 → Parser t a → Parser t a
pErr msg = mapEnv $ alter parserEnvErrorStackL $ \ (stack :* msg') → (msg':&stack :* msg)

pNewWithContext ∷ 𝕊 → Parser t a → Parser t (InputContext ∧ ExpressionContext ∧ a)
pNewWithContext msg aM = do
  ic ← getL parserStateInputContextL
  ec ← getL parserStateExpressionContextL
  cp ← askL parserEnvContextPaddingL
  putL parserStateInputContextL $ InputContext $ truncateParserContext cp $ concat [unInputContext ic,unExpressionContext ec]
  putL parserStateExpressionContextL null
  a ← mapEnv (update parserEnvErrorStackL (null :* msg)) aM
  ic' ← getL parserStateInputContextL
  ec' ← getL parserStateExpressionContextL
  putL parserStateInputContextL $ ic
  putL parserStateExpressionContextL $ ec ⧺ ec'
  return $ ic' :* ec' :* a

pNew ∷ 𝕊 → Parser t a → Parser t a
pNew msg = map snd ∘ pNewWithContext msg

pRender ∷ Format → Parser t a → Parser t a
pRender fmt = mapEnv $ alter parserEnvRenderFormatL $ (⧺) $ single fmt

pAdvance ∷ Parser t (AddBot Loc ∨ ParserToken t)
pAdvance = do
  pi ← getL parserStateInputL
  case advanceInput pi of
    None → return $ Inl $ parserInputEndPos pi
    Some (t,pi') → do
      putL parserStateInputL pi'
      sk ← askL parserEnvSkipL
      case sk $ parserTokenValue t of
        True → do
          modifyL parserStateExpressionContextL $ \ ec → ec ⧺ ExpressionContext (parserTokenContext t)
          pAdvance
        False → do
          fmt ← askL parserEnvRenderFormatL
          return $ Inr $ ParserToken (parserTokenValue t) $ onParserContext (mapOut $ ppFormat fmt) $ parserTokenContext t

pPluck ∷ Parser t (ParserToken t)
pPluck = do
  tM ← pAdvance
  case tM of
    Inl l → pErr "more input" $ pFail $ renderEOFContext l
    Inr t → return t

pEnd ∷ Parser t ()
pEnd = do
  tM ← pAdvance
  case tM of
    Inl _ → return ()
    Inr t → pErr "end of input" $ pFail $ parserTokenContext t

----------------
-- High Level --
----------------

pFinal ∷ Parser t a → Parser t a
pFinal aM = do
  a ← aM
  pEnd
  return a

pAny ∷ Parser t t
pAny = do
  t ← pPluck
  modifyL parserStateExpressionContextL $ \ c → c ⧺ ExpressionContext (parserTokenContext t)
  return $ parserTokenValue t

pShaped ∷ 𝕊 → (t → 𝑂 a) → Parser t a
pShaped msg sh = do
  t ← pPluck
  case sh $ parserTokenValue t of
    None → pErr msg $ pFail $ parserTokenContext t
    Some x → do
      modifyL parserStateExpressionContextL $ \ c → c ⧺ ExpressionContext (parserTokenContext t)
      return x

pSatisfies ∷ 𝕊 → (t → 𝔹) → Parser t t
pSatisfies msg p = pShaped msg $ \ x → case p x of
  True → Some x 
  False → None

pDie ∷ 𝕊 → Parser t a
pDie msg = do
  void $ pSatisfies msg $ const False
  abort

pLit ∷ (Eq t,Pretty t) ⇒ t → Parser t t
pLit l = pSatisfies (ppshow l) $ (≡) l

pWord ∷ ∀ s t. (Pretty s,Eq t,Pretty t,s ⇄ 𝐼 t) ⇒ s → Parser t s
pWord s = pErr (ppshow s) $ isofr ^$ mapM pLit (isoto s ∷ 𝐼 t)

pOptional ∷ Parser t a → Parser t (𝑂 a)
pOptional p = tries [map Some p,return None]

pMany ∷ Parser t a → Parser t (𝐿 a)
pMany xM = tries
  [ pOneOrMore xM
  , return Nil
  ]

pOneOrMore ∷ Parser t a → Parser t (𝐿 a)
pOneOrMore xM = do
  x ← xM
  xs ← pMany xM
  return $ x:&xs

pManySepBy ∷ Parser t () → Parser t a → Parser t (𝐿 a)
pManySepBy sepM xM = tries
  [ pOneOrMoreSepBy sepM xM
  , return Nil
  ]

pOneOrMoreSepBy ∷ Parser t () → Parser t a → Parser t (𝐿 a)
pOneOrMoreSepBy sepM xM = do
  x ← xM
  xs ← map snd ^$ pMany $ sepM ⧆ xM
  return $ x :& xs

pSkip ∷ (t → 𝔹) → Parser t a → Parser t a
pSkip = localL parserEnvSkipL

------------------------
-- High-level Helpers --
------------------------

pLParen ∷ Parser ℂ ()
pLParen = void $ pLit '('

pRParen ∷ Parser ℂ ()
pRParen = void $ pLit ')'

pDigit ∷ Parser ℂ ℂ
pDigit = pSatisfies "digit [0-9]" isDigit

pNatural ∷ Parser ℂ ℕ
pNatural = read𝕊 ∘ string ^$ pOneOrMore pDigit

pInteger ∷ Parser ℂ ℤ
pInteger = do
  sign ← elim𝑂 "" single ^$ pOptional $ pLit '-'
  digits ← string ^$ pOneOrMore pDigit
  return $ read𝕊 $ sign ⧺ digits

pDouble ∷ Parser ℂ 𝔻
pDouble = do
  sign ← elim𝑂 "" single ^$ pOptional $ pLit '-'
  digits ← string ^$ pOneOrMore pDigit
  decimal ← elim𝑂 "" string ^$ pOptional $ do
    dot ← single ^$ pLit '.'
    digits' ← string ^$ pOneOrMore pDigit
    return $ dot ⧺ digits'
  return $ read𝕊 $ sign ⧺ digits ⧺ decimal

pNumber ∷ Parser ℂ (ℤ ∨ 𝔻)
pNumber = do
  sign ← elim𝑂 "" single ^$ pOptional $ pLit '-'
  digits ← string ^$ pOneOrMore pDigit
  decimal ← ifNone "" ^$ pOptional $ do
    dot ← single ^$ pLit '.'
    digits' ← string ^$ pOneOrMore pDigit
    return $ dot ⧺ digits'
  exp ← ifNone "" ^$ pOptional $ do
    e ← single ^$ pLit 'e'
    s ← elim𝑂 "" single ^$ pOptional $ pLit '-'
    digits' ← string ^$ pOneOrMore pDigit
    return $ e ⧺ s ⧺ digits'
  return $ case (decimal ≡ null) ⩓ (exp ≡ null) of
    True → Inl $ read𝕊 $ sign ⧺ digits
    False → Inr $ read𝕊 $ sign ⧺ digits ⧺ decimal ⧺ exp

pLetter ∷ Parser ℂ ℂ
pLetter = pSatisfies "letter [a-zA-Z]" isLetter

pName ∷ Parser ℂ 𝕊
pName = pNew "name" $ do
  s₁ ← single ^$ pSatisfies "first character" $ \ c → joins
    [ isLetter c
    ]
  s₂ ← string ^$ pMany $ pSatisfies "character" $ \ c → joins
    [ isLetter c 
    , isNumber c 
    , c ∈ pow "_-'′"
    ]
  return $ s₁ ⧺ s₂

pWhitespace ∷ Parser ℂ 𝕊
pWhitespace = string ^$ pOneOrMore $ pSatisfies "whitespace" isSpace

pOptionalWhitespace ∷ Parser ℂ ()
pOptionalWhitespace = void $ pOptional $ pWhitespace

pSurroundedBy ∷ Parser t () → Parser t () → Parser t a → Parser t a
pSurroundedBy luM ruM xM = do
  luM
  x ← xM
  ruM
  return x

pSurrounded ∷ Parser t () → Parser t a → Parser t a
pSurrounded uM = pSurroundedBy uM uM

---------------------
-- Running Parsers --
---------------------
             
displayErrorTraces ∷ ParserErrorStackTraces → Doc
displayErrorTraces (ParserErrorStackTraces final chain) = ppVertical $ list $ concat
  [ case isEmpty final of
      True → null 
      False → return $ ppHorizontal $ list $ concat
        [ single $ ppFG red $ ppText "Expected"
        , inbetween (ppFG red $ ppText "OR") $ map ppText $ iter final
        ]
  , mapOn (iter chain) $ \ (msg :* tr) → ppVertical $ list
      [ ppHorizontal $ list
          [ ppFG darkGreen $ ppText "Parsing"
          , ppText msg
          ]
      , concat [ppSpace 2,ppAlign $ displayErrorTraces tr]
      ]
  ]

displaySourceError ∷ AddNull ParserError → Doc
displaySourceError peM = ppVertical $ list $ concat
  [ return $ ppHeader "Parse Failure"
  , case peM of
      Null → return $ ppErr "> No Reported Errors"
      AddNull (ParserError tc sc fs) → concat
        [ return $ ppHorizontal $ list
            [ ppErr ">"
            , concat 
                [ ppText "line:"
                , elimAddBot (ppText "?") (pretty ∘ succ ∘ locRow) $ map locRangeEnd $ parserContextLocRange tc
                ]
            , concat 
                [ ppText "column:"
                , elimAddBot (ppText "?") (pretty ∘ succ ∘ locCol) $ map locRangeEnd $ parserContextLocRange tc
                ]
            ]
        , return $ ppHeader "One of:"
        , inbetween (ppHeader "OR") $ mapOn (iter fs) $ \ (ec :* (ic :* ets)) →
            let lineBegin = meets
                  [ elimAddBot Top (AddTop ∘ locRangeBegin) $ parserContextLocRange $ unInputContext ic
                  , elimAddBot Top (AddTop ∘ locRangeBegin) $ parserContextLocRange $ unExpressionContext ec
                  , elimAddBot Top (AddTop ∘ locRangeBegin) $ parserContextLocRange tc
                  ]
            in ppVertical $ list
              [ ppLineNumbers $ ppSetLineNumber (elimAddTop bot locRow lineBegin + 1) $ concat
                  [ execParserContextDoc $ execParserContext $ unInputContext ic
                  , ppUT '^' green $ execParserContextDoc $ execParserContext $ unExpressionContext ec
                  , ppUT '^' red $ execParserContextDoc $ parserContextError $ execParserContext tc
                  , execParserContextDoc sc
                  ]
              , displayErrorTraces ets
              ]
        ]
  ]
        
runParser₀ ∷ Parser t a → 𝑆 (ParserToken t) → ParserOut t ∧ 𝑂 (ParserState t ∧ a)
runParser₀ p ts = runParser parserEnv₀ (parserState₀ $ parserInput₀ ts) p

parse ∷ (Pretty a) ⇒ Parser t a → 𝑆 (ParserToken t) → Doc ∨ a
parse p ts = case runParser₀ (pFinal p) ts of
  (ParserOut pe :* None) → Inl $ displaySourceError pe
  (_ :* Some (_ :* x)) → Inr x

parseIO ∷ (Pretty a) ⇒ Parser t a → 𝑆 (ParserToken t) → IO a
parseIO p ss = case parse p ss of
  Inl d → pprint d ≫ abortIO
  Inr a → return a

parseIOMain ∷ (Pretty a) ⇒ Parser t a → 𝑆 (ParserToken t) → IO ()
parseIOMain p ss = do
  x ← parseIO p ss
  pprint $ ppVertical $ list
    [ ppHeader "Success"
    , pretty x
    ]

------------------------
-- Running Tokenizers --
------------------------

tokenize ∷ 𝐿 (Parser t a) → 𝑆 (ParserToken t) → Doc ∨ 𝐿 (ParserToken a)
tokenize ps ss = loop $ parserState₀ $ parserInput₀ ss
  where
    loop s
      | isEmpty $ parserInputStream $ parserStateInput s = return null
      | otherwise =
          let results = mapOn ps $ \ p → runParser parserEnv₀ s $ pNewWithContext "token" p
              pe' = concat $ map fst results
              xs = do
                s' :* (_ic :* ec :* t) ← mzero𝑂 *$ map snd results
                return $ map locPos (parserInputEndPos $ parserStateInput s') :* (s' :* ec :* t)
              xM = snd ^$ firstMaxByLT ((<) `on` fst) xs
          in case xM of
            None → Inl $ displaySourceError $ parserOutError pe'
            Some (s' :* ec :* t) → do
              ts ← loop s'
              return $ ParserToken t (unExpressionContext ec):&ts

tokenizeIO ∷ 𝐿 (Parser t a) → 𝑆 (ParserToken t) → IO (𝐿 (ParserToken a))
tokenizeIO ps ss = case tokenize ps ss of
  Inl d → pprint d ≫ abortIO
  Inr a → return a

tokenizeIOMain ∷ (Pretty a) ⇒ 𝐿 (Parser t a) → 𝑆 (ParserToken t) → IO ()
tokenizeIOMain ps ss = do
  x ← tokenizeIO ps ss
  pprint $ ppVertical $ list
    [ ppHeader "Success"
    , pretty $ map parserTokenValue x
    ]

pWithContext ∷ 𝕊 → Parser t a → Parser t (Annotated FullContext a)
pWithContext s p = do
  cp ← askL parserEnvContextPaddingL
  (ic :* ec :* x) ← pNewWithContext s p
  pi ← getL parserStateInputL
  let sc = renderParserInput $ prefixBeforeN𝑆 (succ cp) (parserContextNewlines ∘ parserTokenContext) $ parserInputStream pi
  return $ Annotated (FullContext ic ec sc) x
