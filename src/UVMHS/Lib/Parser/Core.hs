module UVMHS.Lib.Parser.Core where

import UVMHS.Core

import UVMHS.Lib.Pretty
import UVMHS.Lib.Window
import UVMHS.Lib.Annotated
import UVMHS.Lib.IterS

import UVMHS.Lib.Parser.ParserInput
import UVMHS.Lib.Parser.ParserContext
import UVMHS.Lib.Parser.ParserError
import UVMHS.Lib.Parser.Loc

---------------
-- ParserEnv --
---------------

data ParserEnv t = ParserEnv
  { parserEnvReportErrors ∷ 𝔹
  , parserEnvRenderFormat ∷ Formats
  , parserEnvErrorStack ∷ 𝕊 ∧ 𝐼 𝕊
  }
makeLenses ''ParserEnv
makePrettyRecord ''ParserEnv

parserEnv₀ ∷ ParserEnv t
parserEnv₀ = ParserEnv True null $ "<top level>" :* null

---------------
-- ParserOut --
---------------

type ParserOut t = AddNull (ParserError t)

-----------------
-- ParserState --
-----------------

data ParserState t = ParserState
  { parserStatePrefix ∷ WindowR Doc Doc
  , parserStateSkipContext ∷ ParserContext
  , parserStateContext ∷ ParserContext
  , parserStateSuffix ∷ WindowL Doc Doc
  , parserStateInput ∷ ParserInput t
  }
makeLenses ''ParserState
makePrettyRecord ''ParserState

parserState₀ ∷ ParserInput t → ParserState t
parserState₀ = ParserState null null null null

-- # Parser

newtype Parser t a = Parser { unParser ∷ ReaderT (ParserEnv t) (StateT (ParserState t) (FailT ((∧) (ParserOut t)))) a } 
  deriving 
  ( Functor,Return,Bind,Monad
  , MonadFail
  , MonadReader (ParserEnv t)
  , MonadWriter (ParserOut t)
  , MonadState (ParserState t)
  )

runParser ∷ ParserEnv t → ParserState t → Parser t a → ParserOut t ∧ 𝑂 (ParserState t ∧ a)
runParser e s = unFailT ∘ runStateT s ∘ runReaderT e ∘ unParser

-------------------------
-- Low Level Interface --
-------------------------

pNewExpressionContext ∷ Parser t a → Parser t a
pNewExpressionContext aM = do
  pp ← getL parserStatePrefixL
  pk ← getL parserStateSkipContextL
  pc ← getL parserStateContextL
  putL parserStatePrefixL $ concat [pp,parserContextDisplayR pk,parserContextDisplayR pc]
  putL parserStateSkipContextL null
  putL parserStateContextL null
  a ← aM
  pk' ← getL parserStateSkipContextL
  pc' ← getL parserStateContextL
  putL parserStatePrefixL pp
  if parserContextLocRange pc ≡ bot
    then do
      putL parserStateSkipContextL $ pk ⧺ pk'
      putL parserStateContextL pc'
    else do
      putL parserStateSkipContextL pk
      putL parserStateContextL $ pc ⧺ pk' ⧺ pc'
  return a

pGetContext ∷ Parser t (WindowR Doc Doc ∧ ParserContext ∧ WindowL Doc Doc)
pGetContext = do
  pp ← getL parserStatePrefixL
  pk ← getL parserStateSkipContextL
  pc ← getL parserStateContextL
  ps ← getL parserStateSuffixL
  return $ (pp ⧺ parserContextDisplayR pk) :* pc :* ps
  
pGetContextRendered ∷ Parser t FullContext
pGetContextRendered = do
  pp :* pc :* ps ← pGetContext
  return $ FullContext pp (parserContextDisplayL pc) ps

pWithContext ∷ Parser t a → Parser t (WindowR Doc Doc ∧ ParserContext ∧ WindowL Doc Doc ∧ a)
pWithContext aM = do
  x ← aM
  pp :* pc :* ps ← pGetContext
  return $ pp :* pc :* ps :* x

pFail ∷ ParserContext → WindowL Doc Doc → Parser t a
pFail tc ps = do
  whenM (askL parserEnvReportErrorsL) $ \ () → do
    let l = map locRangeEnd $ parserContextLocRange tc
        d = parserContextError tc
    e :* es ← askL parserEnvErrorStackL
    pp :* pc :* _ ← pGetContext
    tell $ AddNull $ ParserError l d ps $ single $ ParserErrorInfo pp (parserContextDisplayR pc) e es
  abort

pErr ∷ 𝕊 → Parser t a → Parser t a
pErr msg = mapEnv $ alter parserEnvErrorStackL $ \ (msg' :* stack) → msg :* (single msg' ⧺ stack)

pNewErrContext ∷ 𝕊 → Parser t a → Parser t a
pNewErrContext msg = mapEnv $ update parserEnvErrorStackL $ msg :* null

pNewContext ∷ 𝕊 → Parser t a → Parser t a
pNewContext msg = pNewExpressionContext ∘ pNewErrContext msg

pWithContextRendered ∷ Parser t a → Parser t (Annotated FullContext a)
pWithContextRendered aM = do
  x ← aM
  fc ← pGetContextRendered
  return $ Annotated fc x

pRender ∷ Formats → Parser t a → Parser t a
pRender fmt = mapEnv $ alter parserEnvRenderFormatL $ (⧺) fmt

pAdvance ∷ Parser t (AddBot Loc ∨ ParserToken t)
pAdvance = do
  pi ← getL parserStateInputL
  case advanceInput pi of
    None → return $ Inl $ parserInputEndPos pi
    Some (ParserToken x sk tc ts :* pi') → do
      putL parserStateInputL pi'
      if sk
        then do
          pk ← getL parserStateSkipContextL
          pc ← getL parserStateContextL
          if parserContextLocRange pc ≡ bot
            then putL parserStateSkipContextL $ pk ⧺ tc
            else putL parserStateContextL $ pc ⧺ tc
          pAdvance
        else do
          fmt ← askL parserEnvRenderFormatL
          return $ Inr $ ParserToken x sk (formatParserContext fmt tc) ts

pPluck ∷ Parser t (ParserToken t)
pPluck = do
  tM ← pAdvance
  case tM of
    Inl l → pErr "more input" $ pFail (eofContext l) null
    Inr t → return t

pRecord ∷ ParserToken t → Parser t ()
pRecord t = do
  modifyL parserStateContextL $ \ c → c ⧺ parserTokenContext t
  putL parserStateSuffixL $ parserTokenSuffix t 

pEnd ∷ Parser t ()
pEnd = do
  tM ← pAdvance
  case tM of
    Inl _ → return ()
    Inr t → pErr "end of input" $ pFail (parserTokenContext t) (parserTokenSuffix t)

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
  pRecord t
  return $ parserTokenValue t

pShaped ∷ {- 𝕊 → -} (t → 𝑂 a) → Parser t a
pShaped {- msg -} sh = do
  t ← pPluck
  case sh $ parserTokenValue t of
    None → {- pErr msg $ -} pFail (parserTokenContext t) (parserTokenSuffix t)
    Some x → do
      pRecord t
      return x

pSatisfies ∷ {- 𝕊 → -} (t → 𝔹) → Parser t t
pSatisfies {- msg -} p = pShaped {- msg -} $ \ x → case p x of
  True → Some x 
  False → None

pDie ∷ {- 𝕊 → -} Parser t a
pDie {- msg -} = do
  void $ pSatisfies {- msg -} $ const False
  abort

pToken ∷ (Eq t {- ,Pretty t -}) ⇒ t → Parser t t
pToken l = pSatisfies {- (ppshow l) -} $ (≡) l

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

------------------------
-- High-level Helpers --
------------------------

pWord ∷ ∀ s t. (Eq t,s ⇄ 𝐼 t) ⇒ s → Parser t s
pWord s = isofr ^$ mapM pToken (isoto s ∷ 𝐼 t)

-- pLParen ∷ Parser ℂ ()
-- pLParen = void $ pToken '('
-- 
-- pRParen ∷ Parser ℂ ()
-- pRParen = void $ pToken ')'
-- 
-- pDigit ∷ Parser ℂ ℂ
-- pDigit = pSatisfies {- "digit [0-9]" -} isDigit
-- 
-- pNatural ∷ Parser ℂ ℕ
-- pNatural = read𝕊 ∘ string ^$ pOneOrMore pDigit
-- 
-- pInteger ∷ Parser ℂ ℤ
-- pInteger = do
--   sign ← elim𝑂 "" single ^$ pOptional $ pToken '-'
--   digits ← string ^$ pOneOrMore pDigit
--   return $ read𝕊 $ sign ⧺ digits
-- 
-- pDouble ∷ Parser ℂ 𝔻
-- pDouble = do
--   sign ← elim𝑂 "" single ^$ pOptional $ pToken '-'
--   digits ← string ^$ pOneOrMore pDigit
--   decimal ← elim𝑂 "" string ^$ pOptional $ do
--     dot ← single ^$ pToken '.'
--     digits' ← string ^$ pOneOrMore pDigit
--     return $ dot ⧺ digits'
--   return $ read𝕊 $ sign ⧺ digits ⧺ decimal
-- 
-- pNumber ∷ Parser ℂ (ℤ ∨ 𝔻)
-- pNumber = do
--   sign ← elim𝑂 "" single ^$ pOptional $ pToken '-'
--   digits ← string ^$ pOneOrMore pDigit
--   decimal ← ifNone "" ^$ pOptional $ do
--     dot ← single ^$ pToken '.'
--     digits' ← string ^$ pOneOrMore pDigit
--     return $ dot ⧺ digits'
--   expr ← ifNone "" ^$ pOptional $ do
--     e ← single ^$ pToken 'e'
--     s ← elim𝑂 "" single ^$ pOptional $ pToken '-'
--     digits' ← string ^$ pOneOrMore pDigit
--     return $ e ⧺ s ⧺ digits'
--   return $ case (decimal ≡ null) ⩓ (expr ≡ null) of
--     True → Inl $ read𝕊 $ sign ⧺ digits
--     False → Inr $ read𝕊 $ sign ⧺ digits ⧺ decimal ⧺ expr
-- 
-- pLetter ∷ Parser ℂ ℂ
-- pLetter = pSatisfies {- "letter [a-zA-Z]" -} isLetter
-- 
-- pName ∷ Parser ℂ 𝕊
-- pName = do -- pNewContext "name" $ do
--   s₁ ← single ^$ pSatisfies {- "first character" -} $ \ c → joins
--     [ isLetter c
--     ]
--   s₂ ← string ^$ pMany $ pSatisfies {- "character" -} $ \ c → joins
--     [ isLetter c 
--     , isNumber c 
--     , c ∈ pow "_-'′"
--     ]
--   return $ s₁ ⧺ s₂
-- 
-- pWhitespace ∷ Parser ℂ 𝕊
-- pWhitespace = string ^$ pOneOrMore $ pSatisfies {- "whitespace" -} isSpace
-- 
-- pOptionalWhitespace ∷ Parser ℂ ()
-- pOptionalWhitespace = void $ pOptional $ pWhitespace
-- 
-- pSurroundedBy ∷ Parser t () → Parser t () → Parser t a → Parser t a
-- pSurroundedBy luM ruM xM = do
--   luM
--   x ← xM
--   ruM
--   return x
-- 
-- pSurrounded ∷ Parser t () → Parser t a → Parser t a
-- pSurrounded uM = pSurroundedBy uM uM
-- 
-- pComment ∷ Parser ℂ 𝕊
-- pComment = do -- pNewContext "comment" $ do
--   s₁ ← pWord "--"
--   s₂ ← string ^$ pMany $ pSatisfies {- "not newline" -} $ \ c → c ≢ '\n'
--   s₃ ← single ^$ pToken '\n'
--   return $ s₁ ⧺ s₂ ⧺ s₃
-- 
-- pCommentML ∷ Parser ℂ 𝕊
-- pCommentML = do -- pNewContext "multiline comment" $ do
--   s₁ ← pWord "{-"
--   s₂ ← afterOther
--   return $ s₁ ⧺ s₂
--   where
--     afterOther ∷ Parser ℂ 𝕊
--     afterOther = tries
--       [ do s₁ ← single ^$ pSatisfies {- "non-delimiter" -} $ \ c → c ∉ pow ['{','-']
--            s₂ ← afterOther
--            return $ s₁ ⧺ s₂
--       , do s₁ ← single ^$ pToken '{'
--            s₂ ← afterBrack
--            return $ s₁ ⧺ s₂
--       , do s₁ ← single ^$ pToken '-'
--            s₂ ← afterDash
--            return $ s₁ ⧺ s₂
--       ]
--     afterBrack ∷ Parser ℂ 𝕊
--     afterBrack = tries
--       [ do s₁ ← single ^$ pSatisfies {- "non-delimiter" -} $ \ c → c ∉ pow ['{','-']
--            s₂ ← afterOther
--            return $ s₁ ⧺ s₂
--       , do s₁ ← single ^$ pToken '{'
--            s₂ ← afterBrack
--            return $ s₁ ⧺ s₂
--       , do s₁ ← single ^$ pToken '-'
--            s₂ ← afterOther
--            s₃ ← afterOther
--            return $ s₁ ⧺ s₂ ⧺ s₃
--       ]
--     afterDash ∷ Parser ℂ 𝕊
--     afterDash = tries
--       [ do s₁ ← single ^$ pSatisfies {- "non-delimiter" -} $ \ c → c ∉ pow ['{','-','}']
--            s₂ ← afterOther
--            return $ s₁ ⧺ s₂
--       , do s₁ ← single ^$ pToken '{'
--            s₂ ← afterBrack
--            return $ s₁ ⧺ s₂
--       , do s₁ ← single ^$ pToken '-'
--            s₂ ← afterDash
--            return $ s₁ ⧺ s₂
--       , do single ^$ pToken '}'
--       ]

------------------------
-- Running Tokenizers --
------------------------

dep__tokenize ∷ ∀ t ts a. (ToStream (ParserToken t) ts) ⇒ 𝐿 (Parser t a) → 𝐿 (Parser t a) → ts → Doc ∨ 𝕍 (ParserToken a)
dep__tokenize sps rps ts = mapInr (vecS ∘ fst) $ loop $ parserState₀ $ parserInput₀ $ stream ts
  where
    loop ∷ ParserState t → Doc ∨ (𝐼S (ParserToken a) ∧ WindowL Doc Doc)
    loop s 
      | isEmpty $ parserInputStream $ parserStateInput s = return $ null :* null
      | otherwise =
          let results ∷ 𝐼 (ParserOut t ∧ 𝑂 (ParserState t ∧ (WindowR Doc Doc ∧ ParserContext ∧ WindowL Doc Doc ∧ a) ∧ 𝔹))
              results = concat
                [ mapOn (iter sps) $ \ p → 
                    mapSnd (map $ flip (:*) True) $ runParser parserEnv₀ s $ pNewContext "<token>" $ tries [localL parserEnvReportErrorsL False $ pWithContext p,pDie]
                , mapOn (iter rps) $ \ p → 
                    mapSnd (map $ flip (:*) False) $ runParser parserEnv₀ s $ pNewContext "<token>" $ tries [localL parserEnvReportErrorsL False $ pWithContext p,pDie]
                ]
              pe = concat $ map fst results
              xs = do
                s' :* (_pp :* pc :* _ps :* t) :* b ← mzero𝑂 *$ map snd results
                return $ map locPos (parserInputEndPos $ parserStateInput s') :* (s' :* pc :* t :* b)
              xM = snd ^$ firstMaxByLT ((<) `on` fst) xs
          in case xM of
            None → throw $ displaySourceError pe
            Some (s' :* pc :* t :* b) → do
              ts' :* ps ← loop s'
              return $ (single (ParserToken t b pc ps) ⧺ ts') :* (parserContextDisplayL pc ⧺ ps)

dep__tokenizeR ∷ (ToStream (ParserToken t) ts) ⇒ 𝐿 (Parser t a) → ts → Doc ∨ 𝕍 (ParserToken a)
dep__tokenizeR = dep__tokenize null

dep__tokenizeIO ∷ (ToStream (ParserToken t) ts) ⇒ 𝐿 (Parser t a) → 𝐿 (Parser t a) → ts → IO (𝕍 (ParserToken a))
dep__tokenizeIO sps rps ts = case dep__tokenize sps rps ts of
  Inl d → pprint d ≫ abortIO
  Inr a → return a

dep__tokenizeRIO ∷ (ToStream (ParserToken t) ts) ⇒ 𝐿 (Parser t a) → ts → IO (𝕍 (ParserToken a))
dep__tokenizeRIO = dep__tokenizeIO null

dep__tokenizeIOMain ∷ (Pretty a,ToStream (ParserToken t) ts) ⇒ 𝐿 (Parser t a) → 𝐿 (Parser t a) → ts → IO ()
dep__tokenizeIOMain sps rps ss = do
  x ← dep__tokenizeIO sps rps ss
  pprint $ ppVertical 
    [ ppHeader "Success"
    , pretty $ map parserTokenValue x
    ]

dep__tokenizeRIOMain ∷ (Pretty a,ToStream (ParserToken t) ts) ⇒ 𝐿 (Parser t a) → ts → IO ()
dep__tokenizeRIOMain = dep__tokenizeIOMain null
