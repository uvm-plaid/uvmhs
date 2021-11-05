module UVMHS.Lib.Parser.Core where

import UVMHS.Core

import UVMHS.Lib.Pretty
import UVMHS.Lib.Window
import UVMHS.Lib.Annotated

import UVMHS.Lib.Parser.ParserInput
import UVMHS.Lib.Parser.ParserContext
import UVMHS.Lib.Parser.ParserError
import UVMHS.Lib.Parser.Loc

---------------
-- ParserEnv --
---------------

data ParserEnv = ParserEnv
  { parserEnvReportErrors ∷ 𝔹
  , parserEnvRenderFormat ∷ Formats
  , parserEnvErrorStack ∷ 𝕊 ∧ 𝐼 𝕊
  , parserEnvSourceName ∷ 𝕊
  }
makeLenses ''ParserEnv
makePrettyRecord ''ParserEnv

parserEnv₀ ∷ 𝕊 → ParserEnv
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
  , parserStateEndPos ∷ AddBT Loc
  , parserStateInput ∷ 𝑆 (ParserToken t)
  }
makeLenses ''ParserState
makePrettyRecord ''ParserState

parserState₀ ∷ 𝑆 (ParserToken t) → ParserState t
parserState₀ = ParserState null null null null $ AddBT bot

-- # Parser

newtype Parser t a = Parser { unParser ∷ ReaderT ParserEnv (StateT (ParserState t) (FailT ((∧) (ParserOut t)))) a } 
  deriving 
  ( Functor,Return,Bind,Monad
  , MonadFail
  , MonadReader ParserEnv
  , MonadWriter (ParserOut t)
  , MonadState (ParserState t)
  )

runParser ∷ ParserEnv → ParserState t → Parser t a → ParserOut t ∧ 𝑂 (ParserState t ∧ a)
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
  
pGetContextRendered ∷ Parser t SrcCxt
pGetContextRendered = do
  pp :* pc :* ps ← pGetContext
  n ← askL parserEnvSourceNameL
  return $ SrcCxt n (parserContextLocRange pc) pp (parserContextDisplayL pc) ps

pWithContext ∷ Parser t a → Parser t (WindowR Doc Doc ∧ ParserContext ∧ WindowL Doc Doc ∧ a)
pWithContext aM = do
  x ← aM
  pp :* pc :* ps ← pGetContext
  return $ pp :* pc :* ps :* x

pFail ∷ ParserContext → WindowL Doc Doc → Parser t a
pFail tc ps = do
  whenM (askL parserEnvReportErrorsL) $ do
    let l = locRangeEnd $ parserContextLocRange tc
        d = parserContextError tc
    e :* es ← askL parserEnvErrorStackL
    pp :* pc :* _ ← pGetContext
    tell $ AddNull $ ParserError l d ps $ single $ ParserErrorInfo pp (parserContextDisplayR pc) e es
  abort

pErr ∷ 𝕊 → Parser t a → Parser t a
pErr msg = mapEnv $ alter parserEnvErrorStackL $ \ (msg' :* stack) → msg :* (stack ⧺ single msg')

pNewErrContext ∷ 𝕊 → Parser t a → Parser t a
pNewErrContext msg = mapEnv $ update parserEnvErrorStackL $ msg :* null

pNewContext ∷ 𝕊 → Parser t a → Parser t a
pNewContext msg = pNewExpressionContext ∘ pNewErrContext msg

pWithContextRendered ∷ Parser t a → Parser t (𝐴 SrcCxt a)
pWithContextRendered aM = do
  x ← aM
  fc ← pGetContextRendered
  return $ 𝐴 fc x

pRender ∷ Formats → Parser t a → Parser t a
pRender fmt = mapEnv $ alter parserEnvRenderFormatL $ (⧺) fmt

pAdvance ∷ Parser t (AddBT Loc ∨ ParserToken t)
pAdvance = do
  pi ← getL parserStateInputL
  ep ← getL parserStateEndPosL
  case un𝑆 pi () of
    None → return $ Inl ep
    Some (ParserToken x sk tc ts :* pi') → do
      let ep' = bumpCol₁ ^$ locRangeEnd $ parserContextLocRange tc
      putL parserStateInputL pi'
      putL parserStateEndPosL ep'
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
    Inl l → {- pErr "more input" $ -} pFail (eofContext l) null
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
    Inr t → pNewContext "end of input" $ pFail (parserTokenContext t) (parserTokenSuffix t)

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
