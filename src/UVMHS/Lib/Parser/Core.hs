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

-- # RawParser

newtype RawParser t a = RawParser { unParser ∷ ReaderT ParserEnv (StateT (ParserState t) (FailT ((∧) (ParserOut t)))) a }
  deriving
  ( Functor,Return,Bind,Monad
  , MonadFail
  , MonadReader ParserEnv
  , MonadWriter (ParserOut t)
  , MonadState (ParserState t)
  )

runRawParser ∷ ParserEnv → ParserState t → RawParser t a → ParserOut t ∧ 𝑂 (ParserState t ∧ a)
runRawParser e s = unFailT ∘ runStateT s ∘ runReaderT e ∘ unParser

-------------------------
-- Low Level Interface --
-------------------------

rpNewExpressionContext ∷ RawParser t a → RawParser t a
rpNewExpressionContext aM = do
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

rpGetContext ∷ RawParser t (WindowR Doc Doc ∧ ParserContext ∧ WindowL Doc Doc)
rpGetContext = do
  pp ← getL parserStatePrefixL
  pk ← getL parserStateSkipContextL
  pc ← getL parserStateContextL
  ps ← getL parserStateSuffixL
  return $ (pp ⧺ parserContextDisplayR pk) :* pc :* ps

rpGetContextRendered ∷ RawParser t SrcCxt
rpGetContextRendered = do
  pp :* pc :* ps ← rpGetContext
  n ← askL parserEnvSourceNameL
  return $ SrcCxt n (parserContextLocRange pc) pp (parserContextDisplayL pc) ps

rpWithContext ∷ RawParser t a → RawParser t (WindowR Doc Doc ∧ ParserContext ∧ WindowL Doc Doc ∧ a)
rpWithContext aM = do
  x ← aM
  pp :* pc :* ps ← rpGetContext
  return $ pp :* pc :* ps :* x

rpFail ∷ ParserContext → WindowL Doc Doc → RawParser t a
rpFail tc ps = do
  whenM (askL parserEnvReportErrorsL) $ \ () → do
    let l = locRangeEnd $ parserContextLocRange tc
        d = parserContextError tc
    e :* es ← askL parserEnvErrorStackL
    pp :* pc :* _ ← rpGetContext
    tell $ AddNull $ ParserError l d ps $ single $ ParserErrorInfo pp (parserContextDisplayR pc) e es
  abort

rpErr ∷ 𝕊 → RawParser t a → RawParser t a
rpErr msg = mapEnv $ alter parserEnvErrorStackL $ \ (msg' :* stack) → msg :* (stack ⧺ single msg')

rpNewErrContext ∷ 𝕊 → RawParser t a → RawParser t a
rpNewErrContext msg = mapEnv $ update parserEnvErrorStackL $ msg :* null

rpNewContext ∷ 𝕊 → RawParser t a → RawParser t a
rpNewContext msg = rpNewExpressionContext ∘ rpNewErrContext msg

rpWithContextRendered ∷ RawParser t a → RawParser t (𝐴 SrcCxt a)
rpWithContextRendered aM = do
  x ← aM
  fc ← rpGetContextRendered
  return $ 𝐴 fc x

rpRender ∷ Formats → RawParser t a → RawParser t a
rpRender fmt = mapEnv $ alter parserEnvRenderFormatL $ (⧺) fmt

rpAdvance ∷ RawParser t (AddBT Loc ∨ ParserToken t)
rpAdvance = do
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
          rpAdvance
        else do
          fmt ← askL parserEnvRenderFormatL
          return $ Inr $ ParserToken x sk (formatParserContext fmt tc) ts

rpPluck ∷ RawParser t (ParserToken t)
rpPluck = do
  tM ← rpAdvance
  case tM of
    Inl l → {- rpErr "more input" $ -} rpFail (eofContext l) null
    Inr t → return t

rpRecord ∷ ParserToken t → RawParser t ()
rpRecord t = do
  modifyL parserStateContextL $ \ c → c ⧺ parserTokenContext t
  putL parserStateSuffixL $ parserTokenSuffix t

rpEnd ∷ RawParser t ()
rpEnd = do
  tM ← rpAdvance
  case tM of
    Inl _ → return ()
    Inr t → rpNewContext "end of input" $ rpFail (parserTokenContext t) (parserTokenSuffix t)

----------------
-- High Level --
----------------

rpFinal ∷ RawParser t a → RawParser t a
rpFinal aM = do
  a ← aM
  rpEnd
  return a

rpAny ∷ RawParser t t
rpAny = do
  t ← rpPluck
  rpRecord t
  return $ parserTokenValue t

rpShaped ∷ {- 𝕊 → -} (t → 𝑂 a) → RawParser t a
rpShaped {- msg -} sh = do
  t ← rpPluck
  case sh $ parserTokenValue t of
    None → {- rpErr msg $ -} rpFail (parserTokenContext t) (parserTokenSuffix t)
    Some x → do
      rpRecord t
      return x

rpDie ∷ RawParser t a
rpDie = do
  t ← rpPluck
  rpFail (parserTokenContext t) $ parserTokenSuffix t

rpGuard ∷ 𝔹 → RawParser t ()
rpGuard b = if b then skip else rpDie

rpFailEff ∷ 𝑂 a → RawParser t a
rpFailEff = elim𝑂 (const rpDie) return

rpSatisfies ∷ {- 𝕊 → -} (t → 𝔹) → RawParser t t
rpSatisfies {- msg -} p = rpShaped {- msg -} $ \ x → case p x of
  True → Some x
  False → None

rpToken ∷ (Eq t {- ,Pretty t -}) ⇒ t → RawParser t t
rpToken l = rpSatisfies {- (ppshow l) -} $ (≡) l

rpOptional ∷ RawParser t a → RawParser t (𝑂 a)
rpOptional p = tries [map Some p,return None]

rpMany ∷ RawParser t a → RawParser t (𝐿 a)
rpMany xM = tries
  [ rpOneOrMore xM
  , return Nil
  ]

rpOneOrMore ∷ RawParser t a → RawParser t (𝐿 a)
rpOneOrMore xM = do
  x ← xM
  xs ← rpMany xM
  return $ x:&xs

rpManySepBy ∷ RawParser t () → RawParser t a → RawParser t (𝐿 a)
rpManySepBy sepM xM = tries
  [ rpOneOrMoreSepBy sepM xM
  , return Nil
  ]

rpOneOrMoreSepBy ∷ RawParser t () → RawParser t a → RawParser t (𝐿 a)
rpOneOrMoreSepBy sepM xM = do
  x ← xM
  xs ← map snd ^$ rpMany $ sepM ⧆ xM
  return $ x :& xs

------------------------
-- High-level Helpers --
------------------------

rpWord ∷ ∀ s t. (Eq t,s ⇄ 𝐼 t) ⇒ s → RawParser t s
rpWord s = isofr ^$ mapM rpToken (isoto s ∷ 𝐼 t)
