module FP.Parser.Common where

import FP.Prelude
import FP.Pretty

-- # Loc

data Loc = Loc
  { locPos ∷ ℕ
  , locRow ∷ ℕ
  , locCol ∷ ℕ
  }
makeLenses ''Loc
makePrettyRecord ''Loc

loc₀ ∷ Loc
loc₀ = Loc bot bot bot

instance Eq Loc where
  (==) = (==) `on` locPos
instance Ord Loc where
  compare = (⋚) `on` locPos

instance Bot Loc where
  bot = Loc bot bot bot
instance Join Loc where
  l₁ ⊔ l₂ = case locPos l₁ ⋚ locPos l₂ of
    LT → l₂
    EQ → l₁
    GT → l₁
instance Meet Loc where
  l₁ ⊓ l₂ = case locPos l₁ ⋚ locPos l₂ of
    LT → l₁
    EQ → l₁
    GT → l₂

bumpRow ∷ Loc → Loc
bumpRow (Loc pos row _) = Loc (pos + 𝕟 1) (row + 𝕟 1) (𝕟 0)

bumpCol ∷ Loc → Loc
bumpCol (Loc pos row col) = Loc (pos + 𝕟 1) row (col + 𝕟 1)

-- # LocRange

data LocRange = LocRange
  { locRangeBegin ∷ Loc
  , locRangeEnd ∷ Loc
  } deriving (Eq, Ord)
makeLenses ''LocRange
makePrettyUnion ''LocRange

instance Join LocRange where
  LocRange b₁ e₁ ⊔ LocRange b₂ e₂ = LocRange (b₁ ⊓ b₂) (e₁ ⊔ e₂)

-- # SourceToken

data SourceToken t = SourceToken
  { sourceTokenValue ∷ t
  , sourceTokenRange ∷ LocRange
  , sourceTokenRender ∷ Doc
  , sourceTokenError ∷ Doc
  }
makeLenses ''SourceToken
makePrettyRecord ''SourceToken

renderChar ∷ ℂ → Doc
renderChar = ppText ∘ 𝕤

renderErrorChar ∷ ℂ → Doc
renderErrorChar '\n' = ppErr "\\n\n"
renderErrorChar c = renderChar c

tokens ∷ 𝕊 → Stream (SourceToken ℂ)
tokens (stream → Stream s₀ f) = streamState loc₀ $ MStream s₀ $ \ s → do
  (c,s') ← abort𝒪 $ f s
  loc ← get
  put $ if c == '\n'
    then bumpRow loc
    else bumpCol loc
  return (SourceToken c (LocRange loc loc) (renderChar c) (renderErrorChar c),s')

-- # SourceInput

data SourceInput t = SourceInput
  { sourceInputStream ∷ Stream (SourceToken t)
  , sourceInputNextLoc ∷ Loc
  }
makeLenses ''SourceInput
makePrettyRecord ''SourceInput

sourceInput₀ ∷ Stream (SourceToken t) → SourceInput t
sourceInput₀ ss = SourceInput ss loc₀

-- # SourceErrorTrace

data SourceErrorTrace = SourceErrorTrace
  { sourceErrorTraceFinal ∷ 𝒫 𝕊
  , sourceErrorTraceChain ∷ 𝕊 ⇰ SourceErrorTrace
  } deriving (Eq, Ord)
makeLenses ''SourceErrorTrace
makePrettyRecord ''SourceErrorTrace

instance Bot SourceErrorTrace where
  bot = SourceErrorTrace bot bot
instance Join SourceErrorTrace where
  SourceErrorTrace fin₁ ch₁ ⊔ SourceErrorTrace fin₂ ch₂ = SourceErrorTrace (fin₁ ⊔ fin₂) (ch₁ ⊔ ch₂)
instance JoinLattice SourceErrorTrace

sourceErrorTraceFromStack ∷ [𝕊] → 𝕊 → SourceErrorTrace
sourceErrorTraceFromStack [] fin = SourceErrorTrace (single fin) bot
sourceErrorTraceFromStack (msg:msgs) fin =
  SourceErrorTrace bot $ dict [msg ↦ sourceErrorTraceFromStack msgs fin]

displaySourceErrorTrace ∷ SourceErrorTrace → Doc
displaySourceErrorTrace (SourceErrorTrace final chain) = ppVertical $ concat
  [ if isEmpty final then null else return $ ppHorizontal $ concat
      [ single $ ppFG red $ ppText "Expected"
      , intersperse (ppFG red $ ppText "OR") $ map ppText $ list final
      ]
  , mapOn (list chain) $ \ (msg,tr) → ppVertical
      [ ppHorizontal
          [ ppFG darkGreen $ ppText "Parsing"
          , ppText msg
          ]
      , concat [ppSpace (𝕟 2),ppAlign $ displaySourceErrorTrace tr]
      ]
  ]

-- # SourceErrorInfo

data SourceErrorInfo = SourceErrorInfo
  { sourceErrorInfoPrefix ∷ Doc
  , sourceErrorInfoTrace ∷ SourceErrorTrace
  }
makeLenses ''SourceErrorInfo
makePrettyRecord ''SourceErrorInfo

-- # SourceError

data SourceError t = SourceError
  { sourceErrorInput ∷ SourceInput t
  , sourceErrorContexts ∷ (AddBot LocRange,Doc) ⇰ SourceErrorInfo
  }
makeLenses ''SourceError
makePrettyRecord ''SourceError

sourceErrorAppend ∷ SourceError t → SourceError t → SourceError t
sourceErrorAppend (SourceError pin₁ ectxs₁) (SourceError pin₂ ectxs₂) =
  case sourceInputNextLoc pin₁ ⋚ sourceInputNextLoc pin₂ of
    LT → SourceError pin₂ ectxs₂
    EQ →
      SourceError pin₁ $ unionWithDictOn ectxs₁ ectxs₂ $ \ pei₁ pei₂ →
        let SourceErrorInfo pre₁ trace₁ = pei₁
            SourceErrorInfo _    trace₂ = pei₂
        in SourceErrorInfo pre₁ (trace₁ ⊔ trace₂)
    GT → SourceError pin₁ ectxs₁

data SourceError𝒪 t = NullSourceError | SourceError𝒪 (SourceError t)

makePrisms ''SourceError𝒪
instance (Pretty t) ⇒ Pretty (SourceError𝒪 t) where
  pretty NullSourceError = ppCon "null"
  pretty (SourceError𝒪 e) = pretty e

instance Monoid (SourceError𝒪 t) where
  null = NullSourceError
  NullSourceError ⧺ pem = pem
  pem ⧺ NullSourceError = pem
  SourceError𝒪 pe₁ ⧺ SourceError𝒪 pe₂ = SourceError𝒪 $ pe₁ `sourceErrorAppend` pe₂

displaySourceError𝒪 ∷ SourceError𝒪 t → Doc
displaySourceError𝒪 NullSourceError = ppHeader "Nothing to Parse"
displaySourceError𝒪 (SourceError𝒪 (SourceError (SourceInput ts (Loc _ row col)) ectxs)) =
  ppVertical $ concat
  [ return $ ppHeader "Parse Failure"
  , return $ ppHorizontal
      [ ppErr ">"
      , concat [ppText "row:",pretty row]
      , concat [ppText "col:",pretty col]
      ]
  , return $ ppHeader "One Of:"
  , intersperse (ppHeader "OR") $ mapOn (list ectxs) $
    \ ((locRange,ctx),SourceErrorInfo pre etrace) →
        let (tokRange,nextTok,followStream) = case unconsStream ts of
              Nothing → (Bot,ppErr "EOF",null)
              Just (x,ts') → (AddBot $ sourceTokenRange x,sourceTokenError x,ts')
            blind = case locRange ⊔ tokRange of
              Bot → id
              AddBot (LocRange low high) → ppBlinders (locRow low) (locRow high)
        in
        ppVertical
          [ ppLineNumbers $ ppSetLineNumber (𝕟 0) $ blind $ concat
              [ pre
              , ppUT '^' green ctx
              , ppUT '^' red nextTok
              , concat $ map sourceTokenRender followStream
              ]
          , displaySourceErrorTrace etrace
          ]
  ]

-- # SourceContextPrefix

data SourceContextPrefix t = SourceContextPrefix
  { sourceContextPrefixBefore ∷ Doc
  , sourceContextPrefixDisplay ∷ Doc
  , sourceContextPrefixDisplayError ∷ Doc
  , sourceContextPrefixRange ∷ AddBot LocRange
  }
makeLenses ''SourceContextPrefix

instance Pretty (SourceContextPrefix t) where
  pretty (SourceContextPrefix prefix display displayError range) =
    ppRecord "="
      [ ppText "display"      ↦ prefix ⧺ ppUT '^' green display
      , ppText "displayError" ↦ prefix ⧺ ppUT '^' red displayError
      , ppText "range"        ↦ pretty range
      ]

instance Monoid (SourceContextPrefix t) where
  null = SourceContextPrefix null null null Bot
  pc₁ ⧺ pc₂ =
    let SourceContextPrefix pre₁ display₁ displayError₁ range₁ = pc₁
        SourceContextPrefix _    display₂ displayError₂ range₂ = pc₂
    in SourceContextPrefix pre₁
       (display₁ ⧺ display₂) (displayError₁ ⧺ displayError₂) (range₁ ⊔ range₂)

pushSourceLocalContext ∷ SourceContextPrefix t → SourceContextPrefix t
pushSourceLocalContext (SourceContextPrefix prefix display _ _) =
  SourceContextPrefix (prefix ⧺ display) null null bot

errorSourceLocalContext ∷ SourceInput t → ([𝕊],𝕊) → SourceContextPrefix t → SourceError t
errorSourceLocalContext pi (stack,message) (SourceContextPrefix prefix display _ range) =
  SourceError pi $ dict
    [(range,display) ↦ SourceErrorInfo prefix (sourceErrorTraceFromStack (reverse stack) message)]

sourceLocalContextFromToken ∷ [Format] → SourceToken t → SourceContextPrefix t
sourceLocalContextFromToken fmt (SourceToken _ range render renderError) =
  SourceContextPrefix null (ppFormat fmt render) (ppFormat fmt renderError) (AddBot range)

-- # SourceContext

data SourceContext t = SourceContext
  { sourceContextPast ∷ SourceContextPrefix t
  , sourceContextFuture ∷ SourceInput t
  }
instance Monoid (SourceContext t) where
  null = SourceContext null $ SourceInput null bot
  SourceContext pc₁ pi₁ ⧺ SourceContext pc₂ pi₂ =
    SourceContext (pc₁ ⧺ pc₂) $ maxBy sourceInputNextLoc pi₁ pi₂

instance Pretty (SourceContext t) where
  pretty (SourceContext (SourceContextPrefix pre display _ range) (SourceInput ss _)) =
    let ff = case range of
          Bot → id
          AddBot (LocRange begin end) → compose
            [ ppSetLineNumber (𝕟 0)
            , ppLineNumbers
            , ppBlinders (locRow begin) (locRow end)
            ]
    in ff $ pre ⧺ (ppUT '^' green display) ⧺ concat (map sourceTokenRender ss)

displaySourceContext ∷ SourceContext t → Doc
displaySourceContext (SourceContext (SourceContextPrefix pre display _ range) (SourceInput ss _)) =
    let ff = case range of
          Bot → id
          AddBot (LocRange begin end) → compose
            [ ppSetLineNumber (𝕟 0)
            , ppLineNumbers
            , ppBlinders (locRow begin) (locRow end)
            ]
    in ff $ pre ⧺ display ⧺ concat (map sourceTokenRender ss)

errorSourceContext ∷ SourceContext t → Doc
errorSourceContext (SourceContext (SourceContextPrefix pre _ displayError range) (SourceInput ss _)) =
  let ff = case range of
        Bot → id
        AddBot (LocRange begin end) → compose
          [ ppSetLineNumber (𝕟 0)
          , ppLineNumbers
          , ppBlinders (locRow begin) (locRow end)
          ]
  in ff $ pre ⧺ (ppUT '^' red displayError) ⧺ concat (map sourceTokenRender ss)
