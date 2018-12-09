module UVMHS.Lib.Pretty.Core where

import UVMHS.Core

import UVMHS.Lib.Pretty.Color

---------------
-- PrettyEnv --
---------------


data Layout = Flat | Break
  deriving (Eq,Ord,Show)
data FailMode = CanFail | CannotFail
  deriving (Eq,Ord,Show)

data PrettyParams = PrettyParams
  { punctuationFormat        ∷ 𝐿 Format
  , keywordPunctuationFormat ∷ 𝐿 Format
  , keywordFormat            ∷ 𝐿 Format
  , constructorFormat        ∷ 𝐿 Format
  , operatorFormat           ∷ 𝐿 Format
  , binderFormat             ∷ 𝐿 Format
  , literalFormat            ∷ 𝐿 Format
  , highlightFormat          ∷ 𝐿 Format
  , headerFormat             ∷ 𝐿 Format
  , errorFormat              ∷ 𝐿 Format
  , lineNumberFormat         ∷ 𝐿 Format
  , appLevel                 ∷ ℕ
  } deriving (Eq,Ord,Show)
makeLenses ''PrettyParams

prettyParams₀ ∷ PrettyParams
prettyParams₀ = PrettyParams
  { punctuationFormat        = list [FG darkGray]
  , keywordPunctuationFormat = list [FG darkYellow,BD]
  , keywordFormat            = list [FG darkYellow,BD,UL]
  , constructorFormat        = list [FG darkGreen,BD]
  , operatorFormat           = list [FG darkBlue]
  , binderFormat             = list [FG darkTeal]
  , literalFormat            = list [FG darkRed]
  , highlightFormat          = list [BG highlight]
  , headerFormat             = list [FG darkPink,BD,UL]
  , errorFormat              = list [FG white,BG darkRed]
  , lineNumberFormat         = list [FG gray]
  , appLevel                 = 100
  }

data PrettyEnv = PrettyEnv
  -- global env
  { prettyParams ∷ PrettyParams
  , maxColumnWidth ∷ ℕ
  , maxRibbonWidth ∷ ℕ
  , doFormat ∷ 𝔹
  , doLineNumbers ∷ 𝔹
  , blinders ∷ 𝑂 (ℕ ∧ ℕ)
  -- local env
  , layout ∷ Layout
  , failMode ∷ FailMode
  , nesting ∷ ℕ
  , level ∷ ℕ
  , bumped ∷ 𝔹
  } deriving (Eq,Ord,Show)
makeLenses ''PrettyEnv

prettyEnv₀ ∷ PrettyEnv
prettyEnv₀ = PrettyEnv
  -- global env
  { prettyParams = prettyParams₀
  , maxColumnWidth = 100
  , maxRibbonWidth = 60
  , doFormat = True
  , doLineNumbers = False
  , blinders = None
  -- local env
  , layout = Break
  , failMode = CannotFail
  , nesting = 0
  , level = 0
  , bumped = False
  }

---------------
-- PrettyOut --
---------------

data Chunk = LineNumber ℕ | Text 𝕊 | Newline
  deriving (Eq, Ord,Show)
data Annotation = 
    FormatA      (𝐿 Format)
  | UndertagA    (𝐿 Format) ℂ
  deriving (Eq,Ord,Show)
type Output = 𝑄 OutputElem
data OutputElem =
    RawChunk Chunk
  | AnnotatedOutput Annotation Output
  deriving (Eq,Ord,Show)

data PrettyOut = PrettyOut
  { output ∷ Output
  , maxDisplayLineNumber ∷ ℕ
  } deriving (Eq,Ord,Show)
makeLenses ''PrettyOut
instance Null PrettyOut where null = PrettyOut null 0
instance Append PrettyOut where PrettyOut o₁ n₁ ⧺ PrettyOut o₂ n₂ = PrettyOut (o₁ ⧺ o₂) (n₁ ⊔ n₂) 
instance Monoid PrettyOut

-----------------
-- PrettyState --
-----------------

data PrettyState = PrettyState
  { column ∷ ℕ
  , ribbon ∷ ℕ
  , lineNumber ∷ ℕ
  , beginning ∷ 𝔹
  , displayLineNumber ∷ ℕ
  } deriving (Eq,Ord,Show)
makeLenses ''PrettyState

prettyState₀ ∷ PrettyState
prettyState₀ = PrettyState
  { column = 0
  , ribbon = 0
  , lineNumber = 0
  , beginning = True
  , displayLineNumber = 1
  }

-------------
-- PrettyM --
-------------

newtype PrettyM a = PrettyM { unPrettyM ∷ RWST PrettyEnv PrettyOut PrettyState 𝑂 a }
  deriving
  (Functor,Return,Bind,Monad
  ,MonadReader PrettyEnv
  ,MonadWriter PrettyOut
  ,MonadState PrettyState
  ,MonadFail)

runPrettyM ∷ PrettyEnv → PrettyState → PrettyM a → 𝑂 (PrettyState ∧ PrettyOut ∧ a)
runPrettyM r s = runRWST r s ∘ unPrettyM

evalPrettyM ∷ PrettyEnv → PrettyState → PrettyM a → 𝑂 a
evalPrettyM r s = evalRWST r s ∘ unPrettyM

execPrettyM ∷ PrettyM () → PrettyOut
execPrettyM aM =
  let errOut = PrettyOut
        { output = single $ AnnotatedOutput (FormatA $ errorFormat prettyParams₀) $ single $ RawChunk $ Text "<internal pretty printing error>"
        , maxDisplayLineNumber = 0
        }
  in ifNone errOut $ evalPrettyM prettyEnv₀ prettyState₀ $ retOut aM

-- # Low-Level Helpers

shouldOutput ∷ PrettyM 𝔹
shouldOutput = do
  ln ← getL lineNumberL
  bl ← askL blindersL
  return $ case bl of
    None → True
    Some (low :꘍ high) → (low ≤ ln) ⩓ (ln ≤ high)

shouldOutputNewline ∷ PrettyM 𝔹
shouldOutputNewline = do
  ln ← getL lineNumberL
  bl ← askL blindersL
  return $ case bl of
    None → True
    Some (low :꘍ high) → (low ≤ ln) ⩓ (ln < high)

spit ∷ 𝕊 → PrettyM ()
spit s = do
  modifyL columnL $ (+) $ length𝕊 s
  modifyL ribbonL $ (+) $ countWith (not ∘ isSpace) s
  whenM shouldOutput $ tellL outputL $ single $ RawChunk $ Text s

annotateOutput ∷ Annotation → Output → PrettyM Output
annotateOutput a o = do
  df ← askL doFormatL
  return $ case df of 
    True → single $ AnnotatedOutput a o
    False → o

doLineNumber ∷ 𝔹 → PrettyM ()
doLineNumber b = do
  when b $ do
    whenM (askL doLineNumbersL) $ do
      lnf ← askL $ lineNumberFormatL ⊚ prettyParamsL
      dln ← getL displayLineNumberL
      whenM shouldOutput $ do
        tellL outputL *$ annotateOutput (FormatA lnf) $ single $ RawChunk $ LineNumber dln
        tellL maxDisplayLineNumberL $ length𝕊 $ show𝕊 dln

doNesting ∷ 𝔹 → PrettyM ()
doNesting b = do
  when b $ do
    n ← askL nestingL
    spit $ build𝕊 $ repeat n " "

word ∷ 𝕊 → PrettyM ()
word s | isEmpty𝕊 s = skip
word s = do
  b ← getputL beginningL False
  doLineNumber b
  doNesting b
  spit s
  cf ← askL failModeL
  when (cf == CanFail) $ do
    cmax ← askL maxColumnWidthL
    rmax ← askL maxRibbonWidthL
    c ← getL columnL
    r ← getL ribbonL
    when (c > cmax) abort
    when (r > rmax) abort

newline ∷ PrettyM ()
newline = do
  whenM shouldOutputNewline $ tellL outputL $ single $ RawChunk Newline
  modifyL lineNumberL succ
  modifyL displayLineNumberL succ
  putL columnL 0
  putL ribbonL 0
  putL beginningL True

-- # Doc

newtype Doc = Doc { runDoc ∷ PrettyM () }
-- instance Eq Doc where (==) = (≡) `on` (normalizeOutput ∘ output ∘ execDoc)
-- instance Ord Doc where compare = compare `on` (normalizeOutput ∘ output ∘ execDoc)
instance Null Doc where null = Doc skip
instance Append Doc where d₁ ⧺ d₂ = Doc $ exec [runDoc d₁,runDoc d₂]
instance Monoid Doc
-- instance HasOrd [ChunkNF] Doc where ord = normalizeOutput ∘ output ∘ execDoc

execDoc ∷ Doc → PrettyOut
execDoc = execPrettyM ∘ runDoc

onDoc ∷ (PrettyM () → PrettyM ()) → Doc → Doc
onDoc f = Doc ∘ f ∘ runDoc

-- # Low Level Interface

ppSpace ∷ ℕ → Doc
ppSpace n = Doc $ word $ build𝕊 $ repeat n " "

ppNewline ∷ Doc
ppNewline = Doc newline

ppText ∷ 𝕊 → Doc
ppText = Doc ∘ exec ∘ inbetween newline ∘ map word ∘ splitOn𝕊 "\n"

ppAnnotate ∷ Annotation → Doc → Doc
ppAnnotate a aM = Doc $ do
  (o :꘍ ()) ← listenL outputL $ runDoc aM
  tellL outputL *$ annotateOutput a o

ppFormat ∷ 𝐿 Format → Doc → Doc
ppFormat = ppAnnotate ∘ FormatA

ppNoFormat ∷ Doc → Doc
ppNoFormat = onDoc $ mapEnv $ update doFormatL False

ppUndertagFormat ∷ 𝐿 Format → ℂ → Doc → Doc
ppUndertagFormat fmts = ppAnnotate ∘ UndertagA fmts

ppIfFlat ∷ Doc → Doc → Doc
ppIfFlat flatAction breakAction = Doc $ do
  l ← askL $ layoutL
  runDoc $ case l of
    Flat → flatAction
    Break → breakAction

ppTryFlat ∷ Doc → Doc
ppTryFlat = onDoc $ mapEnv $ update failModeL CanFail ∘ update layoutL Flat

ppFlat ∷ Doc → Doc
ppFlat = onDoc $ mapEnv $ update layoutL Flat

ppBreak ∷ Doc → Doc
ppBreak = onDoc $ mapEnv $ update layoutL Break

ppGroup ∷ Doc → Doc
ppGroup xM = ppIfFlat xM $ Doc $ tries [runDoc $ ppTryFlat xM,runDoc xM]

ppNest ∷ ℕ → Doc → Doc
ppNest n = onDoc $ mapEnv $ alter nestingL $ (+) n

ppAlign ∷ Doc → Doc
ppAlign d = Doc $ do
  i ← askL $ nestingL
  c ← getL columnL
  runDoc $ ppNest (c - (i ⊓ c)) d

ppLength ∷ Doc → ℕ
ppLength d = elim𝑂 0 column $ evalPrettyM prettyEnv₀ prettyState₀ $ retState $ runDoc d

ppFormatParam ∷ PrettyParams ⟢ 𝐿 Format → 𝕊 → Doc
ppFormatParam l s = Doc $ do
  fmt ← askL $ l ⊚ prettyParamsL
  runDoc $ ppFormat fmt $ ppText s

ppBlinders ∷ ℕ → ℕ → Doc → Doc
ppBlinders low high = onDoc $ mapEnv $ update blindersL $ Some (low :꘍ high)

ppLineNumbers ∷ Doc → Doc
ppLineNumbers = onDoc $ mapEnv $ update doLineNumbersL True

ppSetLineNumber ∷ ℕ → Doc → Doc
ppSetLineNumber n = onDoc $ localStateL displayLineNumberL n

-- # Formatting Helpers

ppFG ∷ Color → Doc → Doc
ppFG c = ppFormat $ list [FG c]

ppBG ∷ Color → Doc → Doc
ppBG c = ppFormat $ list [BG c]

ppUL ∷ Doc → Doc
ppUL = ppFormat $ list [UL]

ppBD ∷ Doc → Doc
ppBD = ppFormat $ list [BD]

ppPun ∷ 𝕊 → Doc
ppPun = ppFormatParam punctuationFormatL

ppKeyPun ∷ 𝕊 → Doc
ppKeyPun = ppFormatParam keywordPunctuationFormatL

ppKey ∷ 𝕊 → Doc
ppKey = ppFormatParam keywordFormatL

ppCon ∷ 𝕊 → Doc
ppCon = ppFormatParam constructorFormatL

ppOp ∷ 𝕊 → Doc
ppOp = ppFormatParam operatorFormatL

ppBdr ∷ 𝕊 → Doc
ppBdr = ppFormatParam binderFormatL

ppLit ∷ 𝕊 → Doc
ppLit = ppFormatParam literalFormatL

ppHl ∷ 𝕊 → Doc
ppHl = ppFormatParam highlightFormatL

ppHeader ∷ 𝕊 → Doc
ppHeader = ppFormatParam headerFormatL

ppErr ∷ 𝕊 → Doc
ppErr = ppFormatParam errorFormatL

ppUT ∷ ℂ → Color → Doc → Doc
ppUT c o = ppUndertagFormat (list [FG o]) c

ppAlignLeft ∷ ℕ → Doc → Doc
ppAlignLeft n d = concat [d,ppSpace $ n - (n ⊓ ppLength d)]

ppAlignRight ∷ ℕ → Doc → Doc
ppAlignRight n d = concat [ppSpace $ n - (n ⊓ ppLength d),d]

-- # Precedence

ppBotLevel ∷ Doc → Doc
ppBotLevel = Doc ∘ mapEnv (update levelL 0 ∘ update bumpedL False) ∘ runDoc

ppClosed ∷ Doc → Doc → Doc → Doc
ppClosed alM arM aM = concat $ map ppAlign
  [ alM
  , ppBotLevel aM
  , arM
  ]

ppParens ∷ Doc → Doc
ppParens = ppClosed (ppPun "(") (ppPun ")")

ppAtLevel ∷ ℕ → Doc → Doc
ppAtLevel i' aM = Doc $ do
  i ← askL $ levelL
  b ← askL $ bumpedL
  let aM' = onDoc (mapEnv $ update levelL i' ∘ update bumpedL False)  aM
  runDoc $ case (i < i') ⩔ ((i ≡ i') ⩓ not b) of
    True → aM'
    False → ppParens aM'

ppBump ∷ Doc → Doc
ppBump = Doc ∘ mapEnv (update bumpedL True) ∘ runDoc

ppInf ∷ ℕ → Doc → Doc → Doc → Doc
ppInf i oM x₁M x₂M = ppGroup $ ppAtLevel i $ ppSeparated $ list [ppBump x₁M,oM,ppBump x₂M]

ppInfl ∷ ℕ → Doc → Doc → Doc → Doc
ppInfl i oM x₁M x₂M = ppGroup $ ppAtLevel i $ ppSeparated $ list [x₁M,oM,ppBump x₂M]

ppInfr ∷ ℕ → Doc → Doc → Doc → Doc
ppInfr i oM x₁M x₂M = ppGroup $ ppAtLevel i $ ppSeparated $ list [ppBump x₁M,oM,x₂M]

ppPre ∷ ℕ → Doc → Doc → Doc
ppPre i oM xM = ppGroup $ ppAtLevel i $ ppSeparated $ list [oM,xM]

ppPost ∷ ℕ → Doc → Doc → Doc
ppPost i oM xM = ppGroup $ ppAtLevel i $ ppSeparated $ list [xM,oM]

ppApp ∷ Doc → 𝐿 Doc → Doc
ppApp x Nil = x
ppApp x xs = ppGroup $ Doc $ do
  l ← askL $ appLevelL ⊚ prettyParamsL
  runDoc $ ppAtLevel l $ ppSeparated $ x :& map ppBump xs

-- # Combinators

ppHorizontal ∷ 𝐿 Doc → Doc
ppHorizontal = concat ∘ inbetween (ppSpace 1) ∘ map ppAlign

ppVertical ∷ 𝐿 Doc → Doc
ppVertical = concat ∘ inbetween ppNewline ∘ map ppAlign

ppSoftline ∷ Doc
ppSoftline = ppIfFlat (ppSpace 1) ppNewline

ppSeparated ∷ 𝐿 Doc → Doc
ppSeparated = ppGroup ∘ concat ∘ inbetween ppSoftline ∘ map ppAlign

ppCollection ∷ 𝕊 → 𝕊 → 𝕊 → 𝐿 Doc → Doc
ppCollection open close sep xs = ppGroup $ ppBotLevel $ ppIfFlat flatCollection breakCollection
  where
    flatCollection = concat [ppPun open,concat $ inbetween (ppPun sep) xs,ppPun close]
    breakCollection = ppVertical $ concat
      [ list 
          $ mapFirst (\ x → ppHorizontal $ list [ppPun open,x]) 
          $ mapAfterFirst (\ x → ppHorizontal $ list [ppPun sep,x]) 
          $ map ppAlign 
          $ iter xs
      , return $ ppPun close
      ]

ppRecord ∷ 𝕊 → 𝐿 (Doc ∧ Doc) → Doc
ppRecord rel kvs = ppCollection "{" "}" "," $ map mapping kvs
  where
    mapping (k :꘍ v) = concat
      [ ppAlign k
      , ppIfFlat null $ ppSpace 1
      , ppPun rel
      , ppIfFlat null $ ppSpace 1
      , ppNest 2 $ ppGroup $ concat
          [ ppIfFlat null ppNewline
          , ppAlign v
          ]
      ]
