module UVMHS.Lib.Pretty.Doc where

import UVMHS.Core

import UVMHS.Lib.Pretty.Annotation
import UVMHS.Lib.Pretty.Color
import UVMHS.Lib.Pretty.Common
import UVMHS.Lib.Pretty.DocA
import UVMHS.Lib.Pretty.Shape
import UVMHS.Lib.Pretty.RenderUndertags

import qualified GHC.Stack as Stack
import qualified Prelude as HS

-- Doc renders local configuration options such as colors and
-- formatting

data PrettyParams = PrettyParams
  { punctuationFormat        ∷ Formats
  , keywordFormat            ∷ Formats
  , constructorFormat        ∷ Formats
  , operatorFormat           ∷ Formats
  , primitiveFormat          ∷ Formats
  , binderFormat             ∷ Formats
  , literalFormat            ∷ Formats
  , highlightFormat          ∷ Formats
  , headerFormat             ∷ Formats
  , commentFormat            ∷ Formats
  , errorFormat              ∷ Formats
  , lineNumberFormat         ∷ Formats
  , appLevel                 ∷ ℕ64
  } deriving (Eq,Ord,Show)
makeLenses ''PrettyParams

prettyParams₀ ∷ PrettyParams
prettyParams₀ = PrettyParams
  { punctuationFormat        = formats [FG grayDark]
  , keywordFormat            = formats [BD,FG yellow]
  , constructorFormat        = formats [BD,FG green]
  , operatorFormat           = formats [FG teal]
  , primitiveFormat          = formats [FG green]
  , binderFormat             = formats [FG blue]
  , literalFormat            = formats [FG red]
  , highlightFormat          = formats [BG highlight]
  , headerFormat             = formats [BD,UL,FG pink]
  , commentFormat            = formats [IT,FG grayLight]
  , errorFormat              = formats [FG white,BG red]
  , lineNumberFormat         = formats [FG grayLight]
  , appLevel                 = 𝕟64 100
  }

data DocEnv = DocEnv
  -- global env
  { docEnvPrettyParams ∷ PrettyParams
  -- local env
  , docEnvPrecLevel ∷ ℕ64
  , docEnvPrecBumped ∷ 𝔹
  } deriving (Eq,Ord,Show)
makeLenses ''DocEnv

docEnv₀ ∷ DocEnv
docEnv₀ = DocEnv
  -- global env
  { docEnvPrettyParams = prettyParams₀
  -- local env
  , docEnvPrecLevel = 𝕟64 0
  , docEnvPrecBumped = False
  }

type DocM = RWS DocEnv DocA ()
newtype Doc = Doc { unDoc ∷ DocM () }

execDocWith ∷ (DocM () → DocM ()) → Doc → DocA
execDocWith f = evalRWS docEnv₀ () ∘ retOut ∘ f ∘ unDoc

execDoc ∷ Doc → DocA
execDoc = execDocWith id

onDoc ∷ (DocM () → DocM ()) → Doc → Doc
onDoc f (Doc xM) = Doc $ f xM

onDoc2 ∷ (DocM () → DocM () → DocM ()) → Doc → Doc → Doc
onDoc2 f (Doc xM₁) (Doc xM₂) = Doc $ f xM₁ xM₂

instance Null Doc where null = Doc skip
instance Append Doc where (⧺) = onDoc2 (≫)
instance Monoid Doc

docShape ∷ Doc → ShapeA
docShape = summaryIShape ∘ staticDocA ∘ execDoc

-----------------
-- COMBINATORS --
-----------------

ppForceBreak ∷ Doc
ppForceBreak = Doc $ tell $ StaticDocA $ SummaryI True null null

ppAnnotate ∷ Annotation → Doc → Doc
ppAnnotate = onDoc ∘ mapOut ∘ annotateDocA

ppFormat ∷ Formats → Doc → Doc
ppFormat = ppAnnotate ∘ formatAnnotation

ppFormatParam ∷ PrettyParams ⟢ Formats → Doc → Doc
ppFormatParam l d = Doc $ do
  fmt ← askL $ l ⊚ docEnvPrettyParamsL
  unDoc $ ppFormat fmt d

ppUndertag ∷ ℂ → Formats → Doc → Doc
ppUndertag = ppAnnotate ∘∘ undertagAnnotation

ppGroup ∷ Doc → Doc
ppGroup = onDoc $ mapOut groupDocA

ppAlign ∷ Doc → Doc
ppAlign = onDoc $ mapOut alignDocA

ppGA ∷ Doc → Doc
ppGA = ppAlign ∘ ppGroup

ppString ∷ 𝕊 → Doc
ppString = Doc ∘ tell ∘ stringDocA

ppStringModal ∷ 𝕊 → 𝕊 → Doc
ppStringModal sf sb = ppModal (ppString sf) $ ppString sb

ppModal ∷ Doc → Doc → Doc
ppModal d₁ d₂ = Doc $ do
  da₁ ← retOut $ unDoc d₁
  da₂ ← retOut $ unDoc d₂
  tell $ docAModal da₁ da₂

ppFG ∷ Color → Doc → Doc
ppFG c = ppFormat $ formats [FG c]

ppBG ∷ Color → Doc → Doc
ppBG c = ppFormat $ formats [BG c]

ppUL ∷ Doc → Doc
ppUL = ppFormat $ formats [UL]

ppBD ∷ Doc → Doc
ppBD = ppFormat $ formats [BD]

ppIT ∷ Doc → Doc
ppIT = ppFormat $ formats [IT]

ppUT ∷ ℂ → Color → Doc → Doc
ppUT c o = ppUndertag c (formats [FG o])

ppPunFmt ∷ Doc → Doc
ppPunFmt = ppFormatParam punctuationFormatL 

ppPun ∷ 𝕊 → Doc
ppPun = ppPunFmt ∘ ppString

ppKeyFmt ∷ Doc → Doc
ppKeyFmt = ppFormatParam keywordFormatL 

ppKey ∷ 𝕊 → Doc
ppKey = ppKeyFmt ∘ ppString

ppConFmt ∷ Doc → Doc
ppConFmt = ppFormatParam constructorFormatL

ppCon ∷ 𝕊 → Doc
ppCon = ppConFmt ∘ ppString

ppOpFmt ∷ Doc → Doc
ppOpFmt = ppFormatParam operatorFormatL

ppOp ∷ 𝕊 → Doc
ppOp = ppOpFmt ∘ ppString

ppPrimFmt ∷ Doc → Doc
ppPrimFmt = ppFormatParam primitiveFormatL

ppPrim ∷ 𝕊 → Doc
ppPrim = ppPrimFmt ∘ ppString

ppBdrFmt ∷ Doc → Doc
ppBdrFmt = ppFormatParam binderFormatL

ppBdr ∷ 𝕊 → Doc
ppBdr = ppBdrFmt  ∘ ppString

ppLitFmt ∷ Doc → Doc
ppLitFmt = ppFormatParam literalFormatL

ppLit ∷ 𝕊 → Doc
ppLit = ppLitFmt ∘ ppString

ppHlFmt ∷ Doc → Doc
ppHlFmt = ppFormatParam highlightFormatL

ppHl ∷ 𝕊 → Doc
ppHl = ppHlFmt ∘ ppString

ppHeaderFmt ∷ Doc → Doc
ppHeaderFmt = ppFormatParam headerFormatL

ppHeader ∷ 𝕊 → Doc
ppHeader = ppHeaderFmt ∘ ppString

ppCommentFmt ∷ Doc → Doc
ppCommentFmt = ppFormatParam commentFormatL

ppComment ∷ 𝕊 → Doc
ppComment = ppCommentFmt ∘ ppString

ppErrFmt ∷ Doc → Doc
ppErrFmt = ppFormatParam errorFormatL

ppErr ∷ 𝕊 → Doc
ppErr = ppErrFmt ∘ ppString

ppLineNumFmt ∷ Doc → Doc
ppLineNumFmt = ppFormatParam lineNumberFormatL

ppLineNum ∷ 𝕊 → Doc
ppLineNum = ppLineNumFmt ∘ ppString

ppSpace ∷ ℕ64 → Doc
ppSpace n = ppString $ string $ replicate n ' '

ppNewline ∷ Doc
ppNewline = ppString "\n"

ppSpaceIfBreak ∷ Doc
ppSpaceIfBreak = ppStringModal "" " "

ppSpaceIfNoBreak ∷ Doc
ppSpaceIfNoBreak = ppStringModal " " ""

ppNewlineIfBreak ∷ Doc
ppNewlineIfBreak = ppStringModal "" "\n"

ppSpaceNewlineIfBreak ∷ Doc
ppSpaceNewlineIfBreak = ppStringModal " " "\n"

ppHorizontal ∷ (ToIter Doc t) ⇒ t → Doc
ppHorizontal = concat ∘ inbetween (ppSpace $ 𝕟64 1) ∘ iter

ppVertical ∷ (ToIter Doc t) ⇒ t → Doc
ppVertical = concat ∘ inbetween ppNewline ∘ iter

ppSeparated ∷ (ToIter Doc t) ⇒ t → Doc
ppSeparated = ppGroup ∘ concat ∘ inbetween ppSpaceNewlineIfBreak ∘ iter

ppSetLevel ∷ ℕ64 → Doc → Doc
ppSetLevel n = onDoc $ mapEnv $ update docEnvPrecLevelL n ∘ update docEnvPrecBumpedL False

ppSetBotLevel ∷ Doc → Doc
ppSetBotLevel = ppSetLevel zero

ppBump ∷ Doc → Doc
ppBump = onDoc $ mapEnv $ update docEnvPrecBumpedL True

ppClosed ∷ Doc → Doc → Doc → Doc
ppClosed alM arM aM = ppSetBotLevel $ concat
  [ alM
  , ppGA aM
  , arM
  ]

ppParens ∷ Doc → Doc
ppParens = ppClosed (ppPun "(") (ppPun ")")

ppLevel ∷ ℕ64 → Doc → Doc
ppLevel i' aM = Doc $ do
  i ← askL $ docEnvPrecLevelL
  b ← askL $ docEnvPrecBumpedL
  unDoc $ case (i < i') ⩔ ((i ≡ i') ⩓ not b) of
    True → ppSetLevel i' aM
    False → ppParens $ ppSetLevel i' aM

ppInfLevel ∷ ℕ64 → Doc → Doc → Doc → Doc
ppInfLevel i oM x₁M x₂M = ppLevel i $ concat $ iter [ppBump x₁M,oM,ppBump x₂M]

ppInflLevel ∷ ℕ64 → Doc → Doc → Doc → Doc
ppInflLevel i oM x₁M x₂M = ppLevel i $ concat $ iter [x₁M,oM,ppBump x₂M]

ppInfrLevel ∷ ℕ64 → Doc → Doc → Doc → Doc
ppInfrLevel i oM x₁M x₂M = ppLevel i $ concat $ iter [ppBump x₁M,oM,x₂M]

ppPreLevel ∷ ℕ64 → Doc → Doc → Doc
ppPreLevel i oM xM = ppLevel i $ concat $ iter [oM,xM]

ppPostLevel ∷ ℕ64 → Doc → Doc → Doc
ppPostLevel i oM xM = ppLevel i $ concat $ iter [xM,oM]

ppInf ∷ ℕ64 → Doc → Doc → Doc → Doc
ppInf i o e₁ e₂ = 
  ppInfLevel i (concat [ppNewlineIfBreak,ppAlign o,ppSpaceIfBreak]) (ppGA e₁) $ ppGA e₂

ppInfl ∷ ℕ64 → Doc → Doc → Doc → Doc
ppInfl i o e₁ e₂ = 
  ppInflLevel i (concat [ppNewlineIfBreak,ppAlign o,ppSpaceIfBreak]) (ppGA e₁) $ ppGA e₂

ppInfr ∷ ℕ64 → Doc → Doc → Doc → Doc
ppInfr i o e₁ e₂ = 
  ppInfrLevel i (concat [ppNewlineIfBreak,ppAlign o,ppSpaceIfBreak]) (ppGA e₁) $ ppGA e₂

ppPre ∷ ℕ64 → Doc → Doc → Doc
ppPre i o e = ppPreLevel i (concat [ppAlign o,ppNewlineIfBreak]) $ ppGA e

ppPost ∷ ℕ64 → Doc → Doc → Doc
ppPost i o e = ppPostLevel i (concat [ppNewlineIfBreak,ppAlign o]) $ ppGA e

ppInfSep ∷ ℕ64 → Doc → Doc → Doc → Doc
ppInfSep i o = ppInf i $ ppSpaceIfNoBreak ⧺ o ⧺ ppSpaceIfNoBreak

ppInflSep ∷ ℕ64 → Doc → Doc → Doc → Doc
ppInflSep i o = ppInfl i $ ppSpaceIfNoBreak ⧺ o ⧺ ppSpaceIfNoBreak

ppInfrSep ∷ ℕ64 → Doc → Doc → Doc → Doc
ppInfrSep i o = ppInfr i $ ppSpaceIfNoBreak ⧺ o ⧺ ppSpaceIfNoBreak

ppPreSep ∷ ℕ64 → Doc → Doc → Doc
ppPreSep i o = ppPre i $ o ⧺ ppSpaceIfNoBreak

ppPostSep ∷ ℕ64 → Doc → Doc → Doc
ppPostSep i o = ppPost i $ ppSpaceIfNoBreak ⧺ o

ppInf' ∷ ℕ64 → Doc → Doc → Doc → Doc
ppInf' i o e₁ e₂ = 
  ppInfLevel i (concat [ppNewlineIfBreak,ppAlign o,ppNewlineIfBreak]) (ppGA e₁) $ ppGA e₂

ppInfl' ∷ ℕ64 → Doc → Doc → Doc → Doc
ppInfl' i o e₁ e₂ = 
  ppInflLevel i (concat [ppNewlineIfBreak,ppAlign o,ppNewlineIfBreak]) (ppGA e₁) $ ppGA e₂

ppInfr' ∷ ℕ64 → Doc → Doc → Doc → Doc
ppInfr' i o e₁ e₂ = 
  ppInfrLevel i (concat [ppNewlineIfBreak,ppAlign o,ppNewlineIfBreak]) (ppGA e₁) $ ppGA e₂

ppInfSep' ∷ ℕ64 → Doc → Doc → Doc → Doc
ppInfSep' i o = ppInf' i $ ppSpaceIfNoBreak ⧺ o ⧺ ppSpaceIfNoBreak

ppInflSep' ∷ ℕ64 → Doc → Doc → Doc → Doc
ppInflSep' i o = ppInfl' i $ ppSpaceIfNoBreak ⧺ o ⧺ ppSpaceIfNoBreak

ppInfrSep' ∷ ℕ64 → Doc → Doc → Doc → Doc
ppInfrSep' i o = ppInfr' i $ ppSpaceIfNoBreak ⧺ o ⧺ ppSpaceIfNoBreak


ppApp ∷ (ToIter Doc t) ⇒ Doc → t → Doc
ppApp x xs 
  | count xs ≡ 𝕟64 0 = ppAlign x
  | otherwise = Doc $ do
    l ← askL $ appLevelL ⊚ docEnvPrettyParamsL
    unDoc $ ppLevel l $ concat 
      [ ppGA x
      , ppSpaceNewlineIfBreak
      , concat $ inbetween ppSpaceNewlineIfBreak $ map (ppGA ∘ ppBump) $ iter xs
      ]

ppCollection ∷ (ToIter Doc t) ⇒ Doc → Doc → Doc → t → Doc
ppCollection l r i xs = ppSetBotLevel $ concat
  [ l
  , ppSpaceIfBreak
  , concat $ inbetween spacer $ map ppGA $ iter xs
  , ppNewlineIfBreak
  , r
  ]
  where
    spacer ∷ Doc
    spacer = concat
      [ ppNewlineIfBreak
      , i
      , ppSpaceIfBreak
      ]

ppRecord ∷ (ToIter (Doc ∧ Doc) t) ⇒ Doc → t → Doc
ppRecord rel kvs = ppCollection (ppPun "{") (ppPun "}") (ppPun ",") $ map mapping $ iter kvs
  where
    mapping (k :* v) = concat
      [ ppGA k
      , ppSpaceIfBreak
      , rel
      , ppNewlineIfBreak
      , ppSpaceIfBreak
      , ppSpaceIfBreak
      , ppGA v
      ]


matrixHelper ∷ (𝒩 m,𝒩 n) ⇒ 𝕍S n HAlign → 𝕍S m VAlign → 𝕍S m (𝕍S n SummaryO) → 𝕍S n ℕ64 ∧ 𝕍S m (𝕍S n SummaryO)
matrixHelper has vas sss =
  let sssT       = 𝐭 sss
      rowHeights = mapOn sss  $ \ ss → joins $ mapOn ss $ \ s → shapeNewlines $ summaryOShape s
      colWidths  = mapOn sssT $ \ ss → joins $ mapOn ss $ \ s → shapeWidth    $ summaryOShape s
      sss'       = svecF 𝕟64s $ \ i → svecF 𝕟64s $ \ j → hvalign (has ⋕ j) (vas ⋕ i) (colWidths ⋕ j) (rowHeights ⋕ i) $ sss ⋕ i ⋕ j
  in colWidths :* sss'

ppMatrix ∷ (𝒩 m,𝒩 n) ⇒ 𝕍S n HAlign → 𝕍S m VAlign → 𝕍S m (𝕍S n Doc) → Doc
ppMatrix has vas dss =
  let sss       = mapp (execRenderUT ∘ summaryIContents ∘ staticDocA ∘ execDoc) dss
      _ :* sss' = matrixHelper has vas sss
      dss'      = svecF 𝕟64s $ \ i → svecF 𝕟64s $ \ j →
        let SummaryO sh t = sss' ⋕ i ⋕ j
        in Doc $ tell $ StaticDocA $ SummaryI True (ShapeA False sh) $ treeIO t
  in 
  ppVertical $ mapOn dss' $ \ ds →
    ppHorizontal $ inbetween null ds

ppMatrixCells ∷ (𝒩 m,𝒩 n) ⇒ 𝕍S n HAlign → 𝕍S m VAlign → 𝕍S m (𝕍S n Doc) → Doc
ppMatrixCells has vas dss =
  let sss        = mapp (execRenderUT ∘ summaryIContents ∘ staticDocA ∘ execDoc) dss
      ws :* sss' = matrixHelper has vas sss
      sep        = ppFG white $ concat $ inbetween (ppString "─┼─") $ mapOn ws $ \ w → ppString $ string $ replicate w '─'
      dss'       = svecF 𝕟64s $ \ i → svecF 𝕟64s $ \ j →
        let SummaryO sh t = sss' ⋕ i ⋕ j
        in Doc $ tell $ StaticDocA $ SummaryI True (ShapeA False sh) $ treeIO t
  in 
  ppVertical $ inbetween sep $ mapOn dss' $ \ ds →
    ppHorizontal $ inbetween (ppFG white $ ppString "│") ds

-----------
-- CLASS --
-----------

class Pretty a where 
  pretty ∷ a → Doc

class PrettyM m a | a → m where
  mpretty ∷ a → m Doc

instance Pretty Doc where pretty = id
instance Pretty () where pretty = ppCon ∘ show𝕊
instance Pretty 𝔹 where pretty = ppCon ∘ show𝕊
instance Pretty ℕ where pretty = ppLit ∘ show𝕊
instance Pretty ℕ64 where pretty = ppLit ∘ show𝕊
instance Pretty ℕ32 where pretty = ppLit ∘ show𝕊
instance Pretty ℕ16 where pretty = ppLit ∘ show𝕊
instance Pretty ℕ8 where pretty = ppLit ∘ show𝕊
instance Pretty ℤ where pretty = ppLit ∘ show𝕊
instance Pretty ℤ64 where pretty = ppLit ∘ show𝕊
instance Pretty ℤ32 where pretty = ppLit ∘ show𝕊
instance Pretty ℤ16 where pretty = ppLit ∘ show𝕊
instance Pretty ℤ8 where pretty = ppLit ∘ show𝕊
instance Pretty ℚ where pretty = ppLit ∘ show𝕊
instance Pretty ℚᴾ where pretty = ppLit ∘ show𝕊
instance Pretty 𝔻  where pretty = ppLit ∘ show𝕊
instance Pretty 𝔻ᴾ  where pretty (𝔻ᴾ d) = ppLit $ show𝕊 d
instance Pretty ℝ  where 
  pretty = \case 
    Integer i → pretty i 
    Rational q → pretty q 
    Double d → pretty d
instance Pretty ℝᴾ  where 
  pretty = \case 
    Natural n → pretty n 
    Rationalᴾ q → pretty q 
    Doubleᴾ d → pretty d

instance Pretty Time where pretty = ppLit ∘ show𝕊

escape ∷ ℂ → 𝐼 ℂ
escape = \case
  '"' → iter $ 𝕤 "\\\""
  '\\' → iter $ 𝕤 "\\\\"
  '\n' → iter $ 𝕤 "\\n"
  '\t' → iter $ 𝕤 "\\t"
  '\r' → iter $ 𝕤 "\\r"
  '\b' → iter $ 𝕤 "\\b"
  '\f' → iter $ 𝕤 "\\f"
  c' → single c'

instance Pretty ℂ where 
  pretty c = ppLit $ string $ concat
    [ iter $ 𝕤 "'"
    , escape c
    , iter $ 𝕤 "'"
    ]

instance Pretty 𝕊 where 
  pretty s = ppLit $ string $ concat
    [ iter $ 𝕤 "\""
    , escape *$ iter s
    , iter $ 𝕤 "\""
    ]

instance (Pretty a,Pretty b) ⇒ Pretty (a,b) where
  pretty (a,b) = ppCollection (ppPun "(") (ppPun ")") (ppPun ",") [pretty a, pretty b]
instance (Pretty a,Pretty b) ⇒ Pretty (a ∧ b) where
  pretty (a :* b) = ppCollection (ppPun "⟨") (ppPun "⟩") (ppPun ",") [pretty a, pretty b]

instance (Pretty a) ⇒ Pretty (𝐿 a) where 
  pretty = ppCollection (ppPun "[") (ppPun "]") (ppPun ",") ∘ map pretty ∘ iter
instance (Pretty a) ⇒ Pretty [a] where 
  pretty = ppCollection (ppPun "[") (ppPun "]") (ppPun ",") ∘ map pretty ∘ iter
instance (Pretty a) ⇒ Pretty (𝐼 a) where 
  pretty xs = ppApp (ppString "𝐼") $ list [pretty $ list xs]
instance (Pretty a) ⇒ Pretty (𝐼C a) where 
  pretty xs = ppApp (ppString "𝐼C") $ list [pretty $ list xs]
instance (Pretty a) ⇒ Pretty (𝑄 a) where 
  pretty xs = ppApp (ppString "𝑄") $ list [pretty $ list xs]
instance (Pretty a) ⇒ Pretty (𝑃 a) where 
  pretty = ppCollection (ppPun "{") (ppPun "}") (ppPun ",") ∘ map pretty ∘ iter
instance (Pretty k,Pretty v) ⇒ Pretty (k ⇰ v) where 
  pretty = ppRecord (ppPun "↦") ∘ map (mapPair pretty pretty) ∘ iter
instance (Pretty a) ⇒ Pretty (𝕍 a) where 
  pretty xs = ppApp (ppString "𝕍") $ list [pretty $ list xs]
instance (Pretty a) ⇒ Pretty (𝕍S n a) where 
  pretty xs = ppApp (ppString "𝕍S") $ list [pretty $ list xs]
instance (Storable a,Pretty a) ⇒ Pretty (𝕌 a) where 
  pretty xs = ppApp (ppString "𝕌") $ list [pretty $ list xs]
instance (Storable a,Pretty a) ⇒ Pretty (𝕌S n a) where 
  pretty xs = ppApp (ppString "𝕌S") $ list [pretty $ list xs]
-- instance (Element a,Pretty a) ⇒ Pretty (𝕄S m n a) where 
--   pretty xs = ppApp (ppString "𝕄S") $ list [pretty $ list xs]

instance (Pretty a) ⇒ Pretty (AddNull a) where
  pretty Null = ppCon "•"
  pretty (AddNull x) = pretty x

instance (Pretty a) ⇒ Pretty (ZOM a) where
  pretty NullZOM = ppCon "⊥"
  pretty (OneZOM x) = pretty x
  pretty MoreZOM = ppCon "⊤"

instance (Pretty a) ⇒ Pretty (AddBot a) where
  pretty Bot = ppCon "⊥"
  pretty (AddBot x) = pretty x

instance (Pretty a) ⇒ Pretty (AddTop a) where
  pretty Top = ppCon "⊤"
  pretty (AddTop x) = pretty x

instance (Pretty a) ⇒ Pretty (AddBT a) where
  pretty BotBT = ppCon "⊥"
  pretty TopBT = ppCon "⊤"
  pretty (AddBT x) = pretty x

instance Pretty Stack.CallStack where pretty = ppString ∘ string ∘ Stack.prettyCallStack

colorsDemo ∷ Doc
colorsDemo = 
  d𝕍 (vec $ iter allColors) HS.$ \ allColorsS → 
    ppMatrix (const𝕍S 𝕟64s LH) (const𝕍S 𝕟64s TV) $ mapOn allColorsS $ \ (n :* c) → 
      svec $ 𝔢 (ppString n)
          ⧺♮ 𝔢 (ppFG c $ ppString "XXXXX")
          ⧺♮ 𝔢 (ppBG c $ ppString "XXXXX")
          ⧺♮ 𝔢 (ppBG black $ ppFG c $ ppString "XXXXX")
          ⧺♮ 𝔢 (ppFG white $ ppBG c $ ppString "XXXXX")
