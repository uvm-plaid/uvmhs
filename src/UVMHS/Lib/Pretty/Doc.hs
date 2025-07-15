module UVMHS.Lib.Pretty.Doc where

import UVMHS.Core

import UVMHS.Lib.Pretty.Annotation
import UVMHS.Lib.Pretty.Color
import UVMHS.Lib.Pretty.Common
import UVMHS.Lib.Pretty.DocA
import UVMHS.Lib.Pretty.Shape
import UVMHS.Lib.Pretty.RenderUndertags
import UVMHS.Lib.TreeAnnote

import qualified GHC.Stack as Stack
import qualified Prelude as HS

-- Doc renders local configuration options such as colors and
-- formatting

data PrettyLayoutMode = Exp_PPLM | Cmd_PPLM
  deriving (Eq,Ord,Show)

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
  , annotationFormat         ∷ Formats
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
  , annotationFormat         = formats [BG grayLight]
  , appLevel                 = 100
  }

data DocEnv = DocEnv
  -- global env
  { docEnvPrettyParams ∷ PrettyParams
  -- local env
  , docEnvPrecLevel ∷ AddTop ℕ64
  , docEnvPrecBumped ∷ 𝔹
  , docEnvLayoutMode ∷ PrettyLayoutMode
  } deriving (Eq,Ord,Show)
makeLenses ''DocEnv

docEnv₀ ∷ DocEnv
docEnv₀ = DocEnv
  -- global env
  { docEnvPrettyParams = prettyParams₀
  -- local env
  , docEnvPrecLevel = AddTop 0
  , docEnvPrecBumped = False
  , docEnvLayoutMode = Exp_PPLM
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

-- RAW STRINGS

ppString ∷ 𝕊 → Doc
ppString = Doc ∘ tell ∘ stringDocA

-- MODE: GROUPING / ALIGNMENT --

ppG ∷ Doc → Doc
ppG = onDoc $ mapOut groupDocA

ppA ∷ Doc → Doc
ppA = onDoc $ mapOut alignDocA

ppGA ∷ Doc → Doc
ppGA = ppA ∘ ppG

ppWhenFlat ∷ Doc → Doc
ppWhenFlat d = Doc $ do
  da ← retOut $ unDoc d
  tell $ docAModal da null

ppWhenBreak ∷ Doc → Doc
ppWhenBreak d = Doc $ do
  da ← retOut $ unDoc d
  tell $ docAModal null da

ppGModal ∷ Doc → Doc → Doc
ppGModal d₁ d₂ = Doc $ do
  da₁ ← retOut $ unDoc d₁
  da₂ ← retOut $ unDoc d₂
  tell $ docAModal da₁ da₂

ppForceBreak ∷ Doc
ppForceBreak = Doc $ tell $ StaticDocA $ SummaryI True null null

ppStringGModal ∷ 𝕊 → 𝕊 → Doc
ppStringGModal x y = ppGModal (ppString x) $ ppString y

-- MODE: EXP / CMD --

ppE ∷ Doc → Doc
ppE = onDoc $ localL docEnvLayoutModeL Exp_PPLM

ppEG ∷ Doc → Doc
ppEG = ppE ∘ ppG

ppEA ∷ Doc → Doc
ppEA = ppE ∘ ppA

ppEGA ∷ Doc → Doc
ppEGA = ppE ∘ ppGA

ppC ∷ Doc → Doc
ppC = onDoc $ localL docEnvLayoutModeL Cmd_PPLM

ppCG ∷ Doc → Doc
ppCG = ppC ∘ ppG

ppCA ∷ Doc → Doc
ppCA = ppC ∘ ppA

ppCGA ∷ Doc → Doc
ppCGA = ppC ∘ ppGA

ppWhenExp ∷ Doc → Doc
ppWhenExp d = Doc $ do
  m ← askL docEnvLayoutModeL
  case m of
    Exp_PPLM → unDoc d
    Cmd_PPLM → null

ppWhenCmd ∷ Doc → Doc
ppWhenCmd d = Doc $ do
  m ← askL docEnvLayoutModeL
  case m of
    Exp_PPLM → null
    Cmd_PPLM → unDoc d

ppWhenBreakCmd ∷ Doc → Doc
ppWhenBreakCmd = ppWhenBreak ∘ ppWhenCmd

ppLModal ∷ Doc → Doc → Doc
ppLModal eD cD = Doc $ do
  m ← askL docEnvLayoutModeL
  case m of
    Exp_PPLM → unDoc eD
    Cmd_PPLM → unDoc cD

ppModal ∷ Doc → Doc → Doc → Doc
ppModal flat whenBreakExp whenBreakCmd = ppGModal flat $ ppLModal whenBreakExp whenBreakCmd

-- ANNOTATIONS --

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

-- COLOR: LOWER LEVEL --

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

-- COLOR: SYNTACTIC CLASSES --

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

ppAnnotation ∷ Doc → Doc
ppAnnotation = ppFormatParam annotationFormatL

-- SPACING --

ppSpaces ∷ ℕ64 → Doc
ppSpaces n = ppString $ string $ replicate n ' '

ppSpace ∷ Doc
ppSpace = ppSpaces 1

ppNewlines ∷ ℕ64 → Doc
ppNewlines n = ppString $ string $ replicate n '\n'

ppNewline ∷ Doc
ppNewline = ppNewlines 1

ppSpacesIfFlat ∷ ℕ64 → Doc
ppSpacesIfFlat n = ppWhenFlat $ ppSpaces n

ppSpaceIfFlat ∷ Doc
ppSpaceIfFlat = ppSpacesIfFlat 1

ppSpacesIfBreak ∷ ℕ64 → Doc
ppSpacesIfBreak n = ppWhenBreak $ ppSpaces n

ppSpaceIfBreak ∷ Doc
ppSpaceIfBreak = ppSpacesIfBreak 1

ppSpacesIfBreakCmd ∷ ℕ64 → Doc
ppSpacesIfBreakCmd n = ppWhenBreakCmd $ ppSpaces n

ppSpaceIfBreakCmd ∷ Doc
ppSpaceIfBreakCmd = ppSpacesIfBreakCmd 1

ppNewlineIfFlat ∷ Doc
ppNewlineIfFlat = ppWhenFlat ppNewline

ppNewlineIfBreak ∷ Doc
ppNewlineIfBreak = ppWhenBreak ppNewline

ppNewlineIfBreakCmd ∷ Doc
ppNewlineIfBreakCmd = ppWhenBreakCmd ppNewline

ppSpacesNewlineIfBreak ∷ ℕ64 → Doc
ppSpacesNewlineIfBreak n = ppGModal (ppSpaces n) ppNewline

ppSpaceNewlineIfBreak ∷ Doc
ppSpaceNewlineIfBreak = ppSpacesNewlineIfBreak 1

ppHorizontal ∷ (ToIter Doc t) ⇒ t → Doc
ppHorizontal = concat ∘ inbetween ppSpace ∘ iter

ppVertical ∷ (ToIter Doc t) ⇒ t → Doc
ppVertical = concat ∘ inbetween ppNewline ∘ iter

ppSeparated ∷ (ToIter Doc t) ⇒ t → Doc
ppSeparated = ppG ∘ concat ∘ inbetween ppSpaceNewlineIfBreak ∘ iter

-- MANUALLY SETTING LEVELS --

ppSetLevel ∷ ℕ64 → Doc → Doc
ppSetLevel n = onDoc $ mapEnv $ update docEnvPrecLevelL (AddTop n) ∘ update docEnvPrecBumpedL False

ppSetTopLevel ∷ Doc → Doc
ppSetTopLevel = onDoc $ mapEnv $ update docEnvPrecLevelL Top ∘ update docEnvPrecBumpedL True

ppSetBotLevel ∷ Doc → Doc
ppSetBotLevel = ppSetLevel zero

ppBump ∷ Doc → Doc
ppBump = onDoc $ mapEnv $ update docEnvPrecBumpedL True

-- BRACKETS --

ppBrackets ∷ Doc → Doc → Doc → Doc
ppBrackets lD rD xD = ppG $ concat
  [ ppSetTopLevel $ ppA lD
  , ppSetBotLevel $ ppEA xD
  , ppSetTopLevel $ ppEA rD
  ]

ppParens ∷ Doc → Doc
ppParens = ppBrackets (ppPun "(") $ ppPun ")"

-- FLAT
--
--     [f x,g y]
--
-- BREAK EXP
--
--     [ f x
--     , g y
--     ]
--
-- BREAK CMD
--
--     [ f x ,
--       g y ]
--
ppCollectionF ∷ Doc → Doc → Doc → (𝔹 → 𝑂 (Doc ∧ Doc) → 𝐼 Doc) → Doc
ppCollectionF l r i xs = 
  if 
    and
      [ isEmpty $ xs False None
      , isEmpty $ xs True None
      ]
  then 
    concat 
      [ ppSetTopLevel $ ppA l
      , ppSetTopLevel $ ppEA r
      ]
  else
    let lWidth = shapeAWidth $ docShape l
        iWidth = shapeAWidth $ docShape i
        tWidth = lWidth ⊔ iWidth
        lExtra = tWidth - lWidth
        iExtra = tWidth - iWidth
        flat = concat
          [ ppSetTopLevel $ ppA l
          , concat $ inbetween (ppSetTopLevel $ ppA i) $ map (ppSetBotLevel ∘ ppEA) $ iter $ xs False None
          , ppSetTopLevel $ ppEA r
          ]
        breakExp = concat
          [ ppSetTopLevel $ ppA l
          , ppSpaces lExtra
          , ppSpace
          , concat $ inbetween (concat [ppNewline,ppSpaces iExtra,ppSetTopLevel $ ppEA i,ppSpace]) $ 
              map (ppSetBotLevel ∘ ppEGA) $ iter $ xs True None
          , ppNewline
          , ppSetTopLevel $ ppEA r
          ]
        breakCmd = concat
          [ ppSetTopLevel $ ppA l
          , ppSpace
          , ppEA $ concat $ inbetween ppNewline $
              map (ppSetBotLevel ∘ ppEGA) $
                iter $ xs True $ Some $ 
                  concat [ppSpace,ppSetTopLevel $ ppEA i] 
                  :* concat [ppSpace,ppSetTopLevel $ ppEA r]
          ]
    in 
    ppGA $ ppModal flat breakExp breakCmd

ppCollection ∷ (ToIter Doc t) ⇒ Doc → Doc → Doc → t → Doc
ppCollection l r i xs = ppCollectionF l r i $ const $ \case
  None → iter xs
  Some (iᵢ :* rᵢ) → mapBeforeLastLast (\ x → concat [x,iᵢ]) (\ x → concat [x,rᵢ]) $ iter xs


-- FLAT
--
--     [a b=f x,c d=g y]
--
-- BREAK EXP
--
--     [ a b = f x
--     , c d = g y
--     ]
--
--     [ a 
--       b = f x
--     , c 
--       d = g y
--     ]
--
--     [ a 
--       b = 
--         f x
--     , c 
--       d = 
--         g y
--     ]
--
--     [ a 
--       b = 
--         f 
--         x
--     , c 
--       d = 
--         g 
--         y
--     ]
--
-- BREAK CMD
--
--     [ a b = f x ,
--       c d = g y ]
--
--     [ a 
--       b = f x ,
--       c 
--       d = g y ]
--
--     [ a 
--       b = 
--         f x ,
--       c 
--       d = 
--         g y ]
--
--     [ a 
--       b = 
--         f 
--         x ,
--       c 
--       d = 
--         g 
--         y ]
--
ppCollectionRec ∷ (ToIter (Doc ∧ Doc) t) ⇒ Doc → Doc → Doc → Doc → t → Doc
ppCollectionRec l r i rel kvs = ppCollectionF l r i $ \ isBreak irO → 
  let kvs' = case irO of
        None → iter kvs
        Some (iᵢ :* rᵢ) → mapBeforeLastLast (\ (k :* v) → k:*concat [v,iᵢ]) (\ (k :* v) → k:*concat [v,rᵢ]) $ iter kvs
  in
  mapOn (iter kvs') $ \ (k :* v) → 
  let flat = concat 
        [ ppSetBotLevel $ ppA k
        , if isBreak then ppSpace else null
        , ppSetTopLevel $ ppEA rel
        , if isBreak then ppSpace else null
        , ppSetBotLevel $ ppEA v
        ]
      whenBreak = concat
        [ ppA k
        , ppSpace
        , ppG $ concat
          [ ppSetTopLevel $ ppEA rel
          , ppSpaceNewlineIfBreak
          , ppSpacesIfBreak 2
          , ppEGA v
          ]
        ]
  in 
  ppGModal flat whenBreak

ppRecord ∷ (ToIter (Doc ∧ Doc) t) ⇒ Doc → t → Doc
ppRecord = ppCollectionRec (ppPun "{") (ppPun "}") $ ppPun ","

-- CHANGING LEVELS BY MAYBE WRAPPING WITH PARENS --

ppLevel ∷ ℕ64 → Doc → Doc
ppLevel i' aM = Doc $ do
  i ← askL $ docEnvPrecLevelL
  b ← askL $ docEnvPrecBumpedL
  unDoc $ case (i < AddTop i') ⩔ ((i ≡ AddTop i') ⩓ not b) of
    True → ppSetLevel i' aM
    False → ppParens $ ppSetLevel i' aM

ppBotLevel ∷ Doc → Doc
ppBotLevel = ppLevel 0

-- INFIX/PREFIX/POSTFIX --

-- FLAT
--
--     f x : g y
--
-- BREAK EXP
--
--     f x
--     : g y
--     
--     f x
--     :
--     g y
--
--     f
--     x
--     :
--     g
--     y
--
-- BREAK CMD
--
--     f x
--       : g y
--
--     f x
--       :
--       g y
--
--     f x
--       :
--       g
--       y
--
--     fff x
--       : g y
--
--     fff x
--       :
--       g y
--
--     fff 
--       x
--       :
--       g
--       y
--
ppInfG ∷ ℕ64 → Doc → Doc → Doc → Doc
ppInfG i o e₁ e₂ = 
  let eWidth = shapeAWidth $ docShape e₁
      eWidthSmall = eWidth ≡ 1
  in
  ppLevel i $ concat
    [ ppA e₁
    , ppModal
        ppSpace
        ppNewline $
          if eWidthSmall 
          then ppSpace 
          else concat [ppNewline,ppSpaces 2]
    , ppSetTopLevel $ ppEA o 
    , ppG $ concat
        [ ppSpaceNewlineIfBreak
        , ppSpacesIfBreakCmd 2
        , ppBump $ ppEA e₂
        ]
    ]

-- FLAT
--
--     f x:g y
--
-- BREAK EXP
--
--     f x
--     :g y
--     
--     f x
--     :
--     g y
--
--     f
--     x
--     :
--     g
--     y
--
-- BREAK CMD
--
--     f x
--       :g y
--
--     f x
--       :
--       g y
--
--     f x
--       :
--       g
--       y
--
--     fff x
--       :g y
--
--     fff x
--       :
--       g y
--
--     fff
--       x
--       :
--       g
--       y
--
ppInfGTight ∷ ℕ64 → Doc → Doc → Doc → Doc
ppInfGTight i o e₁ e₂ = 
  let eWidth = shapeAWidth $ docShape e₁
      eWidthSmall = eWidth ≡ 1
  in
  ppLevel i $ concat
    [ ppA e₁
    , ppModal
        null
        ppNewline $
          if eWidthSmall 
          then ppSpace 
          else concat [ppNewline,ppSpaces 2]
    , ppSetTopLevel $ ppEA o 
    , ppG $ concat
        [ ppNewlineIfBreak
        , ppSpacesIfBreakCmd 2
        , ppEA e₂
        ]
    ]

ppInfGSpace ∷ ℕ64 → Doc → Doc → Doc
ppInfGSpace i e₁ e₂ = 
  let eWidth = shapeAWidth $ docShape e₁
      eWidthSmall = eWidth ≡ 1
  in
  ppLevel i $ concat
    [ ppA e₁
    , ppModal
        ppSpace
        ppNewline $
          if eWidthSmall 
          then ppSpace 
          else concat [ppNewline,ppSpaces 2]
    , ppA e₂
    ]

ppInf ∷ ℕ64 → Doc → Doc → Doc → Doc
ppInf i o e₁ e₂ = ppInfG i o (ppBump $ ppG e₁) $ ppBump $ ppG e₂

ppInfTight ∷ ℕ64 → Doc → Doc → Doc → Doc
ppInfTight i o e₁ e₂ = ppInfGTight i o (ppBump $ ppG e₁) $ ppBump $ ppG e₂

ppInfSpace ∷ ℕ64 → Doc → Doc → Doc
ppInfSpace i e₁ e₂ = ppInfGSpace i (ppBump $ ppG e₁) $ ppBump $ ppG e₂

ppInfl ∷ ℕ64 → Doc → Doc → Doc → Doc
ppInfl i o e₁ e₂ = ppInfG i o e₁ $ ppBump $ ppG e₂

ppInflTight ∷ ℕ64 → Doc → Doc → Doc → Doc
ppInflTight i o e₁ e₂ = ppInfGTight i o e₁ $ ppBump $ ppG e₂

ppInflSpace ∷ ℕ64 → Doc → Doc → Doc
ppInflSpace i e₁ e₂ = ppInfGSpace i e₁ $ ppBump $ ppG e₂

ppInfr ∷ ℕ64 → Doc → Doc → Doc → Doc
ppInfr i o e₁ e₂ = ppInfG i o (ppBump $ ppG e₁) e₂

ppInfrTight ∷ ℕ64 → Doc → Doc → Doc → Doc
ppInfrTight i o e₁ e₂ = ppInfGTight i o (ppBump $ ppG e₁) e₂

ppInfrSpace ∷ ℕ64 → Doc → Doc → Doc
ppInfrSpace i e₁ e₂ = ppInfGSpace i (ppBump $ ppG e₁) e₂

ppAppMLL ∷ ℕ64 → Doc → Doc → Doc
ppAppMLL ℓ = ppInflSpace ℓ

ppAppsMLL ∷ (ToIter Doc t) ⇒ ℕ64 → Doc → t → Doc
ppAppsMLL ℓ x xs = foldOnFrom xs x $ \ xᵢ f → ppAppMLL ℓ f xᵢ

ppAppML ∷ Doc → Doc → Doc
ppAppML x y = Doc $ do
  ℓ ← askL $ appLevelL ⊚ docEnvPrettyParamsL
  unDoc $ ppAppMLL ℓ x y

ppAppsML ∷ (ToIter Doc t) ⇒ Doc → t → Doc
ppAppsML x xs = Doc $ do
  ℓ ← askL $ appLevelL ⊚ docEnvPrettyParamsL
  unDoc $ ppAppsMLL ℓ x xs

-- FLAT
-- 
--     f.a.b(x,g(y),h(z))
--
-- BREAK EXP
--
--     f.a.b( x
--          , g(y)
--          , h(z)
--          )
--
--     f.a.b( x
--          , g( y
--             )
--          , h( z
--             )
--          )
--
--     f.a.b
--       ( x
--       , g( y
--          )
--       , h( z
--          )
--       )
--
--     f
--     .a
--     .b
--       ( x
--       , g( y
--          )
--       , h( z
--          )
--       )
--
--     f
--     .
--     a
--     .
--     b
--       ( x
--       , g( y
--          )
--       , h( z
--          )
--       )
--
ppAppCStyle ∷ (ToIter Doc t) ⇒ ℕ64 → Doc → Doc → Doc → Doc → t → Doc
ppAppCStyle ℓ l r i f xs = 
  let fWidth = shapeAWidth $ docShape f
      fWidthSmall = fWidth ≤ 2
  in
  ppLevel ℓ $ concat
    [ ppGA f
    , ppG $ concat
        [ ppWhenBreak $
            if fWidthSmall 
            then null 
            else concat [ppNewline,ppSpacesIfBreak 2]
        , ppEGA $ ppCollection l r i xs
        ]
    ]

ppAppC ∷ (ToIter Doc t) ⇒ Doc → t → Doc
ppAppC f xs = Doc $ do
  ℓ ← askL $ appLevelL ⊚ docEnvPrettyParamsL
  unDoc $ ppAppCStyle ℓ (ppPun "(") (ppPun ")") (ppPun ",") f xs

ppAppCStyleRec ∷ (ToIter (Doc ∧ Doc) t) ⇒ ℕ64 → Doc → Doc → Doc → Doc → Doc → t → Doc
ppAppCStyleRec ℓ l r i f rel xs = 
  let fWidth = shapeAWidth $ docShape f
      fWidthSmall = fWidth ≤ 2
  in
  ppLevel ℓ $ concat
    [ ppGA f
    , ppG $ concat
        [ ppWhenBreak $
            if fWidthSmall 
            then null 
            else concat [ppNewline,ppSpacesIfBreak 2]
        , ppEGA $ ppCollectionRec l r i rel xs
        ]
    ]
-- FLAT
--
--     ! f x
--
-- BREAK EXP
--
--     !
--     f x
--     
--     !
--     f
--     x
--
-- BREAK CMD
--
--     ! f
--       x
--
--     !!!
--       f x
--
--     !!!
--       f
--       x
--
ppPre ∷ ℕ64 → Doc → Doc → Doc
ppPre i o e = 
  let oWidth = shapeAWidth $ docShape o
      oWidthSmall = oWidth ≡ 1
  in
  ppLevel i $ concat
    [ ppSetTopLevel $ ppA o
    , ppModal
        ppSpace
        ppNewline $
          if oWidthSmall 
          then ppSpace 
          else concat [ppNewline,ppSpaces 2]
    , ppEA e
    ]

-- FLAT
--
--     !f x
--
-- BREAK EXP
--
--     !
--     f x
--     
--     !
--     f
--     x
--
-- BREAK CMD
--
--     ! f
--       x
--
--     !!!
--       f x
--
--     !!!
--       f
--       x
--
ppPreTight ∷ ℕ64 → Doc → Doc → Doc
ppPreTight i o e = 
  let oWidth = shapeAWidth $ docShape o
      oWidthSmall = oWidth ≡ 1
  in
  ppLevel i $ concat
    [ ppSetTopLevel $ ppA o
    , ppModal
        null
        ppNewline $
          if oWidthSmall 
          then ppSpace 
          else concat [ppNewline,ppSpaces 2]
    , ppEA e
    ]

-- FLAT
--
--     f x ?
--
-- BREAK EXP
--
--     f x
--     ?
--     
--     f
--     x
--     ?
--
-- BREAK CMD
--
--     f x
--       ?
--
--     f x
--       ?
--
ppPost ∷ ℕ64 → Doc → Doc → Doc
ppPost i o e = 
  let eWidth = shapeAWidth $ docShape e
      eWidthSmall = eWidth ≡ 1
  in
  ppLevel i $ concat
    [ ppA e
    , ppModal
        ppSpace
        ppNewline $
          if eWidthSmall 
          then ppSpace 
          else concat [ppNewline,ppSpaces 2]
    , ppSetTopLevel $ ppEA o
    ]

-- FLAT
--
--     f x ?
--
-- BREAK EXP
--
--     f x
--     ?
--     
--     f
--     x
--     ?
--
-- BREAK CMD
--
--     f x
--       ?
--
--     f
--       x
--       ?
--
ppPostTight ∷ ℕ64 → Doc → Doc → Doc
ppPostTight i o e = 
  let eWidth = shapeAWidth $ docShape e
      eWidthSmall = eWidth ≡ 1
  in
  ppLevel i $ concat
    [ ppA e
    , ppModal
        null
        ppNewline $
          if eWidthSmall 
          then ppSpace 
          else concat [ppNewline,ppSpaces 2]
    , ppSetTopLevel $ ppEA o
    ]

-- BLOCK --

-- FLAT
--
--     do
--       f x y
--       g z
--
--     do
--       f x 
--         y
--       g z
--
ppBlock ∷ (ToIter Doc t) ⇒ Doc → t → Doc
ppBlock b xs = 
  if isEmpty xs
  then 
    ppBotLevel $ ppA b
  else
    ppBotLevel $ concat
      [ ppForceBreak
      , ppSetTopLevel $ ppA b
      , ppNewline
      , ppSpaces 2
      , ppA $ ppVertical $ map (ppSetBotLevel ∘ ppCG) $ iter xs
      ]

-- STYLES --

ppCxt ∷ 𝕊 → Doc → Doc
ppCxt k v = ppHorizontal
  [ ppFG teal $ ppBD $ ppString k
  , ppEGA v
  ]

-- BAKING --

ppBake ∷ Doc → TreeI
ppBake = execDocA ∘ execDoc

ppEmbed ∷ TreeI → Doc
ppEmbed is =
  let s = fold𝑇VOn is summaryChunksI annotateSummaryI
  in Doc $ tell $ StaticDocA s

-- TABLES --

ppTableHelper ∷ (𝒩 m,𝒩 n) ⇒ 𝕍S n HAlign → 𝕍S m VAlign → 𝕍S m (𝕍S n SummaryO) → 𝕍S n ℕ64 ∧ 𝕍S m (𝕍S n SummaryO)
ppTableHelper has vas sss =
  let sssT       = 𝐭 sss
      rowHeights = mapOn sss  $ \ ss → joins $ mapOn ss $ \ s → shapeNewlines $ summaryOShape s
      colWidths  = mapOn sssT $ \ ss → joins $ mapOn ss $ \ s → shapeWidth    $ summaryOShape s
      sss'       = svecF 𝕟64s $ \ i → svecF 𝕟64s $ \ j → hvalign (has ⋕ j) (vas ⋕ i) (colWidths ⋕ j) (rowHeights ⋕ i) $ sss ⋕ i ⋕ j
  in colWidths :* sss'

ppTable ∷ (𝒩 m,𝒩 n) ⇒ 𝕍S n HAlign → 𝕍S m VAlign → 𝕍S m (𝕍S n Doc) → Doc
ppTable has vas dss =
  let sss       = mapp (execRenderUT ∘ summaryIContents ∘ staticDocA ∘ execDoc) dss
      _ :* sss' = ppTableHelper has vas sss
      dss'      = svecF 𝕟64s $ \ i → svecF 𝕟64s $ \ j →
        let SummaryO sh t = sss' ⋕ i ⋕ j
        in Doc $ tell $ StaticDocA $ SummaryI True (shapeToShapeA sh) $ treeIO t
  in
  ppVertical $ mapOn dss' $ \ ds →
    ppHorizontal $ inbetween null ds

ppTableCells ∷ (𝒩 m,𝒩 n) ⇒ 𝕍S n HAlign → 𝕍S m VAlign → 𝕍S m (𝕍S n Doc) → Doc
ppTableCells has vas dss =
  let sss        = mapp (execRenderUT ∘ summaryIContents ∘ staticDocA ∘ execDoc) dss
      ws :* sss' = ppTableHelper has vas sss
      sep        = ppFG white $ concat $ inbetween (ppString "─┼─") $ mapOn ws $ \ w → ppString $ string $ replicate w '─'
      dss'       = svecF 𝕟64s $ \ i → svecF 𝕟64s $ \ j →
        let SummaryO sh t = sss' ⋕ i ⋕ j
        in Doc $ tell $ StaticDocA $ SummaryI True (shapeToShapeA sh) $ treeIO t
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
instance Pretty Void where pretty = \case
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
instance Pretty TimeD where pretty = ppLit ∘ show𝕊

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

ppTupHS2 ∷ Doc → Doc → Doc
ppTupHS2 x y = ppCollection (ppPun "(") (ppPun ")") (ppPun ",") [x,y]

ppTup ∷ Doc → Doc → Doc
ppTup x y = ppCollection (ppPun "⟨") (ppPun "⟩") (ppPun ",") [x,y]

ppList ∷ (ToIter Doc t) ⇒ t → Doc
ppList = ppCollection (ppPun "[") (ppPun "]") (ppPun ",")

ppLazyList ∷ (ToIter Doc t) ⇒ t → Doc
ppLazyList xs = concat [ppCon "LL",ppList xs]

ppIter ∷ (ToIter Doc t) ⇒ t → Doc
ppIter xs = concat [ppCon "𝐼",ppList xs]

ppIterC ∷ (ToIter Doc t) ⇒ t → Doc
ppIterC xs = concat [ppCon "𝐼C",ppList xs]

ppStream ∷ (ToIter Doc t) ⇒ t → Doc
ppStream xs = concat [ppCon "𝑆",ppList xs]

ppSeq ∷ (ToIter Doc t) ⇒ t → Doc
ppSeq xs = concat [ppCon "𝑄",ppList xs]

ppSet ∷ (ToIter Doc t) ⇒ t → Doc
ppSet = ppCollection (ppPun "{") (ppPun "}") (ppPun ",")

ppDict ∷ (ToIter (Doc ∧ Doc) t) ⇒ t → Doc
ppDict = ppRecord (ppPun "↦") ∘ iter

ppVec ∷ (ToIter Doc t) ⇒ t → Doc
ppVec xs = concat [ppCon "𝕍",ppList xs]

ppVecS ∷ (ToIter Doc t) ⇒ t → Doc
ppVecS xs = concat [ppCon "𝕍S",ppList xs]

ppUVec ∷ (ToIter Doc t) ⇒ t → Doc
ppUVec xs = concat [ppCon "𝕌",ppList xs]

ppUVecS ∷ (ToIter Doc t) ⇒ t → Doc
ppUVecS xs = concat [ppCon "𝕌S",ppList xs]

ppAddNull ∷ AddNull Doc → Doc
ppAddNull = \case
  Null → ppCon "•"
  AddNull x → x

ppAddBot ∷ AddBot Doc → Doc
ppAddBot = \case
  Bot → ppCon "⊥"
  AddBot x → x

ppAddTop ∷ AddTop Doc → Doc
ppAddTop = \case
  Top → ppCon "⊤"
  AddTop x → x

ppAddBT ∷ AddBT Doc → Doc
ppAddBT = \case
  BotBT → ppCon "⊥"
  TopBT → ppCon "⊤"
  AddBT x → x

ppZOM ∷ ZOM Doc → Doc
ppZOM = \case
  NullZOM → ppCon "⊥"
  OneZOM x → x
  MoreZOM → ppCon "⊤"

instance (Pretty a,Pretty b)   ⇒ Pretty (a , b)     where pretty = uncurry ppTupHS2 ∘ mapPair pretty pretty ∘ frhs
instance (Pretty a,Pretty b)   ⇒ Pretty (a ∧ b)     where pretty = uncurry ppTup ∘ mapPair pretty pretty
instance (Pretty a)            ⇒ Pretty (() → a)    where pretty = pretty ∘ appto ()
instance (Pretty a)            ⇒ Pretty (𝐿 a)       where pretty = ppList ∘ map pretty ∘ iter
instance (Pretty a)            ⇒ Pretty [a]         where pretty = ppLazyList ∘ map pretty ∘ iter
instance (Pretty a)            ⇒ Pretty (𝐼 a)       where pretty = ppIter ∘ map pretty
instance (Pretty a)            ⇒ Pretty (𝐼C a)      where pretty = ppIterC ∘ map pretty ∘ iter
instance (Pretty a)            ⇒ Pretty (𝑆 a)       where pretty = ppStream ∘ map pretty
instance (Pretty a)            ⇒ Pretty (𝑄 a)       where pretty = ppSeq ∘ map pretty ∘ iter
instance (Pretty a)            ⇒ Pretty (𝑃 a)       where pretty = ppSet ∘ map pretty ∘ iter
instance (Pretty k,Pretty v)   ⇒ Pretty (k ⇰ v)     where pretty = ppDict ∘ map (mapPair pretty pretty) ∘ iter
instance (Pretty a)            ⇒ Pretty (𝕍 a)       where pretty = ppVec ∘ map pretty ∘ iter
instance (Pretty a)            ⇒ Pretty (𝕍S n a)    where pretty = ppVecS ∘ map pretty ∘ iter
instance (Storable a,Pretty a) ⇒ Pretty (𝕌 a)       where pretty = ppUVec ∘ map pretty ∘ iter
instance (Storable a,Pretty a) ⇒ Pretty (𝕌S n a)    where pretty = ppUVecS ∘ map pretty ∘ iter
instance (Pretty a)            ⇒ Pretty (AddNull a) where pretty = ppAddNull ∘ map pretty
instance (Pretty a)            ⇒ Pretty (AddBot a)  where pretty = ppAddBot ∘ map pretty
instance (Pretty a)            ⇒ Pretty (AddTop a)  where pretty = ppAddTop ∘ map pretty
instance (Pretty a)            ⇒ Pretty (AddBT a)   where pretty = ppAddBT ∘ map pretty
instance (Pretty a)            ⇒ Pretty (ZOM a)     where pretty = ppZOM ∘ map pretty

instance Pretty Stack.CallStack where pretty = ppString ∘ string ∘ Stack.prettyCallStack

-- instance (Element a,Pretty a) ⇒ Pretty (𝕄S m n a) where
--   pretty xs = ppApp (ppString "𝕄S") $ list [pretty $ list xs]

colorsDemo ∷ Doc
colorsDemo =
  d𝕍 (vec $ iter allColors) HS.$ \ allColorsS →
    ppTable (const𝕍S 𝕟64s LH) (const𝕍S 𝕟64s TV) $ mapOn allColorsS $ \ (n :* c) →
      svec $ 𝔢 (ppString n)
          ⧺♮ 𝔢 (ppFG c $ ppString "XXXXX")
          ⧺♮ 𝔢 (ppBG c $ ppString "XXXXX")
          ⧺♮ 𝔢 (ppBG black $ ppFG c $ ppString "XXXXX")
          ⧺♮ 𝔢 (ppFG white $ ppBG c $ ppString "XXXXX")
