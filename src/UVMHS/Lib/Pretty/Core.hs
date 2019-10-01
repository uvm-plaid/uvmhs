module UVMHS.Lib.Pretty.Core where

import UVMHS.Core

import UVMHS.Lib.ATree
import UVMHS.Lib.IterS

import UVMHS.Lib.Pretty.Annotation

-----------
-- CHUNK --
-----------
  
data IChunk =
    RawIChunk ℕ64 𝕊
  | NewlineIChunk ℕ64
  deriving (Eq,Ord,Show)

data OChunk =
    RawOChunk ℕ64 𝕊
  | NewlineOChunk ℕ64
  | PaddingOChunk ℕ64
  deriving (Eq,Ord,Show)

----------------
-- PrettyMode --
----------------

data PrettyMode = NullMode | AMode | GMode | AGMode
  deriving (Eq,Ord,Show)

instance Null PrettyMode where null = NullMode
instance Append PrettyMode where
  NullMode ⧺ m = m
  m ⧺ NullMode = m
  AGMode ⧺ _ = AGMode
  _ ⧺ AGMode = AGMode
  AMode ⧺ AMode = AMode
  GMode ⧺ GMode = GMode
  AMode ⧺ GMode = AGMode
  GMode ⧺ AMode = AGMode
instance Monoid PrettyMode

------------
-- SSHAPE --
------------

data MultiShape = MultiShape
  { multiShapeAligned ∷ 𝔹
  , multiShapeFirstLength ∷ ℕ64
  , multiShapeMidMaxLength ∷ ℕ64
  , multiShapeLastLength ∷ ℕ64
  , multiShapeLines ∷ ℕ64
  } deriving (Eq,Ord,Show)

data Shape = SShape ℕ64 | MShape MultiShape
  deriving (Eq,Ord,Show)
makePrisms ''Shape

alignShape ∷ Shape → Shape
alignShape (SShape l) = SShape l
alignShape (MShape ms) = MShape ms { multiShapeAligned = True }

getShapeAligned ∷ Shape → 𝔹
getShapeAligned (SShape _) = False
getShapeAligned (MShape ms) = multiShapeAligned ms

instance Null Shape where 
  null = SShape $ 𝕟64 0
instance Append Shape where
  SShape l₁ ⧺ SShape l₂ = SShape $ l₁ ⧺ l₂
  SShape l₁ ⧺ MShape (MultiShape a₂ fl₂ mml₂ ll₂ lines₂)
    | not a₂ = MShape $ 
        MultiShape False (l₁ + fl₂) mml₂ ll₂ lines₂
    | otherwise = MShape $ 
        MultiShape True (l₁ + fl₂) (l₁ + mml₂) (l₁ + ll₂) lines₂
  MShape (MultiShape a₁ fl₁ mml₁ ll₁ lines₁) ⧺ SShape l₂ = MShape $ 
    MultiShape a₁ fl₁ mml₁ (ll₁ + l₂) lines₁
  MShape (MultiShape a₁ fl₁ mml₁ ll₁ lines₁) ⧺ MShape (MultiShape a₂ fl₂ mml₂ ll₂ lines₂)
    | not a₂ = MShape $ 
        MultiShape a₁ fl₁ (mml₁ ⊔ (ll₁ + fl₂) ⊔ mml₂) ll₂ (lines₁ + lines₂)
    | otherwise = MShape $ 
        MultiShape a₁ fl₁ (mml₁ ⊔ (ll₁ + fl₂) ⊔ (ll₁ + mml₂)) (ll₁ + ll₂) (lines₁ + lines₂)
instance Monoid Shape

-------------
-- Summary --
-------------

data Summary = Summary
  { summaryShape ∷ Shape
  , summaryContents ∷ 𝐼 (RDoc IChunk)
  } deriving (Show)

instance Null Summary where null = Summary null null
instance Append Summary where
  Summary sh₁ cs₁ ⧺ Summary sh₂ cs₂ = case (sh₁,sh₂) of
    (SShape l₁,SShape l₂) → 
      let sh = SShape $ l₁ ⧺ l₂
      in Summary sh $ cs₁ ⧺ cs₂
    (SShape l₁,MShape (MultiShape a₂ fl₂ mml₂ ll₂ lines₂))
      | not a₂ → 
          let sh = MShape $ MultiShape False (l₁ + fl₂) mml₂ ll₂ lines₂
          in Summary sh $ cs₁ ⧺ cs₂
      | otherwise →
          let sh = MShape $ MultiShape True (l₁ + fl₂) (l₁ + mml₂) (l₁ + ll₂) lines₂
              cs₂' = mapOn cs₂ $ mapp $ \case
                NewlineIChunk n → NewlineIChunk $ n + l₁
                c → c
          in Summary sh $ cs₁ ⧺ cs₂'
    (MShape (MultiShape a₁ fl₁ mml₁ ll₁ lines₁),SShape l₂) →
      let sh = MShape $ MultiShape a₁ fl₁ mml₁ (ll₁ + l₂) lines₁
      in Summary sh $ cs₁ ⧺ cs₂
    (MShape (MultiShape a₁ fl₁ mml₁ ll₁ lines₁),MShape (MultiShape a₂ fl₂ mml₂ ll₂ lines₂))
      | not a₂ → 
          let sh = MShape $ 
                MultiShape a₁ fl₁ (mml₁ ⊔ (ll₁ + fl₂) ⊔ mml₂) ll₂ (lines₁ + lines₂)
          in Summary sh $ cs₁ ⧺ cs₂
      | otherwise → 
          let sh = MShape $ 
                MultiShape a₁ fl₁ (mml₁ ⊔ (ll₁ + fl₂) ⊔ (ll₁ + mml₂)) (ll₁ + ll₂) (lines₁ + lines₂)
              cs₂' = mapOn cs₂ $ mapp $ \case
                NewlineIChunk n → NewlineIChunk $ n + ll₁
                c → c
          in Summary sh $ cs₁ ⧺ cs₂'
instance Monoid Summary

------------------------
-- LDOC + RDoc + SDoc --
------------------------

type LDoc = 𝐴 Summary () PrettyMode (Shape ∧ 𝐼 (RDoc IChunk))
type RDoc a = 𝐴 () Annotation () (𝐼 a)
type SDoc = 𝐴 () Formats () (𝐼 OChunk)

makeLenses ''Summary

ichunkShape ∷ IChunk → Shape
ichunkShape (RawIChunk l _) = SShape l
ichunkShape (NewlineIChunk n) = MShape $ MultiShape False (𝕟64 0) (𝕟64 0) n (𝕟64 1)

rawIChunk𝕊 ∷ 𝕊 → IChunk
rawIChunk𝕊 s = RawIChunk (𝕟64 $ length𝕊 s) s

splitIChunks𝕊 ∷ 𝕊 → 𝐼 IChunk
splitIChunks𝕊 s = iter $ list $ filter (\ s' → s' ≢ RawIChunk (𝕟64 0) "") $ inbetween (NewlineIChunk zero) $ map rawIChunk𝕊 $ iter $ splitOn𝕊 "\n" s

renderIChunks𝕊 ∷ Shape → 𝐼 IChunk → 𝐼 (RDoc IChunk)
renderIChunks𝕊 sh chunks
  | sh ≡ null = null
  | otherwise = single $ Leaf𝐴 () null () chunks

stringCChunk ∷ 𝕊 → LDoc
stringCChunk s =
  let chunks = splitIChunks𝕊 s
      sh = concat $ map ichunkShape chunks
      rd = renderIChunks𝕊 sh chunks
  in Leaf𝐴 (Summary sh rd) () null (sh :* rd)

stringCChunkModal ∷ 𝕊 → 𝕊 → LDoc
stringCChunkModal sf sb =
  let chunksf = splitIChunks𝕊 sf
      chunksb = splitIChunks𝕊 sb
      shf = concat $ map ichunkShape chunksf
      shb = concat $ map ichunkShape chunksb
      rdf = renderIChunks𝕊 shf chunksf
      rdb = renderIChunks𝕊 shb chunksb
  in Leaf𝐴 (Summary shf rdf) () null (shb :* rdb)

-- ######### --
-- COMPILERS --
-- ######### --

-----------------
-- LDOC ⇒ RDOC --
-----------------

data LDocEnv = LDocEnv
  -- global env
  { ldocEnvMaxLineWidth ∷ 𝑂 ℕ64
  , ldocEnvMaxRibbonWidth ∷ 𝑂 ℕ64
  -- local env
  , ldocEnvNest ∷ ℕ64
  } deriving (Eq,Ord,Show)
makeLenses ''LDocEnv

ldocEnv₀ ∷ LDocEnv
ldocEnv₀ = LDocEnv
  { ldocEnvMaxLineWidth = Some $ 𝕟64 120
  , ldocEnvMaxRibbonWidth = Some $ 𝕟64 100
  , ldocEnvNest = 𝕟64 0
  }

data LDocState = LDocState
  { ldocStateRib ∷ ℕ64
  , ldocStateRow ∷ ℕ64
  , ldocStateCol ∷ ℕ64
  } deriving (Eq,Ord,Show)
makeLenses ''LDocState

ldocState₀ ∷ LDocState
ldocState₀ = LDocState
  { ldocStateRib = 𝕟64 0
  , ldocStateRow = 𝕟64 0
  , ldocStateCol = 𝕟64 0
  }

type LDocM = RWS LDocEnv (𝐼 (RDoc IChunk)) LDocState

renderRDoc ∷ Shape → 𝐼 (RDoc IChunk) → LDocM ()
renderRDoc sh rdis = do
  nest ← askL ldocEnvNestL
  tell $ mapOn rdis $ mapp $ \case
    NewlineIChunk n → NewlineIChunk $ n + nest
    c → c
  case sh of
    SShape l → do
      modifyL ldocStateRibL $ (+) l
      modifyL ldocStateColL $ (+) l
    MShape (MultiShape _ _ _ ll lines) → do
      modifyL ldocStateRowL $ (+) lines
      putL ldocStateRibL ll
      putL ldocStateColL ll

alignLDoc ∷ LDocM a → LDocM a
alignLDoc xM = do
  col ← getL ldocStateColL
  nest ← askL ldocEnvNestL
  putL ldocStateColL $ 𝕟64 0
  x ← localL ldocEnvNestL (nest + col) xM
  modifyL ldocStateColL $ (+) col
  return x

groupLDoc ∷ Shape → 𝐼 (RDoc IChunk) → LDocM () → LDocM ()
groupLDoc sh rdis xM 
  | shape mShapeL sh = xM
  | otherwise = do
      lwO ← askL ldocEnvMaxLineWidthL
      rwO ← askL ldocEnvMaxRibbonWidthL
      nest ← askL ldocEnvNestL
      rib ← getL ldocStateRibL
      col ← getL ldocStateColL
      let ml :* mr = case sh of
            SShape l → (nest + col + l) :* (rib + l)
            MShape (MultiShape _ fl mml ll _) → 
              joins [ nest + col + fl , nest + mml , nest + ll ]
              :*
              joins [ rib + fl , mml , ll ]
          mlb = case lwO of
            None → True
            Some lw → ml ≤ lw
          mrb = case rwO of
            None → True
            Some rw → mr ≤ rw
      case mlb ⩓ mrb of 
        True → renderRDoc sh rdis
        False → xM
  
modeLDoc ∷ Shape → 𝐼 (RDoc IChunk) → PrettyMode → LDocM () → LDocM ()
modeLDoc sh rdis = \case
  NullMode → id
  AMode → alignLDoc
  GMode → groupLDoc sh rdis
  AGMode → alignLDoc ∘ groupLDoc sh rdis

compileLDoc ∷ LDoc → LDocM ()
compileLDoc = \case
  Leaf𝐴 (Summary shf rdisf) () m (shb :* rdisb) → modeLDoc shf rdisf m $ renderRDoc shb rdisb
  Append𝐴 (Summary shf rdisf) () m ld₁ lds₂ ld₃ → modeLDoc shf rdisf m $ do
    compileLDoc ld₁
    eachWith compileLDoc lds₂
    compileLDoc ld₃

execLDocWith ∷ (LDocM () → LDocM ()) → LDoc → RDoc IChunk
execLDocWith f = concat ∘ evalRWS ldocEnv₀ ldocState₀ ∘ retOut ∘ f ∘ compileLDoc

execLDoc ∷ LDoc → RDoc IChunk
execLDoc = execLDocWith id

-----------------
-- RDoc ⇒ SDoc --
-----------------

data RDocEnv = RDocEnv
  -- local env
  { rdocEnvUnderFormat ∷ 𝑂 (ℂ ∧ Formats)
  }
makeLenses ''RDocEnv

rdocEnv₀ ∷ RDocEnv
rdocEnv₀ = RDocEnv 
  { rdocEnvUnderFormat = None
  }

data RDocState = RDocState
  { rdocStateCol ∷ ℕ64
  , rdocStateUnders ∷ 𝐼 (ℕ64 ∧ ℕ64 ∧ ℂ ∧ Formats)
  }
makeLenses ''RDocState

rdocState₀ ∷ RDocState
rdocState₀ = RDocState
  { rdocStateCol = 𝕟64 0
  , rdocStateUnders = null
  }

type RDocM = RWS RDocEnv SDoc RDocState

buildUndertags ∷ ℕ64 → RDocM ()
buildUndertags l = do
  uf ← askL rdocEnvUnderFormatL
  case uf of
    None → skip
    Some (c :* fm) → do
      col ← getL rdocStateColL
      modifyL rdocStateUndersL $ flip (⧺) $ single (col :* l :* c :* fm)

renderNewline ∷ ℕ64 → RDocM ()
renderNewline n = do
  tell $ Leaf𝐴 () null () $ single $ NewlineOChunk n
  putL rdocStateColL n

renderRaw ∷ ℕ64 → 𝕊 → RDocM ()
renderRaw l s = do
  tell $ Leaf𝐴 () null () $ single $ RawOChunk l s
  modifyL rdocStateColL $ (+) l

renderPadding ∷ ℕ64 → RDocM ()
renderPadding n =
  case n ≡ zero of
    True → skip
    False → do
      tell $ Leaf𝐴 () null () $ single $ PaddingOChunk n
      modifyL rdocStateColL $ (+) n

renderUndertags ∷ RDocM ()
renderUndertags = do
  us ← list ^$ getL rdocStateUndersL
  putL rdocStateUndersL null
  case us ≡ null of
    True → skip
    False → do
      renderNewline zero
      eachOn us $ \ (colf :* l :* c :* fm) → do
        col ← getL rdocStateColL
        renderPadding $ colf - col
        formatRDoc fm $ renderRaw l $ string $ repeat (nat l) c

renderChunk ∷ IChunk → RDocM ()
renderChunk = \case
  RawIChunk l s → do buildUndertags l ; renderRaw l s
  NewlineIChunk n → do renderUndertags ; renderNewline n
  -- PaddingOChunk n → renderPadding n

formatRDoc ∷ Formats → RDocM () → RDocM ()
formatRDoc fm xM = do
  sd :* () ← hijack xM
  tell $ annoi fm sd

annotateRDoc ∷ Annotation → RDocM () → RDocM ()
annotateRDoc (Annotation fm ut) = formatRDoc fm ∘ localL rdocEnvUnderFormatL ut

compileRDoc ∷ RDoc IChunk → RDocM ()
compileRDoc = \case
  Leaf𝐴 () a () chs → annotateRDoc a $ eachWith renderChunk chs
  Append𝐴 () a () rd₁ rds₂ rd₃ → annotateRDoc a $ do
    compileRDoc rd₁
    eachWith compileRDoc rds₂
    compileRDoc rd₃

execRDoc ∷ RDoc IChunk → SDoc
execRDoc = evalRWS rdocEnv₀ rdocState₀ ∘ retOut ∘ compileRDoc

---------
-- Doc --
---------

data PrettyParams = PrettyParams
  { punctuationFormat        ∷ Formats
  , keywordPunctuationFormat ∷ Formats
  , keywordFormat            ∷ Formats
  , constructorFormat        ∷ Formats
  , operatorFormat           ∷ Formats
  , binderFormat             ∷ Formats
  , literalFormat            ∷ Formats
  , highlightFormat          ∷ Formats
  , headerFormat             ∷ Formats
  , errorFormat              ∷ Formats
  , lineNumberFormat         ∷ Formats
  , appLevel                 ∷ ℕ64
  } deriving (Eq,Ord,Show)
makeLenses ''PrettyParams

prettyParams₀ ∷ PrettyParams
prettyParams₀ = PrettyParams
  { punctuationFormat        = formats [FG darkGray]
  , keywordPunctuationFormat = formats [FG darkYellow,BD]
  , keywordFormat            = formats [FG darkYellow,BD]
  , constructorFormat        = formats [FG darkGreen,BD]
  , operatorFormat           = formats [FG darkBlue]
  , binderFormat             = formats [FG darkTeal]
  , literalFormat            = formats [FG darkRed]
  , highlightFormat          = formats [BG highlight]
  , headerFormat             = formats [FG darkPink,BD,UL]
  , errorFormat              = formats [FG white,BG darkRed]
  , lineNumberFormat         = formats [FG gray]
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

type DocM = RWS DocEnv LDoc ()
newtype Doc = Doc { unDoc ∷ DocM () }

instance Null Doc where null = Doc skip
instance Append Doc where d₁ ⧺ d₂ = Doc $ unDoc d₁ ≫  unDoc d₂
instance Monoid Doc

execDoc ∷ Doc → LDoc
execDoc = evalRWS docEnv₀ () ∘ retOut ∘ unDoc

onDoc ∷ (DocM () → DocM ()) → Doc → Doc
onDoc f = Doc ∘ f ∘ unDoc

-----------------
-- COMBINATORS --
-----------------

ppAnnotate ∷ Annotation → Doc → Doc
ppAnnotate a = onDoc $ mapOut $ homMap𝐴 (alter summaryContentsL $ map $ annoi a) $ mapSnd $ map $ annoi a

ppFormat ∷ Formats → Doc → Doc
ppFormat = ppAnnotate ∘ formatAnnotation

ppFormatParam ∷ PrettyParams ⟢ Formats → Doc → Doc
ppFormatParam l d = Doc $ do
  fmt ← askL $ l ⊚ docEnvPrettyParamsL
  unDoc $ ppFormat fmt d

ppUndertag ∷ ℂ → Formats → Doc → Doc
ppUndertag = ppAnnotate ∘∘ undertagAnnotation

ppGroup ∷ Doc → Doc
ppGroup = onDoc ∘ mapOut $ annoj GMode

ppAlign ∷ Doc → Doc
ppAlign = onDoc $ mapOut $ mapSummary (alter summaryShapeL alignShape) ∘ annoj AMode

ppGA ∷ Doc → Doc
ppGA = ppAlign ∘ ppGroup

ppString ∷ 𝕊 → Doc
ppString = Doc ∘ tell ∘ stringCChunk

ppStringModal ∷ 𝕊 → 𝕊 → Doc
ppStringModal sf sb = Doc $ tell $ stringCChunkModal sf sb

ppFG ∷ Color → Doc → Doc
ppFG c = ppFormat $ formats [FG c]

ppBG ∷ Color → Doc → Doc
ppBG c = ppFormat $ formats [BG c]

ppUL ∷ Doc → Doc
ppUL = ppFormat $ formats [UL]

ppBD ∷ Doc → Doc
ppBD = ppFormat $ formats [BD]

ppUT ∷ ℂ → Color → Doc → Doc
ppUT c o = ppUndertag c $ formats [FG o]

ppPun ∷ 𝕊 → Doc
ppPun = ppFormatParam punctuationFormatL ∘ ppString

ppKeyPun ∷ 𝕊 → Doc
ppKeyPun = ppFormatParam keywordPunctuationFormatL ∘ ppString

ppKey ∷ 𝕊 → Doc
ppKey = ppFormatParam keywordFormatL ∘ ppString

ppCon ∷ 𝕊 → Doc
ppCon = ppFormatParam constructorFormatL ∘ ppString

ppOp ∷ 𝕊 → Doc
ppOp = ppFormatParam operatorFormatL ∘ ppString

ppBdr ∷ 𝕊 → Doc
ppBdr = ppFormatParam binderFormatL ∘ ppString

ppLit ∷ 𝕊 → Doc
ppLit = ppFormatParam literalFormatL ∘ ppString

ppHl ∷ 𝕊 → Doc
ppHl = ppFormatParam highlightFormatL ∘ ppString

ppHeader ∷ 𝕊 → Doc
ppHeader = ppFormatParam headerFormatL ∘ ppString

ppErr ∷ 𝕊 → Doc
ppErr = ppFormatParam errorFormatL ∘ ppString


ppSpace ∷ ℕ64 → Doc
ppSpace n = ppString $ string $ repeat (nat n) ' '

ppNewline ∷ Doc
ppNewline = ppString "\n"

ppSpaceIfBreak ∷ Doc
ppSpaceIfBreak = ppStringModal "" " "

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

ppInf ∷ ℕ64 → Doc → Doc → Doc → Doc
ppInf i oM x₁M x₂M = ppGA $ ppLevel i $ ppSeparated $ map ppAlign $ iter [ppBump x₁M,oM,ppBump x₂M]

ppInfl ∷ ℕ64 → Doc → Doc → Doc → Doc
ppInfl i oM x₁M x₂M = ppGA $ ppLevel i $ ppSeparated $ map ppAlign $ iter [x₁M,oM,ppBump x₂M]

ppInfr ∷ ℕ64 → Doc → Doc → Doc → Doc
ppInfr i oM x₁M x₂M = ppGA $ ppLevel i $ ppSeparated $ map ppAlign $ iter [ppBump x₁M,oM,x₂M]

ppPre ∷ ℕ64 → Doc → Doc → Doc
ppPre i oM xM = ppGA $ ppLevel i $ ppSeparated $ map ppAlign $ iter [oM,xM]

ppPost ∷ ℕ64 → Doc → Doc → Doc
ppPost i oM xM = ppGA $ ppLevel i $ ppSeparated $ map ppAlign $ iter [xM,oM]

ppApp ∷ (ToIter Doc t) ⇒ Doc → t → Doc
ppApp x xs 
  | count xs ≡ zero = ppAlign x
  | otherwise = ppGA $ Doc $ do
    l ← askL $ appLevelL ⊚ docEnvPrettyParamsL
    unDoc $ ppLevel l $ ppGroup $ concat 
      [ ppAlign x
      , ppSpaceNewlineIfBreak
      , concat $ inbetween ppSpaceNewlineIfBreak $ map (ppAlign ∘ ppBump) $ iter xs
      ]

ppCollection ∷ (ToIter Doc t) ⇒ Doc → Doc → Doc → t → Doc
ppCollection l r i xs = ppGA $ ppSetBotLevel $ concat
  [ l
  , ppSpaceIfBreak
  , concat $ inbetween spacer $ iter xs
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
    mapping (k :* v) = ppGroup $ concat
      [ ppAlign k
      , ppSpaceIfBreak
      , rel
      , ppNewlineIfBreak
      , ppSpaceIfBreak
      , ppSpaceIfBreak
      , ppAlign v
      ]

-----------
-- CLASS --
-----------

class Pretty a where 
  pretty ∷ a → Doc

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
  '"' → iter "\\\""
  '\\' → iter "\\\\"
  '\n' → iter "\\n"
  '\t' → iter "\\t"
  '\r' → iter "\\r"
  '\b' → iter "\\b"
  '\f' → iter "\\f"
  c' → single c'

instance Pretty ℂ where 
  pretty c = ppLit $ string $ concat
    [ iter "'"
    , escape c
    , iter "'"
    ]

instance Pretty 𝕊 where 
  pretty s = ppLit $ string $ concat
    [ iter "\""
    , escape *$ iter s
    , iter "\""
    ]

instance (Pretty a,Pretty b) ⇒ Pretty (a,b) where
  pretty (a,b) = ppCollection (ppPun "(") (ppPun ")") (ppPun ",") [pretty a, pretty b]
instance (Pretty a,Pretty b) ⇒ Pretty (a ∧ b) where
  pretty (a :* b) = ppCollection (ppPun "⟨") (ppPun "⟩") (ppPun ",") [pretty a, pretty b]

instance (Pretty a) ⇒ Pretty (𝐿 a) where 
  pretty = ppCollection (ppPun "[") (ppPun "]") (ppPun ",") ∘ map pretty ∘ iter
instance (Pretty a) ⇒ Pretty [a] where 
  pretty = ppCollection (ppPun "[") (ppPun "]") (ppPun ",") ∘ map pretty ∘ iter
instance (Pretty a) ⇒ Pretty (𝕍 a) where 
  pretty xs = ppApp (ppString "𝕍") $ list [pretty $ list xs]
instance (Pretty a) ⇒ Pretty (𝑆 a) where 
  pretty xs = ppApp (ppString "𝑆") $ list [pretty $ list xs]
instance (Pretty a) ⇒ Pretty (𝐼 a) where 
  pretty xs = ppApp (ppString "𝐼") $ list [pretty $ list xs]
instance (Pretty a) ⇒ Pretty (𝐼S a) where 
  pretty xs = ppApp (ppString "𝐼S") $ list [pretty $ list xs]
instance (Pretty a) ⇒ Pretty (𝑄 a) where 
  pretty xs = ppApp (ppString "𝑄") $ list [pretty $ list xs]
instance (Pretty a) ⇒ Pretty (𝑃 a) where 
  pretty = ppCollection (ppPun "{") (ppPun "}") (ppPun ",") ∘ map pretty ∘ iter
instance (Pretty k,Pretty v) ⇒ Pretty (k ⇰ v) where 
  pretty = ppRecord (ppPun "↦") ∘ map (mapPair pretty pretty) ∘ iter

instance (Pretty a) ⇒ Pretty (AddNull a) where
  pretty Null = ppCon "•"
  pretty (AddNull x) = pretty x

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
