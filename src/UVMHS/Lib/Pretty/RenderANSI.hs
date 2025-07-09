module UVMHS.Lib.Pretty.RenderANSI where

import UVMHS.Core

import UVMHS.Lib.TreeAnnote
import UVMHS.Lib.Sep

import UVMHS.Lib.Pretty.Color
import UVMHS.Lib.Pretty.Annotation
import UVMHS.Lib.Pretty.Common
import UVMHS.Lib.Pretty.Doc
import UVMHS.Lib.Pretty.DocA
import UVMHS.Lib.Pretty.RenderUndertags

import Data.IORef (IORef)
import qualified Data.IORef as IORef

data ANSIEnv = ANSIEnv
  -- global env
  { ansiEnvDoFormat ∷ 𝔹
  -- local env
  , ansiEnvFormats ∷ Formats
  } deriving (Eq,Ord,Show)
makeLenses ''ANSIEnv

ansiEnv₀ ∷ ANSIEnv
ansiEnv₀ = ANSIEnv True null

type RenderANSIM = RWS ANSIEnv (𝐼A 𝕊) ()

sgrLeader ∷ 𝕊
sgrLeader = "\ESC["

sgrCloser ∷ 𝕊
sgrCloser = "m"

sgrReset ∷ 𝕊
sgrReset = sgrLeader ⧺ "0" ⧺ sgrCloser

sgrFg ∷ Color → 𝕊
sgrFg = \case
  Color c → case c of
    DefaultColor → "39"
    Black        → "30"
    Red          → "31"
    Green        → "32"
    Yellow       → "33"
    Blue         → "34"
    Magenta      → "35"
    Cyan         → "36"
    GrayLight    → "37"
    GrayDark     → "90"
    RedLight     → "91"
    GreenLight   → "92"
    YellowLight  → "93"
    BlueLight    → "94"
    PinkLight    → "95"
    TealLight    → "96"
    White        → "97"
  Color8 c → "38;5;" ⧺ show𝕊 c
  Color24 r g b → "38;2;" ⧺ show𝕊 r ⧺ ";" ⧺ show𝕊 g ⧺ ";" ⧺ show𝕊 b

sgrBg ∷ Color → 𝕊
sgrBg = \case
  Color c → case c of
    DefaultColor → "49"
    Black        → "40"
    Red          → "41"
    Green        → "42"
    Yellow       → "43"
    Blue         → "44"
    Magenta      → "45"
    Cyan         → "46"
    GrayLight    → "47"
    GrayDark     → "100"
    RedLight     → "101"
    GreenLight   → "102"
    YellowLight  → "103"
    BlueLight    → "104"
    PinkLight    → "105"
    TealLight    → "106"
    White        → "107"
  Color8 c → "48;5;" ⧺ show𝕊 c
  Color24 r g b → "48;2;" ⧺ show𝕊 r ⧺ ";" ⧺ show𝕊 g ⧺ ";" ⧺ show𝕊 b

sgrUl ∷ 𝔹 → 𝕊
sgrUl True = "4"
sgrUl False = "24"

sgrBd ∷ 𝔹 → 𝕊
sgrBd True = "1"
sgrBd False = "22"

sgrIt ∷ 𝔹 → 𝕊
sgrIt True = "3"
sgrIt False = "23"

sgrFormat ∷ Formats → 𝐼A 𝕊
sgrFormat (Formats fg bg ul bd it) = single $ stringS $ iter
  [ sgrLeader
  , concat $ inbetween ";" $ mconcat $ map (mzero𝑂 @𝑄) $ iter
      [ sgrFg ^$ fg
      , sgrBg ^$ bg
      , sgrUl ^$ ul
      , sgrBd ^$ bd
      , sgrIt ^$ it
      ]
  , sgrCloser
  ]

tellSgrFormat ∷ RenderANSIM ()
tellSgrFormat = do
  cf ← askL ansiEnvFormatsL
  tell $ sgrFormat cf

localFormat ∷ Formats → RenderANSIM () → RenderANSIM ()
localFormat f aM = do
  mapEnvL ansiEnvFormatsL (prepend f) $ do
    tellSgrFormat
    aM
  tell $ single sgrReset
  tellSgrFormat

renderChunkANSI ∷ ChunkO → 𝐼A 𝕊
renderChunkANSI = \case
  RawChunkO     n s → 𝐼A n $ single s
  PaddingChunkO n   → 𝐼A n $ single $ string $ replicate (nat n) ' '

formatRenderANSI ∷ Formats → RenderANSIM () → RenderANSIM ()
formatRenderANSI fm xM = do
  b ← askL ansiEnvDoFormatL
  case b of
    True → localFormat fm xM
    False → xM

compileOTree ∷ TreeO → RenderANSIM ()
compileOTree sd = un𝑇V sd fₑ fₐ
  where
    fₑ ∷ Sep () (𝐼A ChunkO) → RenderANSIM ()
    fₑ chs = eachWith tell $ iter $ mapSep (const $ single @_ @(𝐼A _) $ 𝕤 "\n") (concat ∘ map renderChunkANSI ∘ iter) chs
    fₐ ∷ Formats → RenderANSIM () → RenderANSIM ()
    fₐ fm = formatRenderANSI fm

execRenderANSIWith ∷ (RenderANSIM () → RenderANSIM ()) → TreeO → 𝐼A 𝕊
execRenderANSIWith f = evalRWS ansiEnv₀ () ∘ retOut ∘ f ∘ compileOTree

execRenderANSI ∷ TreeO → 𝐼A 𝕊
execRenderANSI = execRenderANSIWith id

gv_PPRINT_COLOR ∷ IORef 𝔹
gv_PPRINT_COLOR = io_UNSAFE $ IORef.newIORef True

ppRenderWith ∷ (RenderANSIM () → RenderANSIM ())
             → (DocAM () → DocAM ())
             → (DocM () → DocM ())
             → Doc → 𝕊
ppRenderWith f₁ f₃ f₄ d = io_UNSAFE $ do
  b ← IORef.readIORef gv_PPRINT_COLOR
  let f₁' = appto f₁ $ if b then id else (∘) $ localL ansiEnvDoFormatL False
  return $ appto (ppG d) $
    stringSS
    ∘ execRenderANSIWith f₁'
    ∘ summaryOContents
    ∘ execRenderUT
    ∘ execDocAWith f₃
    ∘ execDocWith f₄

ppRender ∷ Doc → 𝕊
ppRender = ppRenderWith id id id

ppRenderNoFmt ∷ Doc → 𝕊
ppRenderNoFmt = ppRenderWith (localL ansiEnvDoFormatL False) id id

ppRenderWide ∷ Doc → 𝕊
ppRenderWide =
  ppRenderWith id
               (localL docAEnvMaxLineWidthL None
                ∘ localL docAEnvMaxRibbonWidthL None)
               id

ppRenderWidth ∷ ℕ64 → ℕ64 → Doc → 𝕊
ppRenderWidth w rw =
  ppRenderWith id
               (localL docAEnvMaxLineWidthL (Some w)
                ∘ localL docAEnvMaxRibbonWidthL (Some rw))
               id

ppRenderNarrow ∷ Doc → 𝕊
ppRenderNarrow = ppRenderWidth 0 0

ppRenderNoFmtWide ∷ Doc → 𝕊
ppRenderNoFmtWide =
  ppRenderWith (localL ansiEnvDoFormatL False)
               (localL docAEnvMaxLineWidthL None ∘ localL docAEnvMaxRibbonWidthL None)
               id

ppRenderNoFmtWidth ∷ ℕ64 → ℕ64 → Doc → 𝕊
ppRenderNoFmtWidth w rw =
  ppRenderWith (localL ansiEnvDoFormatL False)
               (localL docAEnvMaxLineWidthL (Some w)
                ∘ localL docAEnvMaxRibbonWidthL (Some rw))
               id

ppRenderNoFmtNarrow ∷ Doc → 𝕊
ppRenderNoFmtNarrow = ppRenderNoFmtWidth 0 0

pprint ∷ (Pretty a) ⇒ a → IO ()
pprint = out ∘ ppRender ∘ pretty

pprintNoFmt ∷ (Pretty a) ⇒ a → IO ()
pprintNoFmt = out ∘ ppRenderNoFmt ∘ pretty

pprintWide ∷ (Pretty a) ⇒ a → IO ()
pprintWide = out ∘ ppRenderWide ∘ pretty

pprintWidth ∷ (Pretty a) ⇒ ℕ64 → ℕ64 → a → IO ()
pprintWidth w rw = out ∘ ppRenderWidth w rw ∘ pretty

pprintNarrow ∷ (Pretty a) ⇒ a → IO ()
pprintNarrow = out ∘ ppRenderNarrow ∘ pretty

pprintNoFmtWide ∷ (Pretty a) ⇒ a → IO ()
pprintNoFmtWide = out ∘ ppRenderNoFmtWide ∘ pretty

pprintNoFmtWidth ∷ (Pretty a) ⇒ ℕ64 → ℕ64 → a → IO ()
pprintNoFmtWidth w rw = out ∘ ppRenderNoFmtWidth w rw ∘ pretty

pprintNoFmtNarrow ∷ (Pretty a) ⇒ a → IO ()
pprintNoFmtNarrow = out ∘ ppRenderNoFmtNarrow ∘ pretty

ppshow ∷ (Pretty a) ⇒ a → 𝕊
ppshow = ppRenderNoFmtWide ∘ pretty

ppColorOn ∷ IO ()
ppColorOn = IORef.writeIORef gv_PPRINT_COLOR True

ppColorOff ∷ IO ()
ppColorOff = IORef.writeIORef gv_PPRINT_COLOR False

pptrace ∷ (Pretty a) ⇒ a → ()
pptrace a = io_UNSAFE $ do
  pprint a
  return ()

pptraceM ∷ (Monad m,Pretty a) ⇒ a → m ()
pptraceM a = let _ = pptrace a in skip

ppabort ∷ (Pretty a) ⇒ a → IO b
ppabort x = do pprint x ; abortIO

instance Eq Doc where (==) = (≡) `on` ppRender
instance Ord Doc where compare = compare `on` ppRender
