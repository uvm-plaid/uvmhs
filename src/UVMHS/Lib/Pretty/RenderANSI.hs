module UVMHS.Lib.Pretty.RenderANSI where

import UVMHS.Core

import UVMHS.Lib.IterS
import UVMHS.Lib.ATree

import UVMHS.Lib.Pretty.Annotation
import UVMHS.Lib.Pretty.Core

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

type RenderANSIM = RWS ANSIEnv 𝐼S𝕊 ()

sgrLeader ∷ 𝕊
sgrLeader = "\ESC["

sgrCloser ∷ 𝕊
sgrCloser = "m"

sgrReset ∷ 𝕊
sgrReset = sgrLeader ⧺ "0" ⧺ sgrCloser

sgrFg ∷ 𝑂 Color → 𝕊
sgrFg None = "39"
sgrFg (Some (Color c)) = case c of
  Black → "90"
  Red → "91"
  Green → "92"
  Yellow → "93"
  Blue → "94"
  Magenta → "95"
  Cyan → "96"
  White → "97"
sgrFg (Some (Color8 c)) = "38;5;" ⧺ show𝕊 c
sgrFg (Some (Color24 r g b)) = "38;2;" ⧺ show𝕊 r ⧺ ";" ⧺ show𝕊 g ⧺ ";" ⧺ show𝕊 b

sgrBg ∷ 𝑂 Color → 𝕊
sgrBg None = "49"
sgrBg (Some (Color c)) = case c of
  Black → "100"
  Red → "101"
  Green → "102"
  Yellow → "103"
  Blue → "104"
  Magenta → "105"
  Cyan → "106"
  White → "107"
sgrBg (Some (Color8 c)) = "48;5;" ⧺ show𝕊 c
sgrBg (Some (Color24 r g b)) = "48;2;" ⧺ show𝕊 r ⧺ ";" ⧺ show𝕊 g ⧺ ";" ⧺ show𝕊 b

sgrUl ∷ 𝔹 → 𝕊
sgrUl True = "4"
sgrUl False = "24"

sgrBd ∷ 𝔹 → 𝕊
sgrBd True = "1"
sgrBd False = "22"

sgrIt ∷ 𝔹 → 𝕊
sgrIt True = "3"
sgrIt False = "23"

sgrFormat ∷ Formats → 𝐼S𝕊
sgrFormat (Formats fg bg ul bd it) = single $ stringC $ iter
  [ sgrLeader 
  , concat $ inbetween ";" $ mconcat $ map (mzero𝑂 @ 𝑄) $ iter
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
  localL ansiEnvFormatsL f $ do
    tellSgrFormat
    aM
  tell $ single sgrReset
  tellSgrFormat

renderChunk' ∷ OChunk → 𝐼S𝕊
renderChunk' = \case
  RawOChunk n s → 𝐼S𝕊 n $ single s
  NewlineOChunk n → concat [single "\n",𝐼S𝕊 n $ single $ string $ repeat (nat n) ' ']
  PaddingOChunk n → 𝐼S𝕊 n $ single $ string $ repeat (nat n) ' '
  
formatSDoc ∷ Formats → RenderANSIM () → RenderANSIM ()
formatSDoc fm xM = do
  b ← askL ansiEnvDoFormatL
  case b of
    True → localFormat fm xM
    False → xM

compileSDoc ∷ SDoc → RenderANSIM ()
compileSDoc = \case
  Leaf𝐴 () fm () chs → formatSDoc fm $ eachWith (tell ∘ renderChunk') chs
  Append𝐴 () fm () sd₁ sds₂ sd₃ → formatSDoc fm $ do
    compileSDoc sd₁
    eachWith compileSDoc sds₂
    compileSDoc sd₃

execSDocWith ∷ (RenderANSIM () → RenderANSIM ()) → SDoc → 𝕊
execSDocWith f = stringCS ∘ evalRWS ansiEnv₀ () ∘ retOut ∘ f ∘ compileSDoc

execSDoc ∷ SDoc → 𝕊
execSDoc = execSDocWith id

ppRender ∷ Doc → 𝕊
ppRender = execSDoc ∘ execRDoc ∘ execLDoc ∘ execDoc

ppRenderNofmt ∷ Doc → 𝕊
ppRenderNofmt = 
  execSDocWith (localL ansiEnvDoFormatL False) 
  ∘ execRDoc 
  ∘ execLDoc 
  ∘ execDoc

ppRenderWide ∷ Doc → 𝕊
ppRenderWide = 
  execSDoc
  ∘ execRDoc 
  ∘ execLDocWith (localL ldocEnvMaxLineWidthL None ∘ localL ldocEnvMaxRibbonWidthL None)
  ∘ execDoc

ppRenderNofmtWide ∷ Doc → 𝕊
ppRenderNofmtWide = 
  execSDocWith (localL ansiEnvDoFormatL False) 
  ∘ execRDoc 
  ∘ execLDocWith (localL ldocEnvMaxLineWidthL None ∘ localL ldocEnvMaxRibbonWidthL None)
  ∘ execDoc

ppshow ∷ (Pretty a) ⇒ a → 𝕊
ppshow = ppRenderNofmtWide ∘ pretty

{-# NOINLINE gv_PPRINT_COLOR #-}
gv_PPRINT_COLOR ∷ IORef 𝔹
gv_PPRINT_COLOR = ioUNSAFE $ IORef.newIORef True

pprint ∷ (Pretty a) ⇒ a → IO ()
pprint x = do
  b ← IORef.readIORef gv_PPRINT_COLOR
  if b
     then out $ ppRender $ pretty x
     else out $ ppRenderNofmt $ pretty x

ppColorOn ∷ IO ()
ppColorOn = IORef.writeIORef gv_PPRINT_COLOR True

ppColorOff ∷ IO ()
ppColorOff = IORef.writeIORef gv_PPRINT_COLOR False

pptrace ∷ (Pretty a) ⇒ a → b → b
pptrace a = ioUNSAFE $ do
  pprint a
  return id

pptraceM ∷ (Monad m,Pretty a) ⇒ a → m ()
pptraceM a = pptrace a skip

ioError ∷ (Pretty e) ⇒ e ∨ a → IO a
ioError = elimChoice (\ e → pprint e ≫ abortIO) return

{-
interpConsoleOutANSI ∷ ConsoleOut → RenderANSIM ()
interpConsoleOutANSI NullCO = skip
interpConsoleOutANSI (ChunkCO s) = tell $ single s
interpConsoleOutANSI (AppendCO o₁ o₂) = exec [interpConsoleOutANSI o₁,interpConsoleOutANSI o₂]
interpConsoleOutANSI (FormatCO f o) = localFormat f $ interpConsoleOutANSI o

execConsoleOutANSI ∷ ConsoleOut → 𝕊
execConsoleOutANSI = concat ∘ evalConsoleANSIM ∘ retOut ∘ interpConsoleOutANSI

pprenderWith ∷ (Pretty a) ⇒ (Doc → Doc) → a → 𝕊
pprenderWith f = execConsoleOutANSI ∘ execPrettyOut ∘ execDoc ∘ f ∘ pretty

pprenderWidth ∷ (Pretty a) ⇒ ℕ → a → 𝕊
pprenderWidth = pprenderWith ∘ onDoc ∘ mapEnv ∘ update maxColumnWidthL

pprender ∷ (Pretty a) ⇒ a → 𝕊
pprender = pprenderWith id

pprint ∷ (Pretty a) ⇒ a → IO ()
pprint = out ∘ pprender

pptrace ∷ (Pretty a) ⇒ a → b → b
pptrace a b = unsafePerformIO $ do
  pprint a
  return b

pptraceM ∷ (Monad m,Pretty a) ⇒ a → m ()
pptraceM a = pptrace a skip

ioError ∷ (Pretty e) ⇒ e ∨ a → IO a
ioError = elimChoice (\ e → pprint e ≫ abortIO) return
-}
