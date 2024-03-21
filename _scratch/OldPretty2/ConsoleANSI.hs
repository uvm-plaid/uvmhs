module UVMHS.Lib.Pretty.ConsoleANSI where

import UVMHS.Core
import UVMHS.Lib.Pretty.Color
import UVMHS.Lib.Pretty.Console
import UVMHS.Lib.Pretty.Class
import UVMHS.Lib.Pretty.Core

import System.IO.Unsafe (unsafePerformIO)

newtype ConsoleANSIM a = ConsoleANSIM { runConsoleANSIM ∷ RWS Formats (𝑄 𝕊) () a }
  deriving
  ( Functor,Return,Bind,Monad
  , MonadReader Formats
  , MonadWriter (𝑄 𝕊)
  , MonadState ()
  )

evalConsoleANSIM ∷ ConsoleANSIM a → a
evalConsoleANSIM = evalRWS null () ∘ runConsoleANSIM

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

sgrFormat ∷ Formats → 𝑄 𝕊
sgrFormat (Formats fg bg ul bd it) = single $ concat
  [ sgrLeader
  , concat $ inbetween ";" $ list $ mconcat $ map (mzero𝑂 @ 𝑄)
      [ sgrFg ^$ fg
      , sgrBg ^$ bg
      , sgrUl ^$ ul
      , sgrBd ^$ bd
      , sgrIt ^$ it
      ]
  , sgrCloser
  ]

tellSgrFormat ∷ ConsoleANSIM ()
tellSgrFormat = do
  cf ← ask
  tell $ sgrFormat cf

localFormat ∷ Formats → ConsoleANSIM () → ConsoleANSIM ()
localFormat f aM = do
  local f $ do
    tellSgrFormat
    aM
  tell $ single sgrReset
  tellSgrFormat

interpConsoleOutANSI ∷ ConsoleOut → ConsoleANSIM ()
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
