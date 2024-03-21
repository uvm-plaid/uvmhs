module UVMHS.Lib.Pretty.Console where

import UVMHS.Core
import UVMHS.Lib.Pretty.Color
import UVMHS.Lib.Pretty.Core

data ConsoleEnv = ConsoleEnv
  { ρUnderFormat ∷ 𝑂 (Formats ∧ ℂ)
  , ρLineNumberWidth ∷ ℕ
  }
makeLenses ''ConsoleEnv

consoleEnv₀ ∷ ConsoleEnv
consoleEnv₀ = ConsoleEnv
  { ρUnderFormat = None
  , ρLineNumberWidth = 0
  }

data ConsoleOut =
    NullCO
  | ChunkCO 𝕊
  | AppendCO ConsoleOut ConsoleOut
  | FormatCO Formats ConsoleOut
instance Null ConsoleOut where null = NullCO
instance Append ConsoleOut where (⧺) = AppendCO
instance Monoid ConsoleOut

data ConsoleState = ConsoleState
  { σCol ∷ ℕ
  , σUnders ∷ 𝐿 (ℕ ∧ ℕ ∧ Formats ∧ ℂ)
  }
makeLenses ''ConsoleState

consoleState₀ ∷ ConsoleState
consoleState₀ = ConsoleState
  { σCol = 0
  , σUnders = list []
  }

newtype ConsoleM a = ConsoleM { runConsoleM ∷ RWS ConsoleEnv ConsoleOut ConsoleState a }
  deriving
  ( Functor,Return,Bind,Monad
  , MonadReader ConsoleEnv
  , MonadWriter ConsoleOut
  , MonadState ConsoleState
  )
evalConsoleM ∷ ConsoleEnv → ConsoleState → ConsoleM a → a
evalConsoleM e s aM = evalRWS e s $ runConsoleM aM

spitConsole ∷ 𝕊 → ConsoleM ()
spitConsole s = do
  tell $ ChunkCO s
  modifyL σColL $ (+) $ length𝕊 s

spitNLConsole ∷ ConsoleM ()
spitNLConsole = do
  tell $ ChunkCO "\n"
  putL σColL $ 0

interpChunk ∷ Chunk → ConsoleM ()
interpChunk (LineNumber n) = do
  lnw ← askL ρLineNumberWidthL
  spitConsole $ alignRight lnw $ show𝕊 n
  spitConsole ": "
interpChunk (Text s) = do
  col ← getL σColL
  spitConsole s
  fM ← askL ρUnderFormatL
  when𝑂 fM $ \ (f :* c) → do
    col' ← getL σColL
    modifyL σUndersL $ (:&) $ (col :* col' :* f :* c)
interpChunk Newline = do
  doUnders
  spitNLConsole

doUnders ∷ ConsoleM ()
doUnders = do
  us ← getL σUndersL
  when (not $ isEmpty us) $ \ () → do
    spitNLConsole
    eachOn (reverse us) $ \ (colₗ :* colᵤ :* f :* c) → do
      col ← getL σColL
      spitConsole $ string $ repeat (colₗ - col) ' '
      mapOut (FormatCO f) $
        spitConsole $ string $ repeat (colᵤ - colₗ) c
    putL σUndersL $ list []

finalize ∷ ConsoleM a → ConsoleM a
finalize aM = do
  a ← aM
  doUnders
  return a

interpAnnotation ∷ Annotation → ConsoleM () → ConsoleM ()
interpAnnotation (FormatA f) = mapOut $ FormatCO $ concat $ map formats f
interpAnnotation (UndertagA fcO) = mapEnv $ update ρUnderFormatL $ case fcO of
  None → None
  Some (f :* c) → Some $ (concat $ map formats f) :* c

interpOutputElem ∷ OutputElem → ConsoleM ()
interpOutputElem (RawChunk c) = interpChunk c
interpOutputElem (AnnotatedOutput a o) = interpAnnotation a $ interpOutput o

interpOutput ∷ Output → ConsoleM ()
interpOutput = exec ∘ map interpOutputElem ∘ iter

execPrettyOut ∷ PrettyOut → ConsoleOut
execPrettyOut (PrettyOut o ln) =
  evalConsoleM consoleEnv₀ consoleState₀
    $ retOut
    $ finalize
    $ mapEnv (update ρLineNumberWidthL ln)
    $ interpOutput o
