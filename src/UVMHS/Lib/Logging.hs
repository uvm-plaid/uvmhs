module UVMHS.Lib.Logging where

import UVMHS.Core
import UVMHS.Lib.Pretty

newtype LogLevel = LogLevel { unLogLevel ∷ ℕ64 }
  deriving (Eq,Ord,Show,Pretty)

newtype LogDepth = LogDepth { unLogDepth ∷ ℕ64 }
  deriving (Eq,Ord,Show,Pretty)

pplog ∷ (Monad m,MonadIO m,MonadReader r m,HasLens r LogLevel) ⇒ ℕ64 → Doc → m ()
pplog l ~msg = do
  ll ← unLogLevel ^$ askL hasLens
  whenZ (l ≤ ll) $ io $ pprint $ concat 
    [ ppBG grayDark $ ppFG white $ ppString $ concat ["▷",show𝕊 l,"◁"]
    , ppSpace 1
    , ppGA msg
    ]

pplogd ∷ (Monad m,MonadIO m,MonadReader r m,HasLens r LogLevel,HasLens r LogDepth) ⇒ ℕ64 → Doc → m ()
pplogd l msg = do
  ld ← unLogDepth ^$ askL hasLens
  pplog l $ ppSpace (ld × 𝕟64 2) ⧺ ppGA msg

pplogdIndent ∷ 
  (Monad m,MonadIO m,MonadReader r m,HasLens r LogLevel,HasLens r LogDepth)
  ⇒ m a → m a
pplogdIndent = mapEnvL hasLens $ LogDepth ∘ succ ∘ unLogDepth

pplogdIndentReset ∷ 
  (Monad m,MonadIO m,MonadReader r m,HasLens r LogLevel,HasLens r LogDepth)
  ⇒ m a → m a
pplogdIndentReset = mapEnvL hasLens $ const $ LogDepth zero
