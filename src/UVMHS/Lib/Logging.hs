module UVMHS.Lib.Logging where

import UVMHS.Core
import UVMHS.Lib.Pretty

newtype LogLevel = LogLevel { unLogLevel ∷ ℕ64 }
  deriving (Eq,Ord,Show,Pretty)

newtype LogDepth = LogDepth { unLogDepth ∷ ℕ64 }
  deriving (Eq,Ord,Show,Pretty)

data LogOptions = LogOptions
  { logOptionsLevel ∷ ℕ64
  , logOptionsDepth ∷ ℕ64
  , logOptionsShowLevel ∷ 𝔹
  } deriving (Eq,Ord,Show)
makeLenses ''LogOptions

logOptions₀ ∷ LogOptions
logOptions₀ = LogOptions 0 0 False

pplog ∷ (Monad m,MonadIO m,MonadReader r m,HasLens r LogOptions) ⇒ ℕ64 → (() → Doc) → m ()
pplog l msg = do
  ll ← askL $ logOptionsLevelL ⊚ hasLens
  b ← askL $ logOptionsShowLevelL ⊚ hasLens
  when (l ≤ ll) $ \ () → io $ do
    pprint $ concat
      [ if not b then null else concat
          [ ppBG grayDark $ ppFG white $ ppString $ concat ["▷",show𝕊 l,"◁"]
          , ppSpace 1
          ]
      , ppGA $ msg ()
      ]
    oflush

pplogd ∷ (Monad m,MonadIO m,MonadReader r m,HasLens r LogOptions) ⇒ ℕ64 → (() → Doc) → m ()
pplogd l msg = do
  ld ← askL $ logOptionsDepthL ⊚ hasLens
  pplog l $ \ () → ppSpace (ld × 𝕟64 2) ⧺ ppGA (msg ())

pplogdIndent ∷ (Monad m,MonadIO m,MonadReader r m,HasLens r LogOptions) ⇒ m a → m a
pplogdIndent = mapEnvL (logOptionsDepthL ⊚ hasLens) succ

pplogdIndentReset ∷ (Monad m,MonadIO m,MonadReader r m,HasLens r LogOptions) ⇒ m a → m a
pplogdIndentReset = localL (logOptionsDepthL ⊚ hasLens) zero

whenLogLevel ∷ (Monad m,MonadReader r m,HasLens r LogOptions) ⇒ ℕ64 → (() → m ()) → m ()
whenLogLevel l f = do
  ll ← askL $ logOptionsLevelL ⊚ hasLens
  when (l ≤ ll) f

