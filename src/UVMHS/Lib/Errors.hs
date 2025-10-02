module UVMHS.Lib.Errors where

import UVMHS.Core
import UVMHS.Lib.Parser
import UVMHS.Lib.Pretty

oops ∷ (Monad m,MonadReader r m,HasLens r e,MonadError e m) ⇒ m a
oops = throw *$ askL hasLens

data GError = GError
  { gerrorTyp ∷ () → 𝕊
  , gerrorLoc ∷ () → 𝑃 SrcCxt
  , gerrorMsg ∷ () → 𝕊
  , gerrorCxt ∷ () → 𝐿 Doc
  }
makeLenses ''GError

instance Pretty GError where
  pretty (GError typ loc msg cxt) = ppVertical $ concat
    [ single𝐼 $ ppHeader $ typ ()
    , map pretty $ iter $ loc ()
    , single𝐼 $ ppErr $ msg ()
    , single𝐼 $ pretty $ cxt ()
    ]

gerror₀ ∷ GError
gerror₀ = GError (const "internal error") null (const "<unknown cause>") null

gerrorFromIO ∷ IOError → GError
gerrorFromIO e = GError (const "IO Error") null (\ () → show𝕊 e) null

-- ERR AS STATE EFFECT --

localizeErr ∷ (Monad m,MonadState s m,HasLens s GError) ⇒ m a → m a
localizeErr = localizeL $ hasLens @_ @GError

setErrTyp ∷ (Monad m,MonadState s m,HasLens s GError) ⇒ (() → 𝕊) → m ()
setErrTyp = putL $ gerrorTypL ⊚ hasLens

setErrLoc ∷ (Monad m,MonadState s m,HasLens s GError) ⇒ (() → 𝑃 SrcCxt) → m ()
setErrLoc = putL $ gerrorLocL ⊚ hasLens

setErrMsg ∷ (Monad m,MonadState s m,HasLens s GError) ⇒ (() → 𝕊) → m ()
setErrMsg = putL $ gerrorMsgL ⊚ hasLens

setErrCxt ∷ (Monad m,MonadState s m,HasLens s GError) ⇒ (() → 𝐿 Doc) → m ()
setErrCxt = putL $ gerrorCxtL ⊚ hasLens

modErrCxt ∷ (Monad m,MonadState s m,HasLens s GError) ⇒ (𝐿 Doc → 𝐿 Doc) → m ()
modErrCxt = modifyL (gerrorCxtL ⊚ hasLens) ∘ map

initErrCxt ∷ (Monad m,MonadState s m,HasLens s GError) ⇒ (() → Doc) → m ()
initErrCxt d = setErrCxt $ \ () → single $ d ()

pushErrCxt ∷ (Monad m,MonadState s m,HasLens s GError) ⇒ (() → Doc) → m ()
pushErrCxt c = modErrCxt $ \ cxt → c () :& cxt

getErrTyp ∷ (Monad m,MonadState s m,HasLens s GError) ⇒ m 𝕊
getErrTyp = appto () ^$ getL $ gerrorTypL ⊚ hasLens

getErrLoc ∷ (Monad m,MonadState s m,HasLens s GError) ⇒ m (𝑃 SrcCxt)
getErrLoc = appto () ^$ getL $ gerrorLocL ⊚ hasLens

getErrMsg ∷ (Monad m,MonadState s m,HasLens s GError) ⇒ m 𝕊
getErrMsg = appto () ^$ getL $ gerrorMsgL ⊚ hasLens

getErrCxt ∷ (Monad m,MonadState s m,HasLens s GError) ⇒ m (𝐿 Doc)
getErrCxt = appto () ^$ getL $ gerrorCxtL ⊚ hasLens

getErrCxtDoc ∷ (Monad m,MonadState s m,HasLens s GError) ⇒ m Doc
getErrCxtDoc = ppVertical ∘ reverse ^$ getErrCxt

-- ERR AS READER EFFECT --

setErrTypEnv ∷ (Monad m,MonadReader r m,HasLens r GError) ⇒ (() → 𝕊) → m a → m a
setErrTypEnv = localL $ gerrorTypL ⊚ hasLens

setErrLocEnv ∷ (Monad m,MonadReader r m,HasLens r GError) ⇒ (() → 𝑃 SrcCxt) → m a → m a
setErrLocEnv = localL $ gerrorLocL ⊚ hasLens

setErrMsgEnv ∷ (Monad m,MonadReader r m,HasLens r GError) ⇒ (() → 𝕊) → m a → m a
setErrMsgEnv = localL $ gerrorMsgL ⊚ hasLens

setErrCxtEnv ∷ (Monad m,MonadReader r m,HasLens r GError) ⇒ (() → 𝐿 Doc) → m a → m a
setErrCxtEnv = localL $ gerrorCxtL ⊚ hasLens

modErrCxtEnv ∷ (Monad m,MonadReader r m,HasLens r GError) ⇒ (𝐿 Doc → 𝐿 Doc) → m a → m a
modErrCxtEnv = mapEnvL (gerrorCxtL ⊚ hasLens) ∘ map

pushErrCxtEnv ∷ (Monad m,MonadReader r m,HasLens r GError) ⇒ (() → Doc) → m a → m a
pushErrCxtEnv c = modErrCxtEnv $ \ cxt → c () :& cxt

getErrTypEnv ∷ (Monad m,MonadReader r m,HasLens r GError) ⇒ m 𝕊
getErrTypEnv = appto () ^$ askL $ gerrorTypL ⊚ hasLens

getErrLocEnv ∷ (Monad m,MonadReader r m,HasLens r GError) ⇒ m (𝑃 SrcCxt)
getErrLocEnv = appto () ^$ askL $ gerrorLocL ⊚ hasLens

getErrMsgEnv ∷ (Monad m,MonadReader r m,HasLens r GError) ⇒ m 𝕊
getErrMsgEnv = appto () ^$ askL $ gerrorMsgL ⊚ hasLens

getErrCxtEnv ∷ (Monad m,MonadReader r m,HasLens r GError) ⇒ m (𝐿 Doc)
getErrCxtEnv = appto () ^$ askL $ gerrorCxtL ⊚ hasLens

getErrCxtDocEnv ∷ (Monad m,MonadReader r m,HasLens r GError) ⇒ m Doc
getErrCxtDocEnv = ppVertical ∘ reverse ^$ getErrCxtEnv
