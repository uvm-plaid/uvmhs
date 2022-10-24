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
  , gerrorCxt ∷ () → Doc
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

errSetTyp ∷ (Monad m,MonadReader r m,HasLens r GError) ⇒ (() → 𝕊) → m a → m a
errSetTyp = localL $ gerrorTypL ⊚ hasLens

errSetLoc ∷ (Monad m,MonadReader r m,HasLens r GError) ⇒ (() → 𝑃 SrcCxt) → m a → m a
errSetLoc = localL $ gerrorLocL ⊚ hasLens

errSetMsg ∷ (Monad m,MonadReader r m,HasLens r GError) ⇒ (() → 𝕊) → m a → m a
errSetMsg = localL $ gerrorMsgL ⊚ hasLens

errSetCxt ∷ (Monad m,MonadReader r m,HasLens r GError) ⇒ (() → Doc) → m a → m a
errSetCxt = localL $ gerrorCxtL ⊚ hasLens

errModCxt ∷ (Monad m,MonadReader r m,HasLens r GError) ⇒ (Doc → Doc) → m a → m a
errModCxt = mapEnvL (gerrorCxtL ⊚ hasLens) ∘ map

errTyp ∷ (Monad m,MonadReader r m,HasLens r GError) ⇒ m 𝕊
errTyp = appto () ^$ askL $ gerrorTypL ⊚ hasLens

errLoc ∷ (Monad m,MonadReader r m,HasLens r GError) ⇒ m (𝑃 SrcCxt)
errLoc = appto () ^$ askL $ gerrorLocL ⊚ hasLens

errMsg ∷ (Monad m,MonadReader r m,HasLens r GError) ⇒ m 𝕊
errMsg = appto () ^$ askL $ gerrorMsgL ⊚ hasLens

errCxt ∷ (Monad m,MonadReader r m,HasLens r GError) ⇒ m Doc
errCxt = appto () ^$ askL $ gerrorCxtL ⊚ hasLens
