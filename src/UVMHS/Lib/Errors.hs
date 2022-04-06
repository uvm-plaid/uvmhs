module UVMHS.Lib.Errors where

import UVMHS.Core
import UVMHS.Lib.Parser
import UVMHS.Lib.Pretty
import UVMHS.Lib.TreeNested

oops ∷ (Monad m,MonadReader r m,HasLens r e,MonadError e m) ⇒ m a
oops = throw *$ askL hasLens

data GError = GError
  { gerrorTyp ∷ 𝕊
  , gerrorLoc ∷ 𝑃 SrcCxt
  , gerrorMsg ∷ 𝕊
  , gerrorCxt ∷ 𝑇A Doc
  }
makeLenses ''GError

instance Pretty GError where
  pretty (GError typ loc msg cxt) = ppVertical $ concat
    [ single𝐼 $ ppHeader typ
    , map pretty $ iter loc
    , single𝐼 $ ppErr msg
    , single𝐼 $ pretty cxt
    ]

gerror₀ ∷ GError
gerror₀ = GError "internal error" null "unknown error" null

errTyp ∷ (Monad m,MonadReader r m,HasLens r GError) ⇒ 𝕊 → m a → m a
errTyp = localL $ gerrorTypL ⊚ hasLens

errLoc ∷ (Monad m,MonadReader r m,HasLens r GError) ⇒ 𝑃 SrcCxt → m a → m a
errLoc = localL $ gerrorLocL ⊚ hasLens

errMsg ∷ (Monad m,MonadReader r m,HasLens r GError) ⇒ 𝕊 → m a → m a
errMsg = localL $ gerrorMsgL ⊚ hasLens

errCxt ∷ (Monad m,MonadReader r m,HasLens r GError) ⇒ 𝑇A Doc → m a → m a
errCxt = localL $ gerrorCxtL ⊚ hasLens

uerrTyp ∷ (Monad m,MonadUCont m,MonadReader r m,HasLens r GError) ⇒ 𝕊 → m a → m a
uerrTyp = ulocalL $ gerrorTypL ⊚ hasLens

uerrLoc ∷ (Monad m,MonadUCont m,MonadReader r m,HasLens r GError) ⇒ 𝑃 SrcCxt → m a → m a
uerrLoc = ulocalL $ gerrorLocL ⊚ hasLens

uerrMsg ∷ (Monad m,MonadUCont m,MonadReader r m,HasLens r GError) ⇒ 𝕊 → m a → m a
uerrMsg = ulocalL $ gerrorMsgL ⊚ hasLens

uerrCxt ∷ (Monad m,MonadUCont m,MonadReader r m,HasLens r GError) ⇒ 𝑇A Doc → m a → m a
uerrCxt = ulocalL $ gerrorCxtL ⊚ hasLens
