module UVMHS.Lib.Errors where

import UVMHS.Core
import UVMHS.Lib.Parser
import UVMHS.Lib.Pretty
import UVMHS.Lib.TreeNested

oops ∷ (Monad m,MonadReader r m,HasLens r e,MonadError e m) ⇒ m a
oops = throw *$ askL hasLens

errSrc ∷ ∀ r e m a. (Monad m,MonadReader r m,HasLens r e,MonadError e m,HasLens e (𝑂 SrcCxt)) ⇒ 𝑂 SrcCxt → m a → m a
errSrc = localL $ (hasLens ∷ e ⟢ 𝑂 SrcCxt) ⊚ (hasLens ∷ r ⟢ e)

errSrcU ∷ ∀ r e m a. (Monad m,MonadUCont m,MonadReader r m,HasLens r e,MonadError e m,HasLens e (𝑂 SrcCxt)) ⇒ 𝑂 SrcCxt → m a → m a
errSrcU = ulocalL $ (hasLens ∷ e ⟢ 𝑂 SrcCxt) ⊚ (hasLens ∷ r ⟢ e)

errMsg ∷ ∀ r e m a. (Monad m,MonadReader r m,HasLens r e,MonadError e m,HasLens e 𝕊) ⇒ 𝕊 → m a → m a
errMsg = localL $ (hasLens ∷ e ⟢ 𝕊) ⊚ (hasLens ∷ r ⟢ e)

errMsgU ∷ ∀ r e m a. (Monad m,MonadUCont m,MonadReader r m,HasLens r e,MonadError e m,HasLens e 𝕊) ⇒ 𝕊 → m a → m a
errMsgU = ulocalL $ (hasLens ∷ e ⟢ 𝕊) ⊚ (hasLens ∷ r ⟢ e)

errCxt ∷ ∀ r e m a. (Monad m,MonadReader r m,HasLens r e,MonadError e m,HasLens e (𝑇A Doc)) ⇒ 𝑇A Doc → m a → m a
errCxt = localL $ (hasLens ∷ e ⟢ 𝑇A Doc) ⊚ (hasLens ∷ r ⟢ e)

errCxtU ∷ ∀ r e m a. (Monad m,MonadUCont m,MonadReader r m,HasLens r e,MonadError e m,HasLens e (𝑇A Doc)) ⇒ 𝑇A Doc → m a → m a
errCxtU = ulocalL $ (hasLens ∷ e ⟢ 𝑇A Doc) ⊚ (hasLens ∷ r ⟢ e)

errCxtExt ∷ ∀ r e m a. (Monad m,MonadReader r m,HasLens r e,MonadError e m,HasLens e (𝑇A Doc)) ⇒ 𝑇A Doc → m a → m a
errCxtExt c = mapEnvL ((hasLens ∷ e ⟢ 𝑇A Doc) ⊚ (hasLens ∷ r ⟢ e)) (▷ c)

errCxtExtU ∷ ∀ r e m a. (Monad m,MonadUCont m,MonadReader r m,HasLens r e,MonadError e m,HasLens e (𝑇A Doc)) ⇒ 𝑇A Doc → m a → m a
errCxtExtU c = umapEnvL ((hasLens ∷ e ⟢ 𝑇A Doc) ⊚ (hasLens ∷ r ⟢ e)) (▷ c)
