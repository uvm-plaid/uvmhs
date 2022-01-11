module UVMHS.Lib.Errors where

import UVMHS.Core
import UVMHS.Lib.Parser
import UVMHS.Lib.Pretty
import UVMHS.Lib.TreeNested

oops ∷ (Monad m,MonadReader r m,HasLens r e,MonadError e m) ⇒ m a
oops = throw *$ askL hasLens

errSrc ∷ ∀ r e m a. (Monad m,MonadReader r m,HasLens r e,MonadError e m,HasLens e (𝑂 SrcCxt)) 
       ⇒ 𝑂 SrcCxt → m a → m a
errSrc c = localL ((hasLens ∷ e ⟢ 𝑂 SrcCxt) ⊚ (hasLens ∷ r ⟢ e)) c

errMsg ∷ ∀ r e m a. (Monad m,MonadReader r m,HasLens r e,MonadError e m,HasLens e 𝕊) 
       ⇒ 𝕊 → m a → m a
errMsg x = localL ((hasLens ∷ e ⟢ 𝕊) ⊚ (hasLens ∷ r ⟢ e)) x

errCxt ∷ ∀ r e m a. (Monad m,MonadReader r m,HasLens r e,MonadError e m,HasLens e (𝑇A Doc)) 
       ⇒ 𝑇A Doc → m a → m a
errCxt c = mapEnvL ((hasLens ∷ e ⟢ 𝑇A Doc) ⊚ (hasLens ∷ r ⟢ e)) (▷ c)
