module UVMHS.Lib.THLiftInstances where

import UVMHS.Core
import UVMHS.Lib.Annotated
import UVMHS.Lib.Parser
import UVMHS.Lib.Pretty
import UVMHS.Lib.TreeAnnote
import UVMHS.Lib.Window

import Instances.TH.Lift ()

import qualified Language.Haskell.TH.Syntax as TH

deriving instance (TH.Lift a) ⇒ TH.Lift (𝑃 a)
deriving instance (TH.Lift k,TH.Lift v) ⇒ TH.Lift (k ⇰ v)
deriving instance (TH.Lift a) ⇒ TH.Lift (𝕍 a)
deriving instance (TH.Lift a) ⇒ TH.Lift (AddBT a)
deriving instance (TH.Lift i,TH.Lift a) ⇒ TH.Lift (WindowL i a)
deriving instance (TH.Lift i,TH.Lift a) ⇒ TH.Lift (WindowR i a)
deriving instance TH.Lift SrcCxt
deriving instance TH.Lift LocRange
deriving instance (TH.Lift 𝒸,TH.Lift a) ⇒ TH.Lift (𝐴 𝒸 a)
deriving instance TH.Lift Loc
instance (TH.Lift a) ⇒ TH.Lift (() → a) where
  liftTyped ∷ ∀ m. TH.Quote m ⇒ (() → a) → TH.Code m (() → a)
  liftTyped f = 
    let x = f ()
    in [|| \ () → x ||]
deriving instance (TH.Lift a,TH.Lift b) ⇒ TH.Lift (a ∧ b)
deriving instance TH.Lift Annotation
deriving instance TH.Lift Formats
deriving instance TH.Lift Color
deriving instance TH.Lift Color3Bit
deriving instance TH.Lift ChunkI
deriving instance (TH.Lift a) ⇒ TH.Lift (𝑂 a)
deriving instance (TH.Lift i,TH.Lift a) ⇒ TH.Lift (𝑇 i a)

instance (TH.Lift i,TH.Lift a) ⇒ TH.Lift (𝑇V i a) where
  liftTyped t = do
    let t' = fold𝑇VWith single annote t
    [|| fold𝑇With single annote t' ||]

instance (TH.Lift a) ⇒ TH.Lift (𝐼 a) where
  liftTyped xs = do
    let xs' = list xs
    [|| iter xs' ||]

instance TH.Lift Doc where
  liftTyped d = do
    let d' = ppBake d
    [|| ppEmbed d' ||]

