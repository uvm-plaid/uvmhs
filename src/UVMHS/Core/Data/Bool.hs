module UVMHS.Core.Data.Bool where

import UVMHS.Core.Init
import UVMHS.Core.Classes

instance Null 𝔹 where null = False
instance Append 𝔹 where (⧺) = (⩔)
instance Monoid 𝔹

instance POrd 𝔹 where
  False ⊑ _      = True
  _     ⊑ True   = True
  True  ⊑ False = False

instance Bot 𝔹 where bot = False
instance Join 𝔹 where (⊔) = (⩔)
instance JoinLattice 𝔹
instance Top 𝔹 where top = True
instance Meet 𝔹 where (⊓) = (⩓)
instance MeetLattice 𝔹
instance Lattice 𝔹
instance Dual 𝔹 where dual = not

instance Zero 𝔹 where zero = False
instance Plus 𝔹 where (+) = (⊔)
instance Additive 𝔹
instance One 𝔹 where one = True
instance Times 𝔹 where (×) = (⊓)
instance Multiplicative 𝔹
