module UVMHS.Lib.Parser.Loc where

import UVMHS.Core
import UVMHS.Lib.Pretty

data Loc = Loc
  { locPos ∷ ℕ
  , locRow ∷ ℕ
  , locCol ∷ ℕ
  }
makeLenses ''Loc
makePrettyRecord ''Loc

instance Eq Loc where (==) = (==) `on` locPos
instance Ord Loc where compare = (⋚) `on` locPos

instance Bot Loc where bot = Loc bot bot bot
instance Join Loc where l₁ ⊔ l₂ = case locPos l₁ ≥ locPos l₂ of {True → l₁;False → l₂}
instance JoinLattice Loc
instance Meet Loc where l₁ ⊓ l₂ = case locPos l₁ ≤ locPos l₂ of {True → l₁;False → l₂}

bumpRow ∷ Loc → Loc
bumpRow (Loc pos row _) = Loc (pos + 1) (row + 1) 0

bumpCol ∷ Loc → Loc
bumpCol (Loc pos row col) = Loc (pos + 1) row (col + 1)

-- # LocRange

data LocRange = LocRange
  { locRangeBegin ∷ Loc
  , locRangeEnd ∷ Loc
  } deriving (Eq, Ord)
makeLenses ''LocRange
makePrettyUnion ''LocRange

instance Join LocRange where LocRange b₁ e₁ ⊔ LocRange b₂ e₂ = LocRange (b₁ ⊓ b₂) (e₁ ⊔ e₂)

locRange₀ ∷ LocRange
locRange₀ = LocRange bot bot
