module UVMHS.Lib.Parser.Loc where

import UVMHS.Core

import UVMHS.Lib.Pretty

data Loc = Loc
  { locPos ∷ ℕ64 ∧ ℕ64
  , locRow ∷ ℕ64
  , locCol ∷ ℕ64
  } deriving (Show)
makeLenses ''Loc
makePrettyRecord ''Loc

instance Eq Loc where (==) = (==) `on` locPos
instance Ord Loc where compare = (⋚) `on` locPos

instance Bot Loc where bot = Loc bot bot bot
instance Join Loc where l₁ ⊔ l₂ = if locPos l₁ ≥ locPos l₂ then l₁ else l₂
instance JoinLattice Loc
instance Meet Loc where l₁ ⊓ l₂ = if locPos l₁ ≤ locPos l₂ then l₁ else l₂

bumpRow₁ ∷ Loc → Loc
bumpRow₁ (Loc (pos₁ :* pos₂) row _) = Loc ((pos₁ + one) :* pos₂) (row + one) zero

bumpCol₁ ∷ Loc → Loc
bumpCol₁ (Loc (pos₁ :* pos₂) row col) = Loc ((pos₁ + one) :* pos₂) row (col + one)

bumpCol₂ ∷ Loc → Loc
bumpCol₂ (Loc (pos₁ :* pos₂) row col) = Loc (pos₁ :* (pos₂ + one)) row (col + one)

-- # LocRange

data LocRange = LocRange
  { locRangeBegin ∷ AddBT Loc
  , locRangeEnd ∷ AddBT Loc
  } deriving (Eq,Ord,Show)
makeLenses ''LocRange
makePrettyUnion ''LocRange

bumpColEnd₂ ∷ LocRange → LocRange
bumpColEnd₂ (LocRange b e) = LocRange b (bumpCol₂ ^$ e)

instance Bot LocRange where bot = LocRange top bot
instance Top LocRange where top = LocRange bot top

instance Join LocRange where LocRange b₁ e₁ ⊔ LocRange b₂ e₂ = LocRange (b₁ ⊓ b₂) (e₁ ⊔ e₂)
instance Meet LocRange where LocRange b₁ e₁ ⊓ LocRange b₂ e₂ = LocRange (b₁ ⊔ b₂) (e₁ ⊓ e₂)

locRange₀ ∷ LocRange
locRange₀ = LocRange bot bot
