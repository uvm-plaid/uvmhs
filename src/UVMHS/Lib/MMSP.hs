module UVMHS.Lib.MMSP where

import UVMHS.Core

import UVMHS.Lib.Parser
import UVMHS.Lib.Annotated
import UVMHS.Lib.Substitution

-- MMSP ≈
-- c ⊔ (c ⊓ (c + c(xᶜ…xᶜ)
--   ⋮    ⋮    ⋮
--   ⋮    ⋮    + c(xᵈ…xᵈ))
--   ⋮    ⋮
--   ⋮    ⊓ (c + c(xᶜ…xᶜ)
--   ⋮         ⋮
--   ⋮         + c(xᵈ…xᵈ))
--   ⊔ (c ⊓ (c + c(xᶜ…xᶜ)
--        ⋮    ⋮
--        ⋮    + c(xᵈ…xᵈ))
--        ⋮
--        ⊓ (c + c(xᶜ…xᶜ)
--             ⋮
--             + c(xᵈ…xᵈ))

newtype MMSP = MMSP
  { mmspMaxs ∷ MMSPMaxs
  }
  deriving (Eq,Ord,Show)

data MMSPMaxs = MMSPMaxs
  { mmspMaxsConstant    ∷ ℕ
  , mmspMaxsMins        ∷ 𝑃 MMSPMins
  }
  deriving (Eq,Ord,Show)

data MMSPMins = MMSPMins
  { mmspMinsConstant    ∷ AddTop ℕ   -- non-zero
  , mmspMinsSums        ∷ 𝑃 MMSPSums -- at least one
  }
  deriving (Eq,Ord,Show)

data MMSPSums = MMSPSums
  { mmspSumsConstant    ∷ ℕ
  , mmspSumsPRods       ∷ MMSPProds ⇰ ℕ -- at least one
  }
  deriving (Eq,Ord,Show)

data MMSPProds = MMSPProds
  { mmspProdsExps        ∷ MMSPAtom ⇰ ℕ -- at least one
  }
  deriving (Eq,Ord,Show)

data MMSPAtom =
    Var_MMSPAtom (𝐴 (𝑃 SrcCxt) Name)
  deriving (Eq,Ord,Show)
makePrisms ''MMSPAtom

----------
-- MMSP --
----------

----------------
-- OPERATIONS --
----------------

instance Zero  MMSP where zero = litMMSP zero
instance One   MMSP where one  = litMMSP one
instance Plus  MMSP where (+)  = plusMMSP
instance Times MMSP where (×)  = timesMMSP
instance Pon   MMSP where (^^) = ponMMSP
instance Bot   MMSP where bot  = litMMSP zero
instance Join  MMSP where (⊔)  = joinMMSP
instance Top   MMSP where top  = topMMSP
instance Meet  MMSP where (⊓)  = meetMMSP

instance Additive       MMSP
instance Multiplicative MMSP
instance JoinLattice    MMSP
instance MeetLattice    MMSP

maxsMMSPL ∷ MMSP ⌲ MMSPMaxs
maxsMMSPL = prism MMSP $ Some ∘ mmspMaxs

minsMMSPL ∷ MMSP ⌲ MMSPMins
minsMMSPL  =
  let mk β̇ = MMSPMaxs zero $ single β̇
      vw = \case
        MMSPMaxs a α | a ≡ zero,Some β̇ ← view single𝑃L α → Some β̇
        _ → None
  in prism mk vw ⊚ maxsMMSPL

sumsMMSPL ∷ MMSP ⌲ MMSPSums
sumsMMSPL =
  let mk γ̇ = MMSPMins Top $ single $ γ̇
      vw = \case
        MMSPMins b β | b ≡ Top,Some γ̇ ← view single𝑃L β → Some γ̇
        _ → None
  in prism mk vw ⊚ minsMMSPL

prodsMMSPL ∷ MMSP ⌲ MMSPProds
prodsMMSPL =
  let mk δ̇ = MMSPSums zero $ δ̇ ↦♭ one
      vw = \case
        MMSPSums c γ | c ≡ zero,Some (δ̇ :* d) ← view single𝐷L γ,d ≡ one → Some δ̇
        _ → None
  in prism mk vw ⊚ sumsMMSPL

atomMMSPL ∷ MMSP ⌲ MMSPAtom
atomMMSPL =
  let mk ω = MMSPProds $ ω ↦♭ one
      vw = \case
        MMSPProds δ | Some (ω :* e) ← view single𝐷L δ,e ≡ one → Some ω
        _ → None
  in prism mk vw ⊚ prodsMMSPL

varMMSPL ∷ MMSP ⌲ 𝐴 (𝑃 SrcCxt) Name
varMMSPL = var_MMSPAtomL ⊚ atomMMSPL

litMMSPL ∷ MMSP ⌲ ℕ
litMMSPL =
  let mk n = MMSPMaxs n null
      vw = \case
        MMSPMaxs a α | isEmpty α → Some a
        _ → None
  in prism mk vw ⊚ maxsMMSPL

topMMSPL ∷ MMSP ⌲ ()
topMMSPL =
  let mk () = MMSPMins Top null
      vw = \case
        MMSPMins b β | b ≡ Top,isEmpty β → Some ()
        _ → None
  in prism mk vw ⊚ minsMMSPL

littMMSPL ∷ MMSP ⌲ AddTop ℕ
littMMSPL =
  let mk = \case
        AddTop n → litMMSP n
        Top → topMMSP
      vw η
        | Some n ← view litMMSPL η = Some $ AddTop n
        | Some () ← view topMMSPL η = Some Top
        | otherwise = None
  in prism mk vw

maxsMMSP ∷ MMSPMaxs → MMSP
maxsMMSP = construct maxsMMSPL

minsMMSP ∷ MMSPMins → MMSP
minsMMSP = construct minsMMSPL

sumsMMSP ∷ MMSPSums → MMSP
sumsMMSP = construct sumsMMSPL

prodsMMSP ∷ MMSPProds → MMSP
prodsMMSP = construct prodsMMSPL

atomMMSP ∷ MMSPAtom → MMSP
atomMMSP = construct atomMMSPL

varMMSP ∷ 𝐴 (𝑃 SrcCxt) Name → MMSP
varMMSP = construct varMMSPL

litMMSP ∷ ℕ → MMSP
litMMSP = construct litMMSPL

topMMSP ∷ MMSP
topMMSP = construct topMMSPL ()

joinMMSP ∷ MMSP → MMSP → MMSP
joinMMSP (MMSP α̇₁) (MMSP α̇₂) = MMSP $ joinMaxs α̇₁ α̇₂

meetMMSP ∷ MMSP → MMSP → MMSP
meetMMSP (MMSP α̇₁) (MMSP α̇₂) = MMSP $ meetMaxs α̇₁ α̇₂

plusMMSP ∷ MMSP → MMSP → MMSP
plusMMSP (MMSP α̇₁) (MMSP α̇₂) = MMSP $ plusMaxs α̇₁ α̇₂

timesMMSP ∷ MMSP → MMSP → MMSP
timesMMSP (MMSP α̇₁) (MMSP α̇₂) = MMSP $ timesMaxs α̇₁ α̇₂

ponMMSP ∷ MMSP → ℕ → MMSP
ponMMSP e n = product $ replicate n e

------------------
-- SUBSTITUTION --
------------------

-- gsubstMMSP ∷ (Substy t,Monad m) ⇒ (a → m MMSP) → t a → MMSP → m MMSP
-- gsubstMMSP 𝓋 𝓈 (MMSP α̇) = gsubstMMSPMaxs 𝓋 𝓈 α̇
--
-- gsubstMMSPMaxs ∷ (Substy t,Monad m) ⇒ (a → m MMSP) → t a → MMSPMaxs → m MMSP
-- gsubstMMSPMaxs 𝓋 𝓈 (MMSPMaxs a α) = (litMMSP a ⊔) ^$ gsubstMMSPMaxsMins 𝓋 𝓈 α
--
-- gsubstMMSPMaxsMins ∷ (Substy t,Monad m) ⇒ (a → m MMSP) → t a → 𝑃 MMSPMins → m MMSP
-- gsubstMMSPMaxsMins 𝓋 𝓈 α = joins ^$ mapM (gsubstMMSPMins 𝓋 𝓈) $ iter α
--
-- gsubstMMSPMins ∷ (Substy t,Monad m) ⇒ (a → m MMSP) → t a → MMSPMins → m MMSP
-- gsubstMMSPMins 𝓋 𝓈 (MMSPMins b β) = (elimAddTop top litMMSP b ⊓) ^$ gsubstMMSPMinsSums 𝓋 𝓈 β
--
-- gsubstMMSPMinsSums ∷ (Substy t,Monad m) ⇒ (a → m MMSP) → t a → 𝑃 MMSPSums → m MMSP
-- gsubstMMSPMinsSums 𝓋 𝓈 β = meets ^$ mapM (gsubstMMSPSums 𝓋 𝓈) $ iter β
--
-- gsubstMMSPSums ∷ (Substy t,Monad m) ⇒ (a → m MMSP) → t a → MMSPSums → m MMSP
-- gsubstMMSPSums 𝓋 𝓈 (MMSPSums c γ) = (litMMSP c +) ^$ gsubstMMSPSumsProds 𝓋 𝓈 γ
--
-- gsubstMMSPSumsProds ∷ (Substy t,Monad m) ⇒ (a → m MMSP) → t a → MMSPProds ⇰ ℕ → m MMSP
-- gsubstMMSPSumsProds 𝓋 𝓈 γ = sum ^$ mapMOn (iter γ) $ \ (δ :* d) →
--   (litMMSP d ×) ^$ gsubstMMSPProds 𝓋 𝓈 δ
--
-- gsubstMMSPProds ∷ (Substy t,Monad m) ⇒ (a → m MMSP) → t a → MMSPProds → m MMSP
-- gsubstMMSPProds 𝓋 𝓈 (MMSPProds δ) = product ^$ mapMOn (iter δ) $ \ (ω :* e) →
--   (^^ e) ^$ gsubstMMSPAtom 𝓋 𝓈 ω
--
-- gsubstMMSPAtom ∷ (Substy t,Monad m) ⇒ (a → m MMSP) → t a → MMSPAtom → m MMSP
-- gsubstMMSPAtom 𝓋 𝓈 = \case
--   Var_MMSPAtom (𝐴 𝒸 𝓎) → case 𝓈var 𝓈 𝓎 of
--     Inl 𝓎' → return $ varMMSP $ 𝐴 𝒸 𝓎'
--     Inr (𝓈O :* e) → elim𝑂 return (gsubstMMSP exfalso) 𝓈O *$ 𝓋 e
--
-- instance Substable m () MMSP MMSP where gsubstS 𝓋 𝓈 = gsubstMMSP 𝓋 $ ifNone null $ 𝓈 ⋕? ()

---------------
-- FREE VARS --
---------------

-- fvMMSP ∷ MMSP → 𝑃 𝕐
-- fvMMSP (MMSP α̇) = fvMMSPMaxs α̇
--
-- fvMMSPMaxs ∷ MMSPMaxs → 𝑃 𝕐
-- fvMMSPMaxs (MMSPMaxs _ α) = joins $ map fvMMSPMins $ iter α
--
-- fvMMSPMins ∷ MMSPMins → 𝑃 𝕐
-- fvMMSPMins (MMSPMins _ β) = joins $ map fvMMSPSums $ iter β
--
-- fvMMSPSums ∷ MMSPSums → 𝑃 𝕐
-- fvMMSPSums (MMSPSums _ γ) = joins $ map (fvMMSPProds ∘ fst) $ iter γ
--
-- fvMMSPProds ∷ MMSPProds → 𝑃 𝕐
-- fvMMSPProds (MMSPProds δ) = joins $ map (fvMMSPAtom ∘ fst) $ iter δ
--
-- fvMMSPAtom ∷ MMSPAtom → 𝑃 𝕐
-- fvMMSPAtom = \case
--   Var_MMSPAtom xA → fv $ aval xA

-- instance HasFBV MMSP where fbv = FBV pø ∘ fvMMSP

----------
-- MAXS --
----------

-- Mins --

-- ┌─────┐
-- │α ≡ 0│
-- └─────┘
zeroMaxsMins ∷ 𝑃 MMSPMins
-- β ≡ 0 ≜ ⨆{}
zeroMaxsMins = null

-- ┌─────┐
-- │α ∨̃ α│
-- └─────┘
joinMaxsMins ∷ 𝑃 MMSPMins → 𝑃 MMSPMins → 𝑃 MMSPMins
joinMaxsMins α₁ α₂ = α₁ ∪ α₂

-- ┌─────┐
-- │b ∧̃ α│
-- └─────┘
cmeetMaxsMins ∷ AddTop ℕ → 𝑃 MMSPMins → 𝑃 MMSPMins
-- b ∧̃ α = c ⊓ ⨆{ β | β ∈ α}
--       ≜ ⨆ { b ∧̃ β | β ∈ α}
cmeetMaxsMins b = pow ∘ map (cmeetMins b) ∘ iter

-- ┌─────┐
-- │α ∧̃ α│
-- └─────┘
meetMaxsMins ∷ 𝑃 MMSPMins → 𝑃 MMSPMins → 𝑃 MMSPMins
-- α₁ ∧̃ α₂ = ⨆{ β | β ∈ α₁ } + ⨆{ β | β ∈ α₂ }
--         ≜ ⨆{ β₁ ∧̃ β₂ | β₁ ∈ α₁ , β₂ ∈ α₂}
meetMaxsMins α₁ α₂ = pow $ mapOn (iter α₁ ⧆ iter α₂) $ \ (β₁ :* β₂) → meetMins β₁ β₂

-- ┌─────┐
-- │c +̃ α│
-- └─────┘
cplusMaxsMins ∷ ℕ → 𝑃 MMSPMins → 𝑃 MMSPMins
-- c +̃ α = c + ⨆{ β | β ∈ α}
--       ≜ ⨆ { c +̃ β | β ∈ α}
cplusMaxsMins c = pow ∘ map (cplusMins c) ∘ iter

-- ┌─────┐
-- │α +̃ α│
-- └─────┘
plusMaxsMins ∷ 𝑃 MMSPMins → 𝑃 MMSPMins → 𝑃 MMSPMins
-- α₁ +̃ α₂ = ⨆{ β | β ∈ α₁ } + ⨆{ β | β ∈ α₂ }
--         ≜ ⨆{ β₁ +̃ β₂ | β₁ ∈ α₁ , β₂ ∈ α₂}
plusMaxsMins α₁ α₂ = pow $ mapOn (iter α₁ ⧆ iter α₂) $ \ (β₁ :* β₂) → plusMins β₁ β₂

-- ┌─────┐
-- │d ×̃ α│
-- └─────┘
ctimesMaxsMins ∷ ℕ → 𝑃 MMSPMins → 𝑃 MMSPMins
-- d ×̃ α = d × ⨆{ β | β ∈ α}
--       ≜ ⨆ { d ×̃ β | β ∈ α}
ctimesMaxsMins d = pow ∘ map (ctimesMins d) ∘ iter

-- ┌─────┐
-- │α ×̃ α│
-- └─────┘
timesMaxsMins ∷ 𝑃 MMSPMins → 𝑃 MMSPMins → 𝑃 MMSPMins
-- α₁ ×̃ α₂ = ⨆{ β | β ∈ α₁ } × ⨆{ β | β ∈ α₂ }
--         ≜ ⨆{ β₁ ×̃ β₂ | β₁ ∈ α₁ , β₂ ∈ α₂}
timesMaxsMins α₁ α₂ = pow $ mapOn (iter α₁ ⧆ iter α₂) $ \ (β₁ :* β₂) → timesMins β₁ β₂

-- Maxs --

-- ┌─────┐
-- │α̇ ∨̃ α̇│
-- └─────┘
joinMaxs ∷ MMSPMaxs → MMSPMaxs → MMSPMaxs
--
joinMaxs (MMSPMaxs a₁ α₁) (MMSPMaxs a₂ α₂) = MMSPMaxs (a₁ ⊔ a₂) $ joinMaxsMins α₁ α₂

-- ┌─────┐
-- │α̇ ∧̃ α̇│
-- └─────┘
meetMaxs ∷ MMSPMaxs → MMSPMaxs → MMSPMaxs
-- (a₁ ∧̇ α₁) ∧̃ (a₂ ∧̇ α₂) ≜ (a₁ ⊓ a₂) ∨̇ ((a₁ ∧̃ α₂) ∨̃ (a₂ ∧̃ α₁) ∨̃ (α₁ ∧̃ α₂))
meetMaxs (MMSPMaxs a₁ α₁) (MMSPMaxs a₂ α₂) =
  MMSPMaxs (a₁ + a₂) $ fold zeroMaxsMins joinMaxsMins
    [ cmeetMaxsMins (AddTop a₁) α₂
    , cmeetMaxsMins (AddTop a₂) α₂
    , meetMaxsMins α₁ α₂
    ]

-- ┌─────┐
-- │α̇ +̃ α̇│
-- └─────┘
plusMaxs ∷ MMSPMaxs → MMSPMaxs → MMSPMaxs
-- (a₁ ∧̇ α₁) +̃ (a₂ ∧̇ α₂) ≜ (a₁ + a₂) ∨̇ ((a₁ +̃ α₂) ∨̃ (a₂ +̃ α₁) ∨̃ (α₁ +̃ α₂))
plusMaxs (MMSPMaxs a₁ α₁) (MMSPMaxs a₂ α₂) =
  MMSPMaxs (a₁ + a₂) $ fold zeroMaxsMins plusMaxsMins
    [ cplusMaxsMins a₁ α₂
    , cplusMaxsMins a₂ α₂
    , plusMaxsMins α₁ α₂
    ]

-- ┌─────┐
-- │α̇ ×̃ α̇│
-- └─────┘
timesMaxs ∷ MMSPMaxs → MMSPMaxs → MMSPMaxs
-- (a₁ ∧̇ α₁) ×̃ (a₂ ∧̇ α₂) ≜ (a₁ × a₂) ∨̇ ((a₁ ×̃ α₂) ∨̃ (a₂ ×̃ α₁) ∨̃ (α₁̇ ×̃ α₂))
timesMaxs (MMSPMaxs a₁ α₁) (MMSPMaxs a₂ α₂) =
  MMSPMaxs (a₁ + a₂) $ fold zeroMaxsMins timesMaxsMins
    [ ctimesMaxsMins a₁ α₂
    , ctimesMaxsMins a₂ α₂
    , timesMaxsMins α₁ α₂
    ]

----------
-- MINS --
----------

-- Sums --

-- ┌─────┐
-- │β ≡ ∞│
-- └─────┘
infMinsSums ∷ 𝑃 MMSPSums
-- β ≡ ∞ ≜ ⨅{}
infMinsSums = null

-- ┌─────┐
-- │β ∧̃ β│
-- └─────┘
meetMinsSums ∷ 𝑃 MMSPSums → 𝑃 MMSPSums → 𝑃 MMSPSums
-- β₁ ∧̃ β₂ = ⨅{ γ | γ ∈ β₁ } ⊓ ⨅{ γ | γ ∈ β₂ }
--         ≜ ⨅( { γ | γ ∈ β₁ }
--            ∪ { γ | γ ∈ β₂ } )
meetMinsSums xs ys = xs ∪ ys

-- ┌─────┐
-- │c +̃ β│
-- └─────┘
cplusMinsSums ∷ ℕ → 𝑃 MMSPSums → 𝑃 MMSPSums
-- c +̃ β = c + ⨅{ γ | γ ∈ β}
--       ≜ ⨅ { c +̃ γ | γ ∈ β}
cplusMinsSums c = pow ∘ map (cplusSums c) ∘ iter

-- ┌─────┐
-- │β +̃ β│
-- └─────┘
plusMinsSums ∷ 𝑃 MMSPSums → 𝑃 MMSPSums → 𝑃 MMSPSums
-- β₁ +̃ β₂ = ⨅{ γ | γ ∈ β₁ } + ⨅{ γ | γ ∈ β₂ }
--         ≜ ⨅{ γ₁ +̃ γ₂ | γ₁ ∈ β₁ , γ₂ ∈ β₂}
plusMinsSums β₁ β₂ = pow $ mapOn (iter β₁ ⧆ iter β₂) $ \ (γ₁ :* γ₂) → plusSums γ₁ γ₂

-- ┌─────┐
-- │d ×̃ β│
-- └─────┘
ctimesMinsSums ∷ ℕ → 𝑃 MMSPSums → 𝑃 MMSPSums
-- d ×̃ β = d × ⨅{ γ | γ ∈ β}
--       ≜ ⨅ { d ×̃ γ | γ ∈ β}
ctimesMinsSums c = pow ∘ map (cplusSums c) ∘ iter

-- ┌─────┐
-- │β ×̃ β│
-- └─────┘
timesMinsSums ∷ 𝑃 MMSPSums → 𝑃 MMSPSums → 𝑃 MMSPSums
-- β₁ ×̃ β₂ = ⨅{ γ | γ ∈ β₁ } × ⨅{ γ | γ ∈ β₂ }
--         ≜ ⨅{ γ₁ ×̃ γ₂ | γ₁ ∈ β₁ , γ₂ ∈ β₂}
timesMinsSums β₁ β₂ = pow $ mapOn (iter β₁ ⧆ iter β₂) $ \ (γ₁ :* γ₂) → timesSums γ₁ γ₂

-- Mins --

-- ┌─────┐
-- │b ∧̃ β̇│
-- └─────┘
cmeetMins ∷ AddTop ℕ → MMSPMins → MMSPMins
-- b₀ ⊓ (b ∧̇ β) ≜ (b₀ ⊓ b) ∧̇ β
cmeetMins b₀ (MMSPMins b β) = MMSPMins (b₀ ⊓ b) β

-- ┌─────┐
-- │β̇ ∧̃ β̇│
-- └─────┘
meetMins ∷ MMSPMins → MMSPMins → MMSPMins
-- (b₁ ∧̇  β₁) ⊓ (b₂ ∧̇  β₂) ≜ (b₁ ⊓ b₂) ∧̇ (β₁ ∧̃ β₂)
meetMins (MMSPMins b₁ β₁) (MMSPMins b₂ β₂) = MMSPMins (b₁ ⊓ b₂) $ meetMinsSums β₁ β₂

-- ┌─────┐
-- │c +̃ β̇│
-- └─────┘
cplusMins ∷ ℕ → MMSPMins → MMSPMins
-- c +̃ (b ∧̇ β) ≜ (c + b) ∧̇ (c +̃ β)
cplusMins c (MMSPMins b β) = MMSPMins (map (+ c) b) $ cplusMinsSums c β

-- ┌─────┐
-- │β̇ +̃ β̇│
-- └─────┘
plusMins ∷ MMSPMins → MMSPMins → MMSPMins
-- (b₁ ∧̇ β₁) +̃ (b₂ ∧̇ β₂) ≜ (b₁ + b₂) ∧̇ ((b₁ +̃ β₂) ∧̃ (b₂ +̃ β₁) ∧̃ (β₁̇ +̃ β₂))
plusMins (MMSPMins b₁ β₁) (MMSPMins b₂ β₂) =
  MMSPMins (b₁ + b₂) $ fold infMinsSums meetMinsSums
    [ flip (elimAddTop null) b₁ $ \ b₁' → cplusMinsSums b₁' β₂
    , flip (elimAddTop null) b₂ $ \ b₂' → cplusMinsSums b₂' β₂
    , plusMinsSums β₁ β₂
    ]

-- ┌─────┐
-- │c ×̃ β̇│
-- └─────┘
ctimesMins ∷ ℕ → MMSPMins → MMSPMins
-- c ×̃ (b ∧̇ β) ≜ (c × b) ∧̇ (c ×̃ β)
ctimesMins c (MMSPMins b β) = MMSPMins (AddTop c × b) $ ctimesMinsSums c β

-- ┌─────┐
-- │β̇ ×̃ β̇│
-- └─────┘
timesMins ∷ MMSPMins → MMSPMins → MMSPMins
-- (b₁ ∧̇ β₁) ×̃ (b₂ ∧̇ β₂) ≜ (b₁ × b₂) ∧̇ ((b₁ ×̃ β₂) ∧̃ (b₂ ×̃ β₁) ∧̃ (β₁̇ ×̃ β₂))
timesMins (MMSPMins b₁ β₁) (MMSPMins b₂ β₂) =
  MMSPMins (b₁ × b₂) $ fold infMinsSums meetMinsSums
    [ flip (elimAddTop null) b₁ $ \ b₁' → ctimesMinsSums b₁' β₂
    , flip (elimAddTop null) b₂ $ \ b₂' → ctimesMinsSums b₂' β₂
    , timesMinsSums β₁ β₂
    ]

----------
-- SUMS --
----------

-- Prods --

-- ┌─────┐
-- │γ ≡ 0│
-- └─────┘
zeroSumsProds ∷ MMSPProds ⇰ ℕ
-- γ ≡ 0 ≜ ∑{}
zeroSumsProds = null

-- ┌─────┐
-- │γ +̃ γ│
-- └─────┘
plusSumsProds ∷ MMSPProds ⇰ ℕ → MMSPProds ⇰ ℕ → MMSPProds ⇰ ℕ
-- γ₁ +̃ γ₂ = ∑{ d×̇δ | d×̇δ ∈ γ₁} + ∑{ d×̇δ | d×̇δ ∈ γ₂ }
--         ≜ ∑( { d×̇δ | d×̇δ ∈ γ₁ , δ ∉ dom(γ₂) }
--            ∪ { d×̇δ | d×̇δ ∈ γ₂ , δ ∉ dom(γ₁) }
--            ∪ { (d₁+d₂)×̇δ | d₁×̇δ ∈ γ₁ , d₂×̇δ ∈ γ₂ } )
plusSumsProds γ₁ γ₂ = γ₁ + γ₂

-- ┌─────┐
-- │d ×̃ γ│
-- └─────┘
ctimesSumsProds ∷ ℕ → MMSPProds ⇰ ℕ → MMSPProds ⇰ ℕ
-- d₀ ×̃ γ ≜ d₀ × ∑{ d×̇δ | d×̇δ ∈ γ }
--        ≜ ∑{ d₀d×̇δ | d×̇δ ∈ γ }
ctimesSumsProds d γ = map (× d) γ

-- ┌─────┐
-- │γ ×̃ γ│
-- └─────┘
timesSumsProds ∷ MMSPProds ⇰ ℕ → MMSPProds ⇰ ℕ → MMSPProds ⇰ ℕ
-- γ₁ ×̃ γ₂ = ∑{ d×̇δ | d×̇δ ∈ γ₁} × ∑{ d×̇δ | d×̇δ ∈ γ₂ }
--         ≜ ∑{ d₁d₂×̇(δ₁×̃δ₂) | d₁×̇δ₁ ∈ γ₁ , d₂×̇δ₂ ∈ γ₂ }
timesSumsProds γ₁ γ₂ = assoc $ mapOn (iter γ₁ ⧆ iter γ₂) $ \ ((δ₁ :* d₁) :* (δ₂ :* d₂)) →
  timesProds δ₁ δ₂ :* (d₁ × d₂)

-- Sums --

-- ┌─────┐
-- │c +̃ γ̇│
-- └─────┘
cplusSums ∷ ℕ → MMSPSums → MMSPSums
-- c₀ +̃ (c +̇ γ) ≜ (c₀ + c) +̇ γ
cplusSums c₀ (MMSPSums c γ) = MMSPSums (c₀ + c) γ

-- ┌─────┐
-- │γ̇ +̃ γ̇│
-- └─────┘
plusSums ∷ MMSPSums → MMSPSums → MMSPSums
-- c₁ +̇ γ₁ +̃ c₂ +̇ γ₂ ≜ (c₁ + c₂) +̇ (γ₁ +̃ γ₂)
plusSums (MMSPSums c₁ γ₁) (MMSPSums c₂ γ₂) = MMSPSums (c₁ + c₂) $ plusSumsProds γ₁ γ₂

-- ┌─────┐
-- │γ̇ ×̃ γ̇│
-- └─────┘
timesSums ∷ MMSPSums → MMSPSums → MMSPSums
-- (c₁ +̇ γ₁) ×̃ (c₂ +̇ γ₂) ≜ (c₁ × c₂) +̇ ((c₁ ×̃ γ₂) +̃ (c₂ ×̃ γ₁) +̃ (γ₁ ×̃ γ₂))
timesSums (MMSPSums c₁ γ₁) (MMSPSums c₂ γ₂) =
  MMSPSums (c₁ × c₂) $ fold zeroSumsProds plusSumsProds
    [ ctimesSumsProds c₁ γ₂
    , ctimesSumsProds c₂ γ₁
    , timesSumsProds γ₁ γ₂
    ]

-----------
-- PRODS --
-----------

-- ┌─────┐
-- │δ ×̃ δ│
-- └─────┘
timesProds ∷ MMSPProds → MMSPProds → MMSPProds
-- δ₁ +̃ δ₂ = ∏{ ω^̇e | ω^̇e ∈ δ₁} × ∏{ ω^̇e | ω^̇e ∈ δ₂ }
--         ≜ ∏( { ω^̇e | ω^̇e ∈ δ₁ , ω ∉ dom(δ₂) }
--            ∪ { ω^̇e | ω^̇e ∈ δ₂ , ω ∉ dom(δ₁) }
--            ∪ { ω^̇(e₁+e₂) | ω^̇e₁ ∈ δ₁ , ω^̇e₂ ∈ δ₂ } )
timesProds (MMSPProds δ₁) (MMSPProds δ₂) = MMSPProds $ δ₁ + δ₂
