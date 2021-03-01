module UVMHS.Lib.MMSP where

import UVMHS.Core

import UVMHS.Lib.Variables

instance (Plus a) ⇒ Plus (AddTop a) where
  Top + _ = Top
  _ + Top = Top
  AddTop x + AddTop y = AddTop $ x + y

instance (Times a) ⇒ Times (AddTop a) where
  Top × _ = Top
  _ × Top = Top
  AddTop x × AddTop y = AddTop $ x × y

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
  { mmspMaxsBindingInfo ∷ FV
  , mmspMaxsConstant    ∷ ℕ
  , mmspMaxsMins        ∷ 𝑃 MMSPMins
  }
  deriving (Eq,Ord,Show)

data MMSPMins = MMSPMins
  { mmspMinsBindingInfo ∷ FV
  , mmspMinsConstant    ∷ AddTop ℕ    -- non-zero
  , mmspMinsSums        ∷ 𝑃 MMSPSums -- at least one
  }
  deriving (Eq,Ord,Show)

data MMSPSums = MMSPSums
  { mmspSumsBindingInfo ∷ FV
  , mmspSumsConstant    ∷ ℕ
  , mmspSumsPRods       ∷ MMSPProds ⇰ ℕ -- at least one
  }
  deriving (Eq,Ord,Show)

data MMSPProds = MMSPProds
  { mmspProdsBindingInfo ∷ FV
  , mmspProdsExps        ∷ MMSPAtom ⇰ ℕ -- at least one
  }
  deriving (Eq,Ord,Show)

data MMSPAtom = 
    Var_MMSP 𝕏
  | Meta_MMSP 𝕏
  deriving (Eq,Ord,Show)

----------
-- MMSP --
----------

----------------
-- OPERATIONS --
----------------

instance Zero MMSP where zero = litMMSP zero
instance One MMSP where one = litMMSP one
instance Plus MMSP where (+) = plusMMSP
instance Times MMSP where (×) = timesMMSP
instance Pon MMSP where (^^) = ponMMSP
instance Bot MMSP where bot = litMMSP zero
instance Join MMSP where (⊔) = joinMMSP
instance Top MMSP where top = topMMSP
instance Meet MMSP where (⊓) = meetMMSP

instance Additive MMSP
instance Multiplicative MMSP
instance JoinLattice MMSP
instance MeetLattice MMSP

maxsMMSPL ∷ MMSP ⌲ MMSPMaxs
maxsMMSPL = prism MMSP $ Some ∘ mmspMaxs

minsMMSPL ∷ MMSP ⌲ MMSPMins
minsMMSPL  = 
  let mk β̇ = MMSPMaxs (mmspMinsBindingInfo β̇) zero $ single β̇
      vw = \case
        MMSPMaxs _𝓍 a α | a ≡ zero,Some β̇ ← view single𝑃L α → Some β̇
        _ → None
  in prism mk vw ⊚ maxsMMSPL

sumsMMSPL ∷ MMSP ⌲ MMSPSums
sumsMMSPL = 
  let mk γ̇ = MMSPMins (mmspSumsBindingInfo γ̇) Top $ single $ γ̇
      vw = \case
        MMSPMins _𝓍 b β | b ≡ Top,Some γ̇ ← view single𝑃L β → Some γ̇
        _ → None
  in prism mk vw ⊚ minsMMSPL

prodsMMSPL ∷ MMSP ⌲ MMSPProds
prodsMMSPL = 
  let mk δ̇ = MMSPSums (mmspProdsBindingInfo δ̇) zero $ δ̇ ↦ one
      vw = \case
        MMSPSums _𝓍 c γ | c ≡ zero,Some (δ̇ :* d) ← view single𝐷L γ,d ≡ one → Some δ̇
        _ → None
  in prism mk vw ⊚ sumsMMSPL

atomMMSPL ∷ MMSP ⌲ MMSPAtom
atomMMSPL =
  let mk ω =
        let 𝓍 = case ω of
              Var_MMSP x → fvLexi $ single x
              Meta_MMSP χ → fvMeta $ single χ
        in MMSPProds 𝓍 $ ω ↦ one
      vw = \case
        MMSPProds _𝓍 δ | Some (ω :* e) ← view single𝐷L δ,e ≡ one → Some ω
        _ → None
  in prism mk vw ⊚ prodsMMSPL

litMMSPL ∷ MMSP ⌲ ℕ
litMMSPL = 
  let mk n = MMSPMaxs bot n null
      vw = \case
        MMSPMaxs _𝓍 a α | isEmpty α → Some a
        _ → None
  in prism mk vw ⊚ maxsMMSPL

topMMSPL ∷ MMSP ⌲ ()
topMMSPL = 
  let mk () = MMSPMins bot Top null
      vw = \case
        MMSPMins _𝓍 b β | b ≡ Top,isEmpty β → Some ()
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
ponMMSP e n = applyN n one (× e)

---------------
-- FREE VARS --
---------------

freeVarsMMSP ∷ MMSP → FV
freeVarsMMSP = mmspMaxsBindingInfo  ∘ mmspMaxs

------------------
-- SUBSTITUTION --
------------------

substMMSP ∷ (Monad m,Ord a,HasFV fv a) ⇒ (a → m MMSP) → Sub fv a → MMSP → m MMSP
substMMSP 𝒸 𝓈 (MMSP α̇) = substMaxs 𝒸 𝓈 α̇

substMaxs ∷ (Monad m,Ord a,HasFV fv a) ⇒ (a → m MMSP) → Sub fv a → MMSPMaxs → m MMSP
substMaxs 𝒸 𝓈 η@(MMSPMaxs 𝓍 a α) = do
  let 𝓈' = subRestrict 𝓍 𝓈
  if isEmpty 𝓈'
  then return $ maxsMMSP η
  else (⊔) (litMMSP a) ^$ substMaxsMins 𝒸 𝓈' α

substMaxsMins ∷ (Monad m,Ord a,HasFV fv a) ⇒ (a → m MMSP) → Sub fv a → 𝑃 MMSPMins → m MMSP
substMaxsMins 𝒸 𝓈 α = joins ^$ mapM (substMins 𝒸 𝓈) $ iter α

substMins ∷ (Monad m,Ord a,HasFV fv a) ⇒ (a → m MMSP) → Sub fv a → MMSPMins → m MMSP
substMins 𝒸 𝓈 η@(MMSPMins 𝓍 b β) = do
  let 𝓈' = subRestrict 𝓍 𝓈
  if isEmpty 𝓈'
  then return $ minsMMSP η
  else (⊓) (elimAddTop top litMMSP b) ^$ substMinsSums 𝒸 𝓈' β

substMinsSums ∷ (Monad m,Ord a,HasFV fv a) ⇒ (a → m MMSP) → Sub fv a → 𝑃 MMSPSums → m MMSP
substMinsSums 𝒸 𝓈 β = meets ^$ mapM (substSums 𝒸 𝓈) $ iter β

substSums ∷ (Monad m,Ord a,HasFV fv a) ⇒ (a → m MMSP) → Sub fv a → MMSPSums → m MMSP
substSums 𝒸 𝓈 η@(MMSPSums 𝓍 c γ) = do
  let 𝓈' = subRestrict 𝓍 𝓈
  if isEmpty 𝓈'
  then return $ sumsMMSP η
  else (+) (litMMSP c) ^$ substSumsProds 𝒸 𝓈' γ

substSumsProds ∷ (Monad m,Ord a,HasFV fv a) ⇒ (a → m MMSP) → Sub fv a → MMSPProds ⇰ ℕ → m MMSP
substSumsProds 𝒸 𝓈 γ = sum ^$ mapMOn (iter γ) $ \ (δ :* d) → (litMMSP d ×) ^$ substProds 𝒸 𝓈 δ

substProds ∷ (Monad m,Ord a,HasFV fv a) ⇒ (a → m MMSP) → Sub fv a → MMSPProds → m MMSP
substProds 𝒸 𝓈 η@(MMSPProds 𝓍 δ) = do
  let 𝓈' = subRestrict 𝓍 𝓈
  if isEmpty 𝓈'
  then return $ prodsMMSP η
  else product ^$ mapMOn (iter δ) $ \ (ω :* e) → do
    ω' ← substAtom 𝒸 𝓈' ω
    return $ ω' ^^ e

substAtom ∷ (Monad m,Ord a) ⇒ (a → m MMSP) → Sub fv a → MMSPAtom → m MMSP
substAtom 𝒸 𝓈 = \case
  Var_MMSP x → case subLexis 𝓈 ⋕? x of
    None → return $ atomMMSP $ Var_MMSP x
    Some e → 𝒸 e
  Meta_MMSP χ → case subMetas 𝓈 ⋕? χ of
    None → return $ atomMMSP $ Meta_MMSP χ
    Some e → 𝒸 e

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
joinMaxs (MMSPMaxs 𝓍₁ a₁ α₁) (MMSPMaxs 𝓍₂ a₂ α₂) = MMSPMaxs (𝓍₁ ⊔ 𝓍₂) (a₁ ⊔ a₂) $ joinMaxsMins α₁ α₂

-- ┌─────┐
-- │α̇ ∧̃ α̇│
-- └─────┘
meetMaxs ∷ MMSPMaxs → MMSPMaxs → MMSPMaxs
-- (a₁ ∧̇ α₁) ∧̃ (a₂ ∧̇ α₂) ≜ (a₁ ⊓ a₂) ∨̇ ((a₁ ∧̃ α₂) ∨̃ (a₂ ∧̃ α₁) ∨̃ (α₁ ∧̃ α₂))
meetMaxs (MMSPMaxs 𝓍₁ a₁ α₁) (MMSPMaxs 𝓍₂ a₂ α₂) = 
  MMSPMaxs (𝓍₁ ⊔ 𝓍₂) (a₁ + a₂) $ fold zeroMaxsMins joinMaxsMins
    [ cmeetMaxsMins (AddTop a₁) α₂
    , cmeetMaxsMins (AddTop a₂) α₂
    , meetMaxsMins α₁ α₂
    ]

-- ┌─────┐
-- │α̇ +̃ α̇│
-- └─────┘
plusMaxs ∷ MMSPMaxs → MMSPMaxs → MMSPMaxs
-- (a₁ ∧̇ α₁) +̃ (a₂ ∧̇ α₂) ≜ (a₁ + a₂) ∨̇ ((a₁ +̃ α₂) ∨̃ (a₂ +̃ α₁) ∨̃ (α₁ +̃ α₂))
plusMaxs (MMSPMaxs 𝓍₁ a₁ α₁) (MMSPMaxs 𝓍₂ a₂ α₂) = 
  MMSPMaxs (𝓍₁ ⊔ 𝓍₂) (a₁ + a₂) $ fold zeroMaxsMins plusMaxsMins
    [ cplusMaxsMins a₁ α₂
    , cplusMaxsMins a₂ α₂
    , plusMaxsMins α₁ α₂
    ]

-- ┌─────┐
-- │α̇ ×̃ α̇│
-- └─────┘
timesMaxs ∷ MMSPMaxs → MMSPMaxs → MMSPMaxs
-- (a₁ ∧̇ α₁) ×̃ (a₂ ∧̇ α₂) ≜ (a₁ × a₂) ∨̇ ((a₁ ×̃ α₂) ∨̃ (a₂ ×̃ α₁) ∨̃ (α₁̇ ×̃ α₂))
timesMaxs (MMSPMaxs 𝓍₁ a₁ α₁) (MMSPMaxs 𝓍₂ a₂ α₂) = 
  MMSPMaxs (𝓍₁ ⊔ 𝓍₂) (a₁ + a₂) $ fold zeroMaxsMins timesMaxsMins
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
cmeetMins b₀ (MMSPMins 𝓍 b β) = MMSPMins 𝓍 (b₀ ⊓ b) β

-- ┌─────┐
-- │β̇ ∧̃ β̇│
-- └─────┘
meetMins ∷ MMSPMins → MMSPMins → MMSPMins
-- (b₁ ∧̇  β₁) ⊓ (b₂ ∧̇  β₂) ≜ (b₁ ⊓ b₂) ∧̇ (β₁ ∧̃ β₂)
meetMins (MMSPMins 𝓍₁ b₁ β₁) (MMSPMins 𝓍₂ b₂ β₂) = MMSPMins (𝓍₁ ⊔ 𝓍₂) (b₁ ⊓ b₂) $ meetMinsSums β₁ β₂

-- ┌─────┐
-- │c +̃ β̇│
-- └─────┘
cplusMins ∷ ℕ → MMSPMins → MMSPMins
-- c +̃ (b ∧̇ β) ≜ (c + b) ∧̇ (c +̃ β)
cplusMins c (MMSPMins 𝓍 b β) = MMSPMins 𝓍 (map (c +) b) $ cplusMinsSums c β

-- ┌─────┐
-- │β̇ +̃ β̇│
-- └─────┘
plusMins ∷ MMSPMins → MMSPMins → MMSPMins
-- (b₁ ∧̇ β₁) +̃ (b₂ ∧̇ β₂) ≜ (b₁ + b₂) ∧̇ ((b₁ +̃ β₂) ∧̃ (b₂ +̃ β₁) ∧̃ (β₁̇ +̃ β₂))
plusMins (MMSPMins 𝓍₁ b₁ β₁) (MMSPMins 𝓍₂ b₂ β₂) = 
  MMSPMins (𝓍₁ ⊔ 𝓍₂) (b₁ + b₂) $ fold infMinsSums meetMinsSums
    [ flip (elimAddTop null) b₁ $ \ b₁' → cplusMinsSums b₁' β₂
    , flip (elimAddTop null) b₂ $ \ b₂' → cplusMinsSums b₂' β₂
    , plusMinsSums β₁ β₂
    ]

-- ┌─────┐
-- │c ×̃ β̇│
-- └─────┘
ctimesMins ∷ ℕ → MMSPMins → MMSPMins
-- c ×̃ (b ∧̇ β) ≜ (c × b) ∧̇ (c ×̃ β)
ctimesMins c (MMSPMins 𝓍 b β) = MMSPMins 𝓍 (AddTop c × b) $ ctimesMinsSums c β

-- ┌─────┐
-- │β̇ ×̃ β̇│
-- └─────┘
timesMins ∷ MMSPMins → MMSPMins → MMSPMins
-- (b₁ ∧̇ β₁) ×̃ (b₂ ∧̇ β₂) ≜ (b₁ × b₂) ∧̇ ((b₁ ×̃ β₂) ∧̃ (b₂ ×̃ β₁) ∧̃ (β₁̇ ×̃ β₂))
timesMins (MMSPMins 𝓍₁ b₁ β₁) (MMSPMins 𝓍₂ b₂ β₂) = 
  MMSPMins (𝓍₁ ⊔ 𝓍₂) (b₁ × b₂) $ fold infMinsSums meetMinsSums
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
plusSumsProds γ₁ γ₂ = γ₁ ⊎ γ₂

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
cplusSums c₀ (MMSPSums 𝓍 c γ) = MMSPSums 𝓍 (c₀ + c) γ

-- ┌─────┐
-- │γ̇ +̃ γ̇│
-- └─────┘
plusSums ∷ MMSPSums → MMSPSums → MMSPSums
-- c₁ +̇ γ₁ +̃ c₂ +̇ γ₂ ≜ (c₁ + c₂) +̇ (γ₁ +̃ γ₂)
plusSums (MMSPSums 𝓍₁ c₁ γ₁) (MMSPSums 𝓍₂ c₂ γ₂) = MMSPSums (𝓍₁ ⊔ 𝓍₂) (c₁ + c₂) $ plusSumsProds γ₁ γ₂

-- ┌─────┐
-- │γ̇ ×̃ γ̇│
-- └─────┘
timesSums ∷ MMSPSums → MMSPSums → MMSPSums
-- (c₁ +̇ γ₁) ×̃ (c₂ +̇ γ₂) ≜ (c₁ × c₂) +̇ ((c₁ ×̃ γ₂) +̃ (c₂ ×̃ γ₁) +̃ (γ₁ ×̃ γ₂))
timesSums (MMSPSums 𝓍₁ c₁ γ₁) (MMSPSums 𝓍₂ c₂ γ₂) =
  MMSPSums (𝓍₁ ⊔ 𝓍₂) (c₁ × c₂) $ fold zeroSumsProds plusSumsProds
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
timesProds (MMSPProds 𝓍₁ δ₁) (MMSPProds 𝓍₂ δ₂) = MMSPProds (𝓍₁ ⊔ 𝓍₂) $ δ₁ ⊎ δ₂
