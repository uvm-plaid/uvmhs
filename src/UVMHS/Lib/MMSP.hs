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

newtype MMSP a = MMSP
  { mmspMaxs ∷ MMSPMaxs a
  } 
  deriving (Eq,Ord,Show)

data MMSPMaxs a = MMSPMaxs
  { mmspMaxsBindingInfo ∷ 𝔛
  , mmspMaxsConstant    ∷ ℕ
  , mmspMaxsMins        ∷ 𝑃 (MMSPMins a)
  }
  deriving (Eq,Ord,Show)

data MMSPMins a = MMSPMins
  { mmspMinsBindingInfo ∷ 𝔛
  , mmspMinsConstant    ∷ AddTop ℕ       -- non-zero
  , mmspMinsSums        ∷ 𝑃 (MMSPSums a) -- at least one
  }
  deriving (Eq,Ord,Show)

data MMSPSums a = MMSPSums
  { mmspSumsBindingInfo ∷ 𝔛
  , mmspSumsConstant    ∷ ℕ
  , mmspSumsPRods       ∷ MMSPProds a ⇰ ℕ -- at least one
  }
  deriving (Eq,Ord,Show)

data MMSPProds a = MMSPProds
  { mmspProdsBindingInfo ∷ 𝔛
  , mmspProdsExps        ∷ MMSPAtom a ⇰ ℕ -- at least one
  }
  deriving (Eq,Ord,Show)

data MMSPAtom a = 
    Var_MMSP 𝕏
  | Meta_MMSP 𝕏 (𝔖 a)
  deriving (Eq,Ord,Show)

----------
-- MMSP --
----------

----------------
-- OPERATIONS --
----------------

instance Zero (MMSP a) where zero = litMMSP zero
instance One (MMSP a) where one = litMMSP one
instance (Ord a) ⇒ Plus (MMSP a) where (+) = plusMMSP
instance (Ord a) ⇒ Times (MMSP a) where (×) = timesMMSP
instance (Ord a) ⇒ Pon (MMSP a) where (^^) = ponMMSP
instance Bot (MMSP a) where bot = litMMSP zero
instance (Ord a) ⇒ Join (MMSP a) where (⊔) = joinMMSP
instance (Ord a) ⇒ Top (MMSP a) where top = topMMSP
instance (Ord a) ⇒ Meet (MMSP a) where (⊓) = meetMMSP

instance (Ord a) ⇒ Additive (MMSP a)
instance (Ord a) ⇒ Multiplicative (MMSP a)
instance (Ord a) ⇒ JoinLattice (MMSP a)
instance (Ord a) ⇒ MeetLattice (MMSP a)

maxsMMSPL ∷ MMSP a ⌲ MMSPMaxs a
maxsMMSPL = prism MMSP $ Some ∘ mmspMaxs

minsMMSPL ∷ (Ord a) ⇒ MMSP a ⌲ MMSPMins a
minsMMSPL  = 
  let mk β̇ = MMSPMaxs (mmspMinsBindingInfo β̇) zero $ single β̇
      vw = \case
        MMSPMaxs _𝓍 a α | a ≡ zero , Some β̇ ← view single𝑃L α → Some β̇
        _ → None
  in prism mk vw ⊚ maxsMMSPL

sumsMMSPL ∷ (Ord a) ⇒ MMSP a ⌲ MMSPSums a
sumsMMSPL = 
  let mk γ̇ = MMSPMins (mmspSumsBindingInfo γ̇) Top $ single $ γ̇
      vw = \case
        MMSPMins _𝓍 b β | b ≡ Top , Some γ̇ ← view single𝑃L β → Some γ̇
        _ → None
  in prism mk vw ⊚ minsMMSPL

prodsMMSPL ∷ (Ord a) ⇒ MMSP a ⌲ MMSPProds a
prodsMMSPL = 
  let mk δ̇ = MMSPSums (mmspProdsBindingInfo δ̇) zero $ δ̇ ↦ one
      vw = \case
        MMSPSums _𝓍 c γ | c ≡ zero , Some (δ̇ :* d) ← view single𝐷L γ , d ≡ one → Some δ̇
        _ → None
  in prism mk vw ⊚ sumsMMSPL

atomMMSPL ∷ (Ord a) ⇒ (a → 𝔛) → MMSP a ⌲ MMSPAtom a
atomMMSPL fv =
  let mk ω =
        let 𝓍 = case ω of
              Var_MMSP x → 𝔵lexical $ single x
              Meta_MMSP χ 𝓈 → 𝔵meta (single χ) ⊔ joins (map fv $ iter $ 𝔰values 𝓈)
        in MMSPProds 𝓍 $ ω ↦ one
      vw = \case
        MMSPProds _𝓍 δ | Some (ω :* e) ← view single𝐷L δ , e ≡ one → Some ω
        _ → None
  in prism mk vw ⊚ prodsMMSPL

litMMSPL ∷ MMSP a ⌲ ℕ
litMMSPL = 
  let mk n = MMSPMaxs bot n null
      vw = \case
        MMSPMaxs _𝓍 a α | isEmpty α → Some a
        _ → None
  in prism mk vw ⊚ maxsMMSPL

topMMSPL ∷ (Ord a) ⇒ MMSP a ⌲ ()
topMMSPL = 
  let mk () = MMSPMins bot Top null
      vw = \case
        MMSPMins _𝓍 b β | b ≡ Top , isEmpty β → Some ()
        _ → None
  in prism mk vw ⊚ minsMMSPL

littMMSPL ∷ (Ord a) ⇒ MMSP a ⌲ AddTop ℕ
littMMSPL =
  let mk = \case
        AddTop n → litMMSP n
        Top → topMMSP
      vw η 
        | Some n ← view litMMSPL η = Some $ AddTop n
        | Some () ← view topMMSPL η = Some Top
        | otherwise = None
  in prism mk vw

maxsMMSP ∷ MMSPMaxs a → MMSP a
maxsMMSP = construct maxsMMSPL

minsMMSP ∷ (Ord a) ⇒ MMSPMins a → MMSP a
minsMMSP = construct minsMMSPL

sumsMMSP ∷ (Ord a) ⇒ MMSPSums a → MMSP a
sumsMMSP = construct sumsMMSPL

prodsMMSP ∷ (Ord a) ⇒ MMSPProds a → MMSP a
prodsMMSP = construct prodsMMSPL

atomMMSP ∷ (Ord a) ⇒ (a → 𝔛) → MMSPAtom a → MMSP a
atomMMSP = construct ∘ atomMMSPL

litMMSP ∷ ℕ → MMSP a
litMMSP = construct litMMSPL

topMMSP ∷ (Ord a) ⇒ MMSP a
topMMSP = construct topMMSPL ()

joinMMSP ∷ (Ord a) ⇒ MMSP a → MMSP a → MMSP a
joinMMSP (MMSP α̇₁) (MMSP α̇₂) = MMSP $ joinMaxs α̇₁ α̇₂

meetMMSP ∷ (Ord a) ⇒ MMSP a → MMSP a → MMSP a
meetMMSP (MMSP α̇₁) (MMSP α̇₂) = MMSP $ meetMaxs α̇₁ α̇₂

plusMMSP ∷ (Ord a) ⇒ MMSP a → MMSP a → MMSP a
plusMMSP (MMSP α̇₁) (MMSP α̇₂) = MMSP $ plusMaxs α̇₁ α̇₂

timesMMSP ∷ (Ord a) ⇒ MMSP a → MMSP a → MMSP a
timesMMSP (MMSP α̇₁) (MMSP α̇₂) = MMSP $ timesMaxs α̇₁ α̇₂

ponMMSP ∷ (Ord a) ⇒ MMSP a → ℕ → MMSP a
ponMMSP e n = applyN n one (× e)

---------------
-- FREE VARS --
---------------

freeVarsMMSP ∷ MMSP a → 𝔛
freeVarsMMSP = mmspMaxsBindingInfo  ∘ mmspMaxs

------------------
-- SUBSTITUTION --
------------------

substMMSP ∷ (Monad m, Ord a) ⇒ (a → 𝔛) → (a → m (MMSP a)) → 𝔖 a → MMSP a → m (MMSP a)
substMMSP fv 𝒸 𝓈 (MMSP α̇) = substMaxs fv 𝒸 𝓈 α̇

substMaxs ∷ (Monad m, Ord a) ⇒ (a → 𝔛) → (a → m (MMSP a)) → 𝔖 a → MMSPMaxs a → m (MMSP a)
substMaxs fv 𝒸 𝓈 η@(MMSPMaxs 𝓍 a α) = do
  let 𝓈' = 𝔰restrict 𝓍 𝓈
  if isEmpty 𝓈'
  then return $ maxsMMSP η
  else (⊔) (litMMSP a) ^$ substMaxsMins fv 𝒸 𝓈' α

substMaxsMins ∷ (Monad m, Ord a) ⇒ (a → 𝔛) → (a → m (MMSP a)) → 𝔖 a → 𝑃 (MMSPMins a) → m (MMSP a)
substMaxsMins fv 𝒸 𝓈 α = joins ^$ mapM (substMins fv 𝒸 𝓈) $ iter α

substMins ∷ (Monad m, Ord a) ⇒ (a → 𝔛) → (a → m (MMSP a)) → 𝔖 a → MMSPMins a → m (MMSP a)
substMins fv 𝒸 𝓈 η@(MMSPMins 𝓍 b β) = do
  let 𝓈' = 𝔰restrict 𝓍 𝓈
  if isEmpty 𝓈'
  then return $ minsMMSP η
  else (⊓) (elimAddTop top litMMSP b) ^$ substMinsSums fv 𝒸 𝓈' β

substMinsSums ∷ (Monad m, Ord a) ⇒ (a → 𝔛) → (a → m (MMSP a)) → 𝔖 a → 𝑃 (MMSPSums a) → m (MMSP a)
substMinsSums fv 𝒸 𝓈 β = meets ^$ mapM (substSums fv 𝒸 𝓈) $ iter β

substSums ∷ (Monad m, Ord a) ⇒ (a → 𝔛) → (a → m (MMSP a)) → 𝔖 a → MMSPSums a → m (MMSP a)
substSums fv 𝒸 𝓈 η@(MMSPSums 𝓍 c γ) = do
  let 𝓈' = 𝔰restrict 𝓍 𝓈
  if isEmpty 𝓈'
  then return $ sumsMMSP η
  else (+) (litMMSP c) ^$ substSumsProds fv 𝒸 𝓈' γ

substSumsProds ∷ (Monad m, Ord a) ⇒ (a → 𝔛) → (a → m (MMSP a)) → 𝔖 a → MMSPProds a ⇰ ℕ → m (MMSP a)
substSumsProds fv 𝒸 𝓈 γ = sum ^$ mapMOn (iter γ) $ \ (δ :* d) → (litMMSP d ×) ^$ substProds fv 𝒸 𝓈 δ

substProds ∷ (Monad m, Ord a) ⇒ (a → 𝔛) → (a → m (MMSP a)) → 𝔖 a → MMSPProds a → m (MMSP a)
substProds fv 𝒸 𝓈 η@(MMSPProds 𝓍 δ) = do
  let 𝓈' = 𝔰restrict 𝓍 𝓈
  if isEmpty 𝓈'
  then return $ prodsMMSP η
  else product ^$ mapMOn (iter δ) $ \ (ω :* e) → do
    ω' ← substAtom fv 𝒸 𝓈' ω
    return $ ω' ^^ e

substAtom ∷ (Monad m, Ord a) ⇒ (a → 𝔛) → (a → m (MMSP a)) → 𝔖 a → MMSPAtom a → m (MMSP a)
substAtom fv 𝒸 𝓈 = \case
  Var_MMSP x → case 𝔰lexicals 𝓈 ⋕? x of
    None → return $ atomMMSP fv $ Var_MMSP x
    Some e → 𝒸 e
  Meta_MMSP χ 𝓈' → case 𝔰metas 𝓈 ⋕? χ of
    None → return $ atomMMSP fv $ Meta_MMSP χ 𝓈'
    Some e → substMMSP fv 𝒸 𝓈' *$ 𝒸 e

----------
-- MAXS --
----------

-- Mins --

-- ┌─────┐
-- │α ≡ 0│
-- └─────┘
zeroMaxsMins ∷ 𝑃 (MMSPMins a)
-- β ≡ 0 ≜ ⨆{}
zeroMaxsMins = null

-- ┌─────┐
-- │α ∨̃ α│
-- └─────┘
joinMaxsMins ∷ (Ord a) ⇒ 𝑃 (MMSPMins a) → 𝑃 (MMSPMins a) → 𝑃 (MMSPMins a)
joinMaxsMins α₁ α₂ = α₁ ∪ α₂

-- ┌─────┐
-- │b ∧̃ α│
-- └─────┘
cmeetMaxsMins ∷ (Ord a) ⇒ AddTop ℕ → 𝑃 (MMSPMins a) → 𝑃 (MMSPMins a)
-- b ∧̃ α = c ⊓ ⨆{ β | β ∈ α} 
--       ≜ ⨆ { b ∧̃ β | β ∈ α}
cmeetMaxsMins b = pow ∘ map (cmeetMins b) ∘ iter

-- ┌─────┐
-- │α ∧̃ α│
-- └─────┘
meetMaxsMins ∷ (Ord a) ⇒ 𝑃 (MMSPMins a) → 𝑃 (MMSPMins a) → 𝑃 (MMSPMins a)
-- α₁ ∧̃ α₂ = ⨆{ β | β ∈ α₁ } + ⨆{ β | β ∈ α₂ }
--         ≜ ⨆{ β₁ ∧̃ β₂ | β₁ ∈ α₁ , β₂ ∈ α₂}
meetMaxsMins α₁ α₂ = pow $ mapOn (iter α₁ ⧆ iter α₂) $ \ (β₁ :* β₂) → meetMins β₁ β₂

-- ┌─────┐
-- │c +̃ α│
-- └─────┘
cplusMaxsMins ∷ (Ord a) ⇒ ℕ → 𝑃 (MMSPMins a) → 𝑃 (MMSPMins a)
-- c +̃ α = c + ⨆{ β | β ∈ α} 
--       ≜ ⨆ { c +̃ β | β ∈ α}
cplusMaxsMins c = pow ∘ map (cplusMins c) ∘ iter

-- ┌─────┐
-- │α +̃ α│
-- └─────┘
plusMaxsMins ∷ (Ord a) ⇒ 𝑃 (MMSPMins a) → 𝑃 (MMSPMins a) → 𝑃 (MMSPMins a)
-- α₁ +̃ α₂ = ⨆{ β | β ∈ α₁ } + ⨆{ β | β ∈ α₂ }
--         ≜ ⨆{ β₁ +̃ β₂ | β₁ ∈ α₁ , β₂ ∈ α₂}
plusMaxsMins α₁ α₂ = pow $ mapOn (iter α₁ ⧆ iter α₂) $ \ (β₁ :* β₂) → plusMins β₁ β₂

-- ┌─────┐
-- │d ×̃ α│
-- └─────┘
ctimesMaxsMins ∷ (Ord a) ⇒ ℕ → 𝑃 (MMSPMins a) → 𝑃 (MMSPMins a)
-- d ×̃ α = d × ⨆{ β | β ∈ α} 
--       ≜ ⨆ { d ×̃ β | β ∈ α}
ctimesMaxsMins d = pow ∘ map (ctimesMins d) ∘ iter

-- ┌─────┐
-- │α ×̃ α│
-- └─────┘
timesMaxsMins ∷ (Ord a) ⇒ 𝑃 (MMSPMins a) → 𝑃 (MMSPMins a) → 𝑃 (MMSPMins a)
-- α₁ ×̃ α₂ = ⨆{ β | β ∈ α₁ } × ⨆{ β | β ∈ α₂ }
--         ≜ ⨆{ β₁ ×̃ β₂ | β₁ ∈ α₁ , β₂ ∈ α₂}
timesMaxsMins α₁ α₂ = pow $ mapOn (iter α₁ ⧆ iter α₂) $ \ (β₁ :* β₂) → timesMins β₁ β₂

-- Maxs --

-- ┌─────┐
-- │α̇ ∨̃ α̇│
-- └─────┘
joinMaxs ∷ (Ord a) ⇒ MMSPMaxs a → MMSPMaxs a → MMSPMaxs a
-- 
joinMaxs (MMSPMaxs 𝓍₁ a₁ α₁) (MMSPMaxs 𝓍₂ a₂ α₂) = MMSPMaxs (𝓍₁ ⊔ 𝓍₂) (a₁ ⊔ a₂) $ joinMaxsMins α₁ α₂

-- ┌─────┐
-- │α̇ ∧̃ α̇│
-- └─────┘
meetMaxs ∷ (Ord a) ⇒ MMSPMaxs a → MMSPMaxs a → MMSPMaxs a
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
plusMaxs ∷ (Ord a) ⇒ MMSPMaxs a → MMSPMaxs a → MMSPMaxs a
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
timesMaxs ∷ (Ord a) ⇒ MMSPMaxs a → MMSPMaxs a → MMSPMaxs a
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
infMinsSums ∷ 𝑃 (MMSPSums a)
-- β ≡ ∞ ≜ ⨅{}
infMinsSums = null

-- ┌─────┐
-- │β ∧̃ β│
-- └─────┘
meetMinsSums ∷ (Ord a) ⇒ 𝑃 (MMSPSums a) → 𝑃 (MMSPSums a) → 𝑃 (MMSPSums a)
-- β₁ ∧̃ β₂ = ⨅{ γ | γ ∈ β₁ } ⊓ ⨅{ γ | γ ∈ β₂ }
--         ≜ ⨅( { γ | γ ∈ β₁ }
--            ∪ { γ | γ ∈ β₂ } )
meetMinsSums xs ys = xs ∪ ys

-- ┌─────┐
-- │c +̃ β│
-- └─────┘
cplusMinsSums ∷ (Ord a) ⇒ ℕ → 𝑃 (MMSPSums a) → 𝑃 (MMSPSums a)
-- c +̃ β = c + ⨅{ γ | γ ∈ β} 
--       ≜ ⨅ { c +̃ γ | γ ∈ β}
cplusMinsSums c = pow ∘ map (cplusSums c) ∘ iter

-- ┌─────┐
-- │β +̃ β│
-- └─────┘
plusMinsSums ∷ (Ord a) ⇒ 𝑃 (MMSPSums a) → 𝑃 (MMSPSums a) → 𝑃 (MMSPSums a)
-- β₁ +̃ β₂ = ⨅{ γ | γ ∈ β₁ } + ⨅{ γ | γ ∈ β₂ }
--         ≜ ⨅{ γ₁ +̃ γ₂ | γ₁ ∈ β₁ , γ₂ ∈ β₂}
plusMinsSums β₁ β₂ = pow $ mapOn (iter β₁ ⧆ iter β₂) $ \ (γ₁ :* γ₂) → plusSums γ₁ γ₂

-- ┌─────┐
-- │d ×̃ β│
-- └─────┘
ctimesMinsSums ∷ (Ord a) ⇒ ℕ → 𝑃 (MMSPSums a) → 𝑃 (MMSPSums a)
-- d ×̃ β = d × ⨅{ γ | γ ∈ β} 
--       ≜ ⨅ { d ×̃ γ | γ ∈ β}
ctimesMinsSums c = pow ∘ map (cplusSums c) ∘ iter

-- ┌─────┐
-- │β ×̃ β│
-- └─────┘
timesMinsSums ∷ (Ord a) ⇒ 𝑃 (MMSPSums a) → 𝑃 (MMSPSums a) → 𝑃 (MMSPSums a)
-- β₁ ×̃ β₂ = ⨅{ γ | γ ∈ β₁ } × ⨅{ γ | γ ∈ β₂ }
--         ≜ ⨅{ γ₁ ×̃ γ₂ | γ₁ ∈ β₁ , γ₂ ∈ β₂}
timesMinsSums β₁ β₂ = pow $ mapOn (iter β₁ ⧆ iter β₂) $ \ (γ₁ :* γ₂) → timesSums γ₁ γ₂

-- Mins --

-- ┌─────┐
-- │b ∧̃ β̇│
-- └─────┘
cmeetMins ∷ AddTop ℕ → MMSPMins a → MMSPMins a
-- b₀ ⊓ (b ∧̇ β) ≜ (b₀ ⊓ b) ∧̇ β
cmeetMins b₀ (MMSPMins 𝓍 b β) = MMSPMins 𝓍 (b₀ ⊓ b) β

-- ┌─────┐
-- │β̇ ∧̃ β̇│
-- └─────┘
meetMins ∷ (Ord a) ⇒ MMSPMins a → MMSPMins a → MMSPMins a
-- (b₁ ∧̇  β₁) ⊓ (b₂ ∧̇  β₂) ≜ (b₁ ⊓ b₂) ∧̇ (β₁ ∧̃ β₂)
meetMins (MMSPMins 𝓍₁ b₁ β₁) (MMSPMins 𝓍₂ b₂ β₂) = MMSPMins (𝓍₁ ⊔ 𝓍₂) (b₁ ⊓ b₂) $ meetMinsSums β₁ β₂

-- ┌─────┐
-- │c +̃ β̇│
-- └─────┘
cplusMins ∷ (Ord a) ⇒ ℕ → MMSPMins a → MMSPMins a
-- c +̃ (b ∧̇ β) ≜ (c + b) ∧̇ (c +̃ β)
cplusMins c (MMSPMins 𝓍 b β) = MMSPMins 𝓍 (map (c +) b) $ cplusMinsSums c β

-- ┌─────┐
-- │β̇ +̃ β̇│
-- └─────┘
plusMins ∷ (Ord a) ⇒ MMSPMins a → MMSPMins a → MMSPMins a
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
ctimesMins ∷ (Ord a) ⇒ ℕ → MMSPMins a → MMSPMins a
-- c ×̃ (b ∧̇ β) ≜ (c × b) ∧̇ (c ×̃ β)
ctimesMins c (MMSPMins 𝓍 b β) = MMSPMins 𝓍 (AddTop c × b) $ ctimesMinsSums c β

-- ┌─────┐
-- │β̇ ×̃ β̇│
-- └─────┘
timesMins ∷ (Ord a) ⇒ MMSPMins a → MMSPMins a → MMSPMins a
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
zeroSumsProds ∷ MMSPProds a ⇰ ℕ
-- γ ≡ 0 ≜ ∑{}
zeroSumsProds = null

-- ┌─────┐
-- │γ +̃ γ│
-- └─────┘
plusSumsProds ∷ (Ord a) ⇒ MMSPProds a ⇰ ℕ → MMSPProds a ⇰ ℕ → MMSPProds a ⇰ ℕ
-- γ₁ +̃ γ₂ = ∑{ d×̇δ | d×̇δ ∈ γ₁} + ∑{ d×̇δ | d×̇δ ∈ γ₂ }
--         ≜ ∑( { d×̇δ | d×̇δ ∈ γ₁ , δ ∉ dom(γ₂) }
--            ∪ { d×̇δ | d×̇δ ∈ γ₂ , δ ∉ dom(γ₁) }
--            ∪ { (d₁+d₂)×̇δ | d₁×̇δ ∈ γ₁ , d₂×̇δ ∈ γ₂ } )
plusSumsProds γ₁ γ₂ = γ₁ ⊎ γ₂

-- ┌─────┐
-- │d ×̃ γ│
-- └─────┘
ctimesSumsProds ∷ ℕ → MMSPProds a ⇰ ℕ → MMSPProds a ⇰ ℕ
-- d₀ ×̃ γ ≜ d₀ × ∑{ d×̇δ | d×̇δ ∈ γ }
--        ≜ ∑{ d₀d×̇δ | d×̇δ ∈ γ }
ctimesSumsProds d γ = map (× d) γ

-- ┌─────┐
-- │γ ×̃ γ│
-- └─────┘
timesSumsProds ∷ (Ord a) ⇒ MMSPProds a ⇰ ℕ → MMSPProds a ⇰ ℕ → MMSPProds a ⇰ ℕ
-- γ₁ ×̃ γ₂ = ∑{ d×̇δ | d×̇δ ∈ γ₁} × ∑{ d×̇δ | d×̇δ ∈ γ₂ }
--         ≜ ∑{ d₁d₂×̇(δ₁×̃δ₂) | d₁×̇δ₁ ∈ γ₁ , d₂×̇δ₂ ∈ γ₂ }
timesSumsProds γ₁ γ₂ = assoc $ mapOn (iter γ₁ ⧆ iter γ₂) $ \ ((δ₁ :* d₁) :* (δ₂ :* d₂)) → 
  timesProds δ₁ δ₂ :* (d₁ × d₂)

-- Sums --

-- ┌─────┐
-- │c +̃ γ̇│
-- └─────┘
cplusSums ∷ ℕ → MMSPSums a → MMSPSums a
-- c₀ +̃ (c +̇ γ) ≜ (c₀ + c) +̇ γ
cplusSums c₀ (MMSPSums 𝓍 c γ) = MMSPSums 𝓍 (c₀ + c) γ

-- ┌─────┐
-- │γ̇ +̃ γ̇│
-- └─────┘
plusSums ∷ (Ord a) ⇒ MMSPSums a → MMSPSums a → MMSPSums a
-- c₁ +̇ γ₁ +̃ c₂ +̇ γ₂ ≜ (c₁ + c₂) +̇ (γ₁ +̃ γ₂)
plusSums (MMSPSums 𝓍₁ c₁ γ₁) (MMSPSums 𝓍₂ c₂ γ₂) = MMSPSums (𝓍₁ ⊔ 𝓍₂) (c₁ + c₂) $ plusSumsProds γ₁ γ₂

-- ┌─────┐
-- │γ̇ ×̃ γ̇│
-- └─────┘
timesSums ∷ (Ord a) ⇒ MMSPSums a → MMSPSums a → MMSPSums a
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
timesProds ∷ (Ord a) ⇒ MMSPProds a → MMSPProds a → MMSPProds a
-- δ₁ +̃ δ₂ = ∏{ ω^̇e | ω^̇e ∈ δ₁} × ∏{ ω^̇e | ω^̇e ∈ δ₂ }
--         ≜ ∏( { ω^̇e | ω^̇e ∈ δ₁ , ω ∉ dom(δ₂) }
--            ∪ { ω^̇e | ω^̇e ∈ δ₂ , ω ∉ dom(δ₁) }
--            ∪ { ω^̇(e₁+e₂) | ω^̇e₁ ∈ δ₁ , ω^̇e₂ ∈ δ₂ } )
timesProds (MMSPProds 𝓍₁ δ₁) (MMSPProds 𝓍₂ δ₂) = MMSPProds (𝓍₁ ⊔ 𝓍₂) $ δ₁ ⊎ δ₂
