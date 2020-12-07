module UVMHS.Lib.AD where

import UVMHS.Core
import UVMHS.Lib.Pretty

--------------------------
-- Dual Number Forward ---
--------------------------

data ADF a = ADF
  { adfVal ∷ a
  , adfDer ∷ a
  } deriving (Eq,Ord,Show)
makeLenses ''ADF
makePrettySum ''ADF

-- ∂ns₁ ∂ns₂ ms = ADF (𝕄S ns₁ (ADF (𝕄S ns₂ (𝕄S ms a))))
--
-- 𝕄S ms (ADF (𝕄S ns₁ (ADF (𝕄S ns₂ a)))) ≈
-- valval: 𝕄S ns₁ (𝕄S ns₂ (𝕄S ms a))
-- valder: 𝕄S ns₁ (𝕄S ns₂ (𝕄S ms a))
-- derval: 𝕄S ns₁ (𝕄S ns₂ (𝕄S ms a))
-- derder: 𝕄S ns₁ (𝕄S ns₂ (𝕄S ms a))

constADF ∷ (Zero a) ⇒ a → ADF a
constADF x = ADF x zero

sensADF ∷ a → a → ADF a
sensADF = ADF

plusADF ∷ (Plus a) ⇒ ADF a → ADF a → ADF a
plusADF (ADF v₁ d₁) (ADF v₂ d₂) = ADF (v₁ + v₂) $ d₁ + d₂

timesADF ∷ (Plus a,Times a) ⇒ ADF a → ADF a → ADF a
timesADF (ADF v₁ d₁) (ADF v₂ d₂) = ADF (v₁ × v₂) $ d₁ × v₂ + d₂ × v₁

---------------------------
-- Dual Number Backward ---
---------------------------

data ADB a = ADB
  { adbVal ∷ a
  , adbDer ∷ a → a → a
  }
makeLenses ''ADB
makePrettySum ''ADB

-- ∂ns₁ ∂ns₂ ms = ADB (𝕄S ns₁ (ADB (𝕄S ns₂ (𝕄S ms a))))

constADB ∷ (Zero a) ⇒ a → ADB a
constADB x = ADB x $ const id

sensADB ∷ a → (a → a → a) → ADB a
sensADB = ADB

plusADB ∷ (Plus a) ⇒ ADB a → ADB a → ADB a
plusADB (ADB v₁ 𝒹₁) (ADB v₂ 𝒹₂) = ADB (v₁ + v₂) $ \ d → 𝒹₁ d ∘ 𝒹₂ d

timesADB ∷ (Times a) ⇒ ADB a → ADB a → ADB a
timesADB (ADB v₁ 𝒹₁) (ADB v₂ 𝒹₂) = ADB (v₁ × v₂) $ \ d → 𝒹₁ (d × v₂) ∘ 𝒹₂ (d × v₁)

-- }}}

------------------------------
-- Dual Number Flat Forward --
------------------------------

-- this should just be a newtype over ADF --
data ADFF (ns ∷ [𝐍]) (f ∷ [𝐍] → ★ → ★) (ms ∷ [𝐍]) (a ∷ ★) = ADFF
  { adffVal ∷ f (ns ⧺ ms) a
  , adffDer ∷ f (ns ⧺ ms) a
  }
makeLenses ''ADFF
makePrettySum ''ADFF

-- ∂ns₁ ∂ns₂ ms = ADFF ns₁ (ADFF ns₂ 𝕄S) ms a
-- val:    (ADFF (ns₁ ⧺ ns₂) 𝕄S ms a)²
-- der:    (ADFF (ns₁ ⧺ ns₂) 𝕄S ms a)²
-- valval: 𝕄S (ns₁ ⧺ ns₂ ⧺ ms) a
-- valder: 𝕄S (ns₁ ⧺ ns₂ ⧺ ms) a
-- derval: 𝕄S (ns₁ ⧺ ns₂ ⧺ ms) a
-- derder: 𝕄S (ns₁ ⧺ ns₂ ⧺ ms) a

constADFF ∷ (Zero (f (ns ⧺ ms) a)) ⇒ f (ns ⧺ ms) a → ADFF ns f ms a
constADFF v = ADFF v zero

sensADFF ∷ f (ns ⧺ ms) a → f (ns ⧺ ms) a → ADFF ns f ms a
sensADFF = ADFF

plusADFF ∷ (Plus (f (ns ⧺ ms) a)) ⇒ ADFF ns f ms a → ADFF ns f ms a → ADFF ns f ms a
plusADFF (ADFF v₁ d₁) (ADFF v₂ d₂) = ADFF (v₁ + v₂) $ d₁ + d₂

timesADFF ∷ (Plus (f (ns ⧺ ms) a),Times (f (ns ⧺ ms) a)) ⇒ ADFF ns f ms a → ADFF ns f ms a → ADFF ns f ms a
timesADFF (ADFF v₁ d₁) (ADFF v₂ d₂) = ADFF (v₁ × v₂) $ d₁ × v₂ + d₂ × v₁

-------------------------------
-- Dual Number Flat Backward --
-------------------------------

-- this should just be a newtype over ADB --
data ADFB (ns ∷ [𝐍]) (f ∷ [𝐍] → ★ → ★) (ms ∷ [𝐍]) (a ∷ ★) = ADFB
  { adfbVal ∷ f (ns ⧺ ms) a
  , adfbDer ∷ f (ns ⧺ ms) a → f (ns ⧺ ms) a → f (ns ⧺ ms) a
  }
makeLenses ''ADFB
makePrettySum ''ADFB

-- ∂ns₁ ∂ns₂ ms = ADFB ns₁ (ADFB ns₂ 𝕄S) ms a
-- val:   𝕄S ms
-- der:   𝕄S ms → X → X
-- X.val: 𝕄S (ns₁ ⧺ ms)
-- X.der: 𝕄S (ns₁ ⧺ ms) → 𝕄S (ns₂ ⧺ ns₁ ⧺ ms) → 𝕄S (ns₂ ⧺ ns₁ ⧺ ms)

constADFB ∷ f (ns ⧺ ms) a → ADFB ns f ms a
constADFB v = ADFB v $ const id

sensADFB ∷ f (ns ⧺ ms) a → (f (ns ⧺ ms) a → f (ns ⧺ ms) a → f (ns ⧺ ms) a) → ADFB ns f ms a
sensADFB = ADFB

plusADFB ∷ (Plus (f (ns ⧺ ms) a)) ⇒ ADFB ns f ms a → ADFB ns f ms a → ADFB ns f ms a
plusADFB (ADFB v₁ 𝒹₁) (ADFB v₂ 𝒹₂) = ADFB (v₁ + v₂) $ \ d → 𝒹₁ d ∘ 𝒹₂ d

timesADFB ∷ (Plus (f (ns ⧺ ms) a),Times (f (ns ⧺ ms) a)) ⇒ ADFB ns f ms a → ADFB ns f ms a → ADFB ns f ms a
timesADFB (ADFB v₁ 𝒹₁) (ADFB v₂ 𝒹₂) = ADFB (v₁ × v₂) $ \ d → 𝒹₁ (d × v₂) ∘ 𝒹₂ (d × v₁)
