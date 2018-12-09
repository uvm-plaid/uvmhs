module Main where

import UVMHS

-- ℓ ∈ Entity 
type Entity = 𝑃 𝕊

-- η ∈ Event
data Event = Entity :↝ Entity
  deriving (Eq,Ord,Show)
  

-- h ∈ Eff
data Eff =
    Emp
  | Eps
  | Ev Event
  | Eff :| Eff
  | Eff :⨟ Eff
  | Var 𝕊
  | Mu 𝕊 Eff
  deriving (Eq,Ord,Show)

-- t ∈ Trace
type Trace = 𝐿 Event

subst ∷ 𝕊 → Eff → Eff → Eff
subst x h = \case
  Emp → Emp
  Eps → Eps
  Ev η → Ev η
  h₁ :| h₂ → subst x h h₁ :| subst x h h₂
  h₁ :⨟ h₂ → subst x h h₁ :⨟ subst x h h₂
  Var x' → case x ≡ x' of 
    True → h 
    False → Var x'
  Mu x' h' → case x ≡ x' of
    True → Mu x' h'
    False → Mu x' $ subst x h h'
  
step ∷ Trace → Eff → 𝐿 (Trace ∧ Eff)
step t = \case
  Emp → mzero
  Eps → return $ t :꘍ Eps
  Ev η → return $ (t ⧺ single η) :꘍ Eps
  h₁ :| h₂ → mconcat $
    list
    [ return $ t :꘍ h₁
    , return $ t :꘍ h₂
    ]
  Eps :⨟ h₂ → return $ t :꘍ h₂
  h₁ :⨟ h₂ → do
    (t' :꘍ h₁') ← step t h₁
    return $ t' :꘍ (h₁' :⨟ h₂)
  Var _ → error "expression not closed"
  Mu x h → return $ t :꘍ subst x (Mu x h) h

stepN ∷ ℕ → Trace → Eff → 𝑃 (Trace ∧ Eff)
stepN n t h 
  | n ≡ 0 = single (t :꘍ h)
  | otherwise = pow $ do
      t' :꘍ h' ← iter $ step t h
      iter $ stepN (n - 1) t' h'

-- tA ∈ TraceA
type TraceA = 𝑃 Event

closure ∷ TraceA → TraceA
closure tA₀ = lfp tA₀ $ \ tA → (tA ∪) $ pow $ do
  ℓ₁ :↝ ℓ₂ ← iter tA
  ℓ₃ :↝ ℓ₄ ← iter tA
  case ℓ₂ ∩ ℓ₃ ≢ pø of
    True → return $ ℓ₁ :↝ ℓ₄
    False → mzero

-- stepBar ∷ Trace → Eff → 𝑃 (Trace ∧ Eff) ∧ 𝔹
-- stepBar t = \case
--   Emp → pø
--   Eps → single (t :꘍ Eps)
--   Ev η → single ((t ⧺ single η) :꘍ Eps)
--   h₁ :| h₂ → single (t :꘍ h₁) ∪ single (t :꘍ h₂)
--   Eps :⨟ h₂ → single (t :꘍ h₂)
--   h₁ :⨟ h₂ → pow [ (t' :꘍ (h₁' :⨟ h₂)) | (t' :꘍ h₁') ← iter (step t h₁) ]
--   Var x → undefined
--   Mu x h → single (t :꘍ subst x (Mu x h) h)

e₁ = Mu "x" $ (Ev (single "A" :↝ single "A")) :| (Ev (single "B" :↝ single "B") :⨟ Var "x")
e₂ = Mu "x" $ (Ev (single "OPEN" :↝ single "X"):⨟ Var "x" :⨟ Ev (single "CLOSE" :↝ single "X")) :| Eps

main ∷ IO ()
main = do
  out "======="
  out "EXAMPLE"
  out "======="
  eachWith (stepN 20 null e₂) $ \ x →
    out $ show𝕊 x
  out "======="
  out "CLOSURE"
  out "======="
  out $ show𝕊 $ closure $ pow $
    list
    [ pow ["A","B"] :↝ pow ["X","Y"]
    , pow ["Y","Z"] :↝ pow ["H","I"]
    ]
  
-- let bind ma f = flatten (map f ma)
-- 
-- let return a = [a]
-- 
-- let rec multicomp fstar n x = match n with
--   0 -> x
-- | n -> fstar (multicomp fstar (n-1) x)
-- 
-- let multistep te n = (multicomp (fun ma -> bind ma step) n) (return ([], te))
-- 
-- 
-- let te0 = Mu('h', Choice(Ev(1), Seq(Ev(2), Var('h'))))
-- let te1 = Ev(1)
-- let te2 = Choice(Ev(1), Seq(Ev(2), Ev(3)))
