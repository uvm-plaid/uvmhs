-- class ConstAD a b | b → a where constAD ∷ a → b
-- class SensAD a b c | c → a,c → b where sensAD ∷ a → b → c

-- instance {-# OVERLAPPABLE #-} (a ~ b)           ⇒ ConstAD a b        where constAD = id
-- instance {-# OVERLAPPING #-}  (𝒩 n,ConstAD a b) ⇒ ConstAD a (𝕍S n b) where constAD = const𝕍S 𝕟64s ∘ constAD

-- =================== --
-- Dual Number Forward -- {{{
-- =================== --

---------------------------------
-- Dual Number Forward Generic --
---------------------------------

data DNF a b = DNF 
  { dnfVal ∷ a
  , dnfDer ∷ b
  } deriving (Eq,Ord,Show)
makeLenses ''DNF
makePrettySum ''DNF

constDNF ∷ (Zero b) ⇒ a → DNF a b
constDNF x = DNF x zero

sensDNF ∷ a → b → DNF a b
sensDNF = DNF

plusDNF ∷ (Plus a,Plus b) ⇒ DNF a b → DNF a b → DNF a b
plusDNF (DNF v₁ d₁) (DNF v₂ d₂) = DNF (v₁ + v₂) $ d₁ + d₂

timesDNF ∷ (ConstAD a b,Times a,Plus b,Times b) ⇒ DNF a b → DNF a b → DNF a b
timesDNF (DNF v₁ d₁) (DNF v₂ d₂) = DNF (v₁ × v₂) $ d₁ × constAD v₂ + d₂ × constAD v₁

instance (Zero b)                             ⇒ ConstAD a   (DNF a b) where constAD = constDNF
instance                                        SensAD  a b (DNF a b) where sensAD  = sensDNF
instance (Zero a,Zero b)                      ⇒ Zero        (DNF a b) where zero    = constDNF zero
instance (Plus a,Plus b)                      ⇒ Plus        (DNF a b) where (+)     = plusDNF
instance (ConstAD a b,Times a,Plus b,Times b) ⇒ Times       (DNF a b) where (×)     = timesDNF

----------------------------------------
-- Dual Number Forward Generic Vector --
----------------------------------------

newtype DNFV n a b = DNFV { unDNFV ∷ DNF a (𝕍S n b) }
  deriving (Eq,Ord,Show,Pretty)

deriving instance (𝒩 n,Zero b)                                    ⇒ ConstAD a          (DNFV n a b)
deriving instance                                                   SensAD  a (𝕍S n b) (DNFV n a b)
deriving instance (𝒩 n,Zero a,Zero b)                             ⇒ Zero               (DNFV n a b)
deriving instance (𝒩 n,Plus a,Plus b)                             ⇒ Plus               (DNFV n a b)
deriving instance (𝒩 n,ConstAD a (𝕍S n b),Times a,Plus b,Times b) ⇒ Times              (DNFV n a b)

--------------------------------
-- Dual Number Forward Scalar --
--------------------------------

newtype DNFS a = DNFS { unDNFS ∷ DNF a a }
  deriving (Eq,Ord,Show,Pretty)

deriving instance (Zero a)         ⇒ ConstAD a   (DNFS a)
deriving instance                    SensAD  a a (DNFS a)
deriving instance (Zero a)         ⇒ Zero        (DNFS a)
deriving instance (Plus a)         ⇒ Plus        (DNFS a)
deriving instance (Plus a,Times a) ⇒ Times       (DNFS a)

--------------------------------------
-- Dual Number Forward Scalar for 𝔻 --
--------------------------------------

newtype DNF𝔻 = DNF𝔻 { unDNF𝔻 ∷ DNFS 𝔻 }
  deriving (Eq,Ord,Show,Pretty)

deriving instance ConstAD 𝔻   DNF𝔻
deriving instance SensAD  𝔻 𝔻 DNF𝔻
deriving instance Plus        DNF𝔻
deriving instance Times       DNF𝔻

---------------------------------------------
-- Dual Number Forward Scalar for (𝕍S n 𝔻) --
---------------------------------------------

newtype DNF𝕍𝔻 n = DNF𝕍𝔻 { unDNF𝕍𝔻 ∷ DNFS (𝕍S n 𝔻) }
  deriving (Eq,Ord,Show,Pretty)

deriving instance (𝒩 n) ⇒ ConstAD (𝕍S n 𝔻)          (DNF𝕍𝔻 n)
deriving instance         SensAD  (𝕍S n 𝔻) (𝕍S n 𝔻) (DNF𝕍𝔻 n)
deriving instance (𝒩 n) ⇒ Plus                      (DNF𝕍𝔻 n)
deriving instance (𝒩 n) ⇒ Times                     (DNF𝕍𝔻 n)

----------------------------------
-- Dual Number Forward Gradient --
----------------------------------

newtype DNFG n a = DNFG { unDNFG ∷ DNFV n a a }
  deriving (Eq,Ord,Show,Pretty)

deriving instance (𝒩 n,Zero a)         ⇒ ConstAD a          (DNFG n a)
deriving instance                        SensAD  a (𝕍S n a) (DNFG n a)
deriving instance (𝒩 n,Zero a)         ⇒ Zero               (DNFG n a)
deriving instance (𝒩 n,Plus a)         ⇒ Plus               (DNFG n a)
deriving instance (𝒩 n,Plus a,Times a) ⇒ Times              (DNFG n a)

----------------------------------------
-- Dual Number Forward Gradient for 𝔻 --
----------------------------------------

newtype DNFV𝔻 n = DNFV𝔻 { unDNFV𝔻 ∷ DNFV n 𝔻 𝔻 }
  deriving (Eq,Ord,Show,Pretty)

deriving instance (𝒩 n) ⇒ ConstAD 𝔻          (DNFV𝔻 n)
deriving instance         SensAD  𝔻 (𝕍S n 𝔻) (DNFV𝔻 n)
deriving instance (𝒩 n) ⇒ Zero               (DNFV𝔻 n)
deriving instance (𝒩 n) ⇒ Plus               (DNFV𝔻 n)
deriving instance (𝒩 n) ⇒ Times              (DNFV𝔻 n)

----------------------------------
-- Dual Number Forward Jacobian --
----------------------------------

newtype DNFJ m n a = DNFJ { unDNFJ ∷ DNFV m a (DNFG n a) }
  deriving (Eq,Ord,Show,Pretty)

deriving instance (𝒩 m,𝒩 n,Zero a)                ⇒ ConstAD a                   (DNFJ m n a)
deriving instance                                   SensAD  a (𝕍S m (DNFG n a)) (DNFJ m n a)
deriving instance (𝒩 m,𝒩 n,Zero a)                ⇒ Zero                        (DNFJ m n a)
deriving instance (𝒩 m,𝒩 n,Plus a)                ⇒ Plus                        (DNFJ m n a)
deriving instance (𝒩 m,𝒩 n,Zero a,Plus a,Times a) ⇒ Times                       (DNFJ m n a)

----------------------------------------
-- Dual Number Forward Jacobian for 𝔻 --
----------------------------------------

newtype DNFJ𝔻 m n = DNFJ𝔻 { unDNFJ𝔻 ∷ DNFV m 𝔻 (DNFG n 𝔻) }
  deriving (Eq,Ord,Show,Pretty)

deriving instance (𝒩 m,𝒩 n) ⇒ ConstAD 𝔻                   (DNFJ𝔻 m n)
deriving instance             SensAD  𝔻 (𝕍S m (DNFG n 𝔻)) (DNFJ𝔻 m n)
deriving instance (𝒩 m,𝒩 n) ⇒ Zero                        (DNFJ𝔻 m n)
deriving instance (𝒩 m,𝒩 n) ⇒ Plus                        (DNFJ𝔻 m n)
deriving instance (𝒩 m,𝒩 n) ⇒ Times                       (DNFJ𝔻 m n)

-- }}}

-- ==================== --
-- Dual Number Backward -- {{{
-- ==================== --

----------------------------------
-- Dual Number Backward Generic --
----------------------------------

data DNB a b = DNB 
  { dnbVal ∷ a
  , dnbDer ∷ a → b
  }
makeLenses ''DNB
makePrettySum ''DNB

constDNB ∷ (Zero b) ⇒ a → DNB a b
constDNB x = DNB x $ \ _ → zero

sensDNB ∷ a → (a → b) → DNB a b
sensDNB = DNB

plusDNB ∷ (Plus a,Plus b) ⇒ DNB a b → DNB a b → DNB a b
plusDNB (DNB v₁ d₁) (DNB v₂ d₂) = DNB (v₁ + v₂) $ \ δ → 
  d₁ δ + d₂ δ

timesDNB ∷ (Times a,Plus b) ⇒ DNB a b → DNB a b → DNB a b
timesDNB (DNB v₁ d₁) (DNB v₂ d₂) = DNB (v₁ × v₂) $ \ δ →
  d₁ (δ × v₂) + d₂ (δ × v₁)

instance (Zero b)         ⇒ ConstAD a         (DNB a b) where constAD = constDNB
instance                    SensAD  a (a → b) (DNB a b) where sensAD  = sensDNB
instance (Zero a,Zero b)  ⇒ Zero              (DNB a b) where zero    = constAD zero
instance (Plus a,Plus b)  ⇒ Plus              (DNB a b) where (+)     = plusDNB
instance (Times a,Plus b) ⇒ Times             (DNB a b) where (×)     = timesDNB

-------------------------------------------
-- Dual Gradient Backward Generic Vector --
-------------------------------------------

newtype DNBV n a b = DNBV { unDNBV ∷ DNB a (𝕍S n b) }

deriving instance (𝒩 n,Zero b)         ⇒ ConstAD a              (DNBV n a b)
deriving instance                        SensAD  a (a → 𝕍S n b) (DNBV n a b)
deriving instance (𝒩 n,Zero a,Zero b)  ⇒ Zero                   (DNBV n a b)
deriving instance (𝒩 n,Plus a,Plus b)  ⇒ Plus                   (DNBV n a b)
deriving instance (𝒩 n,Times a,Plus b) ⇒ Times                  (DNBV n a b)

--------------------------------
-- Dual Number Backward Scalar --
--------------------------------

newtype DNBS a = DNBS { unDNBS ∷ DNB a a }

deriving instance (Zero a)         ⇒ ConstAD a         (DNBS a)
deriving instance                    SensAD  a (a → a) (DNBS a)
deriving instance (Zero a)         ⇒ Zero              (DNBS a)
deriving instance (Plus a)         ⇒ Plus              (DNBS a)
deriving instance (Plus a,Times a) ⇒ Times             (DNBS a)

---------------------------------------
-- Dual Number Backward Scalar for 𝔻 --
---------------------------------------

newtype DNB𝔻 = DNB𝔻 { unDNB𝔻 ∷ DNBS 𝔻 }

deriving instance ConstAD 𝔻         DNB𝔻
deriving instance SensAD  𝔻 (𝔻 → 𝔻) DNB𝔻
deriving instance Zero              DNB𝔻
deriving instance Plus              DNB𝔻
deriving instance Times             DNB𝔻

-----------------------------------
-- Dual Number Backward Gradient --
-----------------------------------

newtype DNBG n a = DNBG { unDNBG ∷ DNBV n a a }

deriving instance (𝒩 n,Zero a)         ⇒ ConstAD a              (DNBG n a)
deriving instance                        SensAD  a (a → 𝕍S n a) (DNBG n a)
deriving instance (𝒩 n,Zero a)         ⇒ Zero                   (DNBG n a)
deriving instance (𝒩 n,Plus a)         ⇒ Plus                   (DNBG n a)
deriving instance (𝒩 n,Plus a,Times a) ⇒ Times                  (DNBG n a)

-----------------------------------------
-- Dual Number Backward Gradient for 𝔻 --
-----------------------------------------

newtype DNBV𝔻 n = DNBV𝔻 { unDNBV𝔻 ∷ DNBV n 𝔻 𝔻 }

deriving instance (𝒩 n) ⇒ ConstAD 𝔻              (DNBV𝔻 n)
deriving instance         SensAD  𝔻 (𝔻 → 𝕍S n 𝔻) (DNBV𝔻 n)
deriving instance (𝒩 n) ⇒ Zero                   (DNBV𝔻 n)
deriving instance (𝒩 n) ⇒ Plus                   (DNBV𝔻 n)
deriving instance (𝒩 n) ⇒ Times                  (DNBV𝔻 n)

-----------------------------------
-- Dual Number Backward Jacobian --
-----------------------------------

newtype DNBJ m n a = DNBJ { unDNBJ ∷ DNBV m a (DNBG n a) }

deriving instance (𝒩 m,𝒩 n,Zero a)                ⇒ ConstAD a                       (DNBJ m n a)
deriving instance                                   SensAD  a (a → 𝕍S m (DNBG n a)) (DNBJ m n a)
deriving instance (𝒩 m,𝒩 n,Zero a)                ⇒ Zero                            (DNBJ m n a)
deriving instance (𝒩 m,𝒩 n,Plus a)                ⇒ Plus                            (DNBJ m n a)
deriving instance (𝒩 m,𝒩 n,Zero a,Plus a,Times a) ⇒ Times                           (DNBJ m n a)

-----------------------------------------
-- Dual Number Backward Jacobian for 𝔻 --
-----------------------------------------

newtype DNBJ𝔻 m n = DNBJ𝔻 { unDNBJ𝔻 ∷ DNBV m 𝔻 (DNBG n 𝔻) }

deriving instance (𝒩 m,𝒩 n) ⇒ ConstAD 𝔻                       (DNBJ𝔻 m n)
deriving instance             SensAD  𝔻 (𝔻 → 𝕍S m (DNBG n 𝔻)) (DNBJ𝔻 m n)
deriving instance (𝒩 m,𝒩 n) ⇒ Zero                            (DNBJ𝔻 m n)
deriving instance (𝒩 m,𝒩 n) ⇒ Plus                            (DNBJ𝔻 m n)
deriving instance (𝒩 m,𝒩 n) ⇒ Times                           (DNBJ𝔻 m n)

-- }}}

-----------------------------------------
-- Dual Number Forward Matrix Generic -- {{{
-----------------------------------------

type DNFMC = HasSpine :∧: AllCC 𝒩

data DNFM (ms ∷ [𝐍]) (nss ∷ [[𝐍]]) a = DNFM
  { dnfmVal ∷ 𝕄S ms a
  , dnfmScl ∷ 𝐿S nss DNFMC (𝕄S' a)
  , dnfmDer ∷ 𝐿S nss DNFMC (𝕄S' a)
  }
makeLenses ''DNFM
makePrettySum ''DNFM

-- DNFM2
-- val:    𝕄S ms
-- scl:    𝐿S nss₁
-- derval: 𝐿S nss₁
-- derscl: 𝐿S nss₂
-- derder: 𝐿S nss₂

constDNFM ∷ ∀ ms nss a. (HasSpine nss,AllC DNFMC nss,Zero a) ⇒ 𝕄S ms a → 𝐿S nss DNFMC (𝕄S' a) → DNFM ms nss a
constDNFM v s = DNFM v s zero

sensDNFM ∷ 𝕄S ms a → 𝐿S nss DNFMC (𝕄S' a) → 𝐿S nss DNFMC (𝕄S' a) → DNFM ms nss a
sensDNFM = DNFM

plusDNFM ∷ (AllC 𝒩 ms,Plus a) ⇒ DNFM ms nss a → DNFM ms nss a → DNFM ms nss a
plusDNFM (DNFM v₁ s₁ d₁) (DNFM v₂ s₂ d₂) = DNFM (v₁ + v₂) (s₁ + s₂) $ d₁ + d₂

timesDNFM ∷ (AllC 𝒩 ms,Plus a,Times a) ⇒ DNFM ms nss a → DNFM ms nss a → DNFM ms nss a
timesDNFM (DNFM v₁ s₁ d₁) (DNFM v₂ s₂ d₂) = DNFM (v₁ × v₂) (s₁ × s₂) $ d₁ × s₂ + d₂ × s₁

-- }}}

-----------------------------------------
-- Dual Number Backward Matrix Generic -- {{{
-----------------------------------------

type DNBMC = AllCC 𝒩

data DNBM (ms ∷ [𝐍]) (nss ∷ [[𝐍]]) a = DNBM
  { dnbmVal ∷ 𝕄S ms a
  , dnbmDer ∷ 𝕄S ms a → 𝐿S nss DNBMC (𝕄S' a) → 𝐿S nss DNBMC (𝕄S' a) 
  }
makeLenses ''DNBM
makePrettySum ''DNBM

-- DNBM2
-- val:   𝕄S ms
-- der:   𝕄S ms → X → X
-- X.val: 𝐿S nss₁ → 𝐿S ns₁
-- X.der: (𝐿S nss₁ → 𝐿S ns₁) → 𝐿S nss₂ → 𝐿S nss₂

constDNBM ∷ 𝕄S ms a → DNBM ms nss a
constDNBM x = DNBM x $ const id

sensDNBM ∷ 𝕄S ms a → (𝕄S ms a → 𝐿S nss DNBMC (𝕄S' a) → 𝐿S nss DNBMC (𝕄S' a)) → DNBM ms nss a
sensDNBM = DNBM

plusDNBM ∷ (AllC 𝒩 ms,Plus a) ⇒ DNBM ms nss a → DNBM ms nss a → DNBM ms nss a
plusDNBM (DNBM v₁ 𝒹₁) (DNBM v₂ 𝒹₂) = DNBM (v₁ + v₂) $ \ d → 𝒹₁ d ∘ 𝒹₂ d

timesDNBM ∷ (AllC 𝒩 ms,Times a) ⇒ DNBM ms nss a → DNBM ms nss a → DNBM ms nss a
timesDNBM (DNBM v₁ 𝒹₁) (DNBM v₂ 𝒹₂) = DNBM (v₁ × v₂) $ \ d → 𝒹₁ (d × v₂) ∘ 𝒹₂ (d × v₁)

-- }}}


