module UVMHS.Lib.Window where

import UVMHS.Core

import UVMHS.Lib.Pretty

-------------
-- WindowL --
-------------

data WindowL i a =
    ZerWindowL a
  | OneWindowL 𝔹 a i a

eWindowL ∷ a → WindowL i a
eWindowL = ZerWindowL

iWindowL ∷ (Null a) ⇒ i → WindowL i a
iWindowL i = OneWindowL False null i null

overflowL ∷ WindowL i a → 𝔹
overflowL (ZerWindowL _) = False
overflowL (OneWindowL o _ _ _) = o

instance (Null a) ⇒ Null (WindowL i a) where 
  null = ZerWindowL null
instance (Append a) ⇒ Append (WindowL i a) where
  ZerWindowL x ⧺ ZerWindowL y = ZerWindowL $ x ⧺ y
  ZerWindowL x ⧺ OneWindowL o y i z = OneWindowL o (x ⧺ y) i z
  OneWindowL True x i y ⧺ _ = OneWindowL True x i y
  OneWindowL False x i y ⧺ ZerWindowL z = OneWindowL False x i $ y ⧺ z
  OneWindowL False x i y ⧺ OneWindowL _ z _ _ = OneWindowL True x i $ y ⧺ z
instance (Monoid a) ⇒ Monoid (WindowL i a) 

instance ToIter a (WindowL a a) where
  iter (ZerWindowL x) = single x
  iter (OneWindowL _ x i y) = iter [x,i,y]

mapWindowL ∷ (i → j) → (a → b) → WindowL i a → WindowL j b
mapWindowL _ f (ZerWindowL x) = ZerWindowL $ f x
mapWindowL g f (OneWindowL o x i y) = OneWindowL o (f x) (g i) $ f y

-------------
-- WindowR --
-------------

data WindowR i a =
    ZerWindowR a
  | OneWindowR 𝔹 a i a

eWindowR ∷ a → WindowR i a
eWindowR = ZerWindowR

iWindowR ∷ (Null a) ⇒ i → WindowR i a
iWindowR i = OneWindowR False null i null

overflowR ∷ WindowR i a → 𝔹
overflowR (ZerWindowR _) = False
overflowR (OneWindowR o _ _ _) = o

instance (Null a) ⇒ Null (WindowR i a) where 
  null = ZerWindowR null
instance (Append a) ⇒ Append (WindowR i a) where
  ZerWindowR x ⧺ ZerWindowR y = ZerWindowR $ x ⧺ y
  OneWindowR o x i y ⧺ ZerWindowR z = OneWindowR o x i $ y ⧺ z
  _ ⧺ OneWindowR True x i y = OneWindowR True x i y
  ZerWindowR x ⧺ OneWindowR False y i z = OneWindowR False (x ⧺ y) i z
  OneWindowR _ _ _ x ⧺ OneWindowR False y i z = OneWindowR True (x ⧺ y) i z
instance (Monoid a) ⇒ Monoid (WindowR i a) 

instance ToIter a (WindowR a a) where
  iter (ZerWindowR x) = single x
  iter (OneWindowR _ x i y) = iter [x,i,y]

mapWindowR ∷ (i → j) → (a → b) → WindowR i a → WindowR j b
mapWindowR _ f (ZerWindowR x) = ZerWindowR $ f x
mapWindowR g f (OneWindowR o x i y) = OneWindowR o (f x) (g i) $ f y

makePrettySum ''WindowL
makePrettySum ''WindowR

-- RENDER --

renderWindowL ∷ WindowL Doc Doc → Doc
renderWindowL dL
  | overflowL dL = ppVertical
      [ concat dL
      , ppFormat (formats [BG lightGray]) $ ppString "…"
      ]
  | otherwise = concat dL

renderWindowR ∷ WindowR Doc Doc → Doc
renderWindowR dR
  | overflowR dR = ppVertical
      [ ppFormat (formats [BG lightGray]) $ ppString "…"
      , concat dR
      ]
  | otherwise = concat dR
        
-- import UVMHS.Core

-- ------------
-- -- Swivel --
-- ------------
-- 
-- swivelL ∷ 𝐿 a → a → a ∧ 𝐿 a
-- swivelL Nil x = x :* Nil
-- swivelL (x :& xs) y =
--   let x' :* xs' = swivelL xs y
--   in x :* (x' :& xs')
-- 
-- swivelR ∷ a → 𝐿 a → 𝐿 a ∧ a
-- swivelR x Nil = Nil :* x
-- swivelR x (y :& xs) =
--   let xs' :* x' = swivelR y xs
--   in (x :& xs') :* x'
-- 
-- iswivelL ∷ 𝐿 (a ∧ i) → a → a ∧ 𝐿 (i ∧ a)
-- iswivelL Nil x = x :* Nil
-- iswivelL ((x :* i) :& xis) y =
--   let x' :* ixs = iswivelL xis y
--   in x :* ((i :* x') :& ixs)
-- 
-- iswivelR ∷ a → 𝐿 (i ∧ a) → 𝐿 (a ∧ i) ∧ a
-- iswivelR x Nil = Nil :* x
-- iswivelR x ((i :* y) :& ixs) =
--   let xis :* x' = iswivelR y ixs
--   in ((x :* i) :& xis) :* x'
-- 
-- ------------
-- -- Window --
-- ------------
-- 
-- data Window i a = 
--     WindowE a
--   | WindowS ℕ64 a i (𝐼 (a ∧ i)) a
-- 
-- windowI ∷ (Null a) ⇒ i → Window i a
-- windowI i = WindowS one null i null null
-- 
-- instance (Null a) ⇒ Null (Window i a) where null = WindowE null
-- instance (Append a) ⇒ Append (Window i a) where
--   WindowE x₁ ⧺ WindowE x₂ = WindowE $ x₁ ⧺ x₂
--   WindowE x₁ ⧺ WindowS n x₂₁ i₂ xis₂ x₂₂ = WindowS n (x₁ ⧺ x₂₁) i₂ xis₂ x₂₂
--   WindowS n x₁₁ i₁ xis₁ x₁₂ ⧺ WindowE x₂ = WindowS n x₁₁ i₁ xis₁ $ x₁₂ ⧺ x₂
--   WindowS n₁ x₁₁ i₁ xis₁ x₁₂ ⧺ WindowS n₂ x₂₁ i₂ xis₂ x₂₂ = 
--     let xis' = xis₁ ⧺ single ((x₁₁ ⧺ x₂₁) :* i₂) ⧺ xis₂
--     in WindowS (n₁ + n₂) x₁₁ i₁ xis' x₂₂
-- instance (Monoid a) ⇒ Monoid (Window i a)
-- 
-- -------------
-- -- FWindow --
-- -------------
-- 
-- windowWidth ∷ ℕ64
-- windowWidth = 𝕟64 2
-- 
-- data FWindow i a = 
--     FWindowE a
--   | FWindowS ℕ64 a i (𝐿 (a ∧ i)) a
-- 
-- fwindowI ∷ (Null a) ⇒ i → FWindow i a
-- fwindowI i = FWindowS one null i null null
-- 
-- instance (Null a) ⇒ Null (FWindow i a) where null = FWindowE null
-- instance (Append a) ⇒ Append (FWindow i a) where
--   FWindowE x₁ ⧺ FWindowE x₂ = FWindowE $ x₁ ⧺ x₂
--   FWindowE x₁ ⧺ FWindowS n x₂₁ i₂ xis₂ x₂₂ = FWindowS n (x₁ ⧺ x₂₁) i₂ xis₂ x₂₂
--   FWindowS n x₁₁ i₁ xis₁ x₁₂ ⧺ FWindowE x₂ = FWindowS n x₁₁ i₁ xis₁ $ x₁₂ ⧺ x₂
--   FWindowS n₁ x₁₁ i₁ xis₁ x₁₂ ⧺ FWindowS n₂ x₂₁ i₂ xis₂ x₂₂ = 
--     | n₂ ≡ windowWidth + 1 = FWindowS n₂ (x₁₂ ⧺ x₂₁) i₂ xis₂ x₂₂
--     let n = n₁ + n₂
--     in case n > (windowWidth + one) of
--       True →
--         let xis :* xi = swivelR (x₁₁ :* i₁) $ xis₁ ⧺ ((x₁₂ ⧺ x₂₁) :* i₂) :& xis₂
--             (x' :* i') :* xis' = swivelL (lastN windowWidth xis) xi
--         in FWindowS (windowWidth + one) x' i' xis' x₂₂
--       False →
--         let xis = xis₁ ⧺ ((x₁₂ ⧺ x₂₁) :* i₂) :& xis₂
--         in FWindowS n x₁₁ i₁ xis x₂₂
-- instance (Monoid a) ⇒ Monoid (FWindow i a)
-- 
-- data LWindow i a = 
--     LWindowE a
--   | LWindowS ℕ64 a i (𝐼 (a ∧ i)) a
-- 
-- lwindowI ∷ (Null a) ⇒ i → LWindow i a
-- lwindowI i = LWindowS null i null null
-- 
-- instance (Null a) ⇒ Null (LWindow i a) where null = LWindowE null
-- instance (Append a) ⇒ Append (LWindow i a) where
--   LWindowE x₁ ⧺ LWindowE x₂ = LWindowE $ x₁ ⧺ x₂
--   LWindowE x₁ ⧺ LWindowS x₂₁ i₂ xis₂ x₂₂ = LWindowS (x₁ ⧺ x₂₁) i₂ xis₂ x₂₂
--   LWindowS x₁₁ i₁ xis₁ x₁₂ ⧺ LWindowE x₂ = LWindowS x₁₁ i₁ xis₁ $ x₁₂ ⧺ x₂
--   LWindowS x₁₁ i₁ xis₁ x₁₂ ⧺ LWindowS x₂₁ i₂ xis₂ x₂₂ = 
--     let xis' = reverse $ iter $ firstN (nat windowWidth) $ list $ reverse $ xis₁ ⧺ single ((x₁₁ ⧺ x₂₁) :* i₂) ⧺ xis₂
--     in LWindowS x₁₁ i₁ xis' x₂₂
-- instance (Monoid a) ⇒ Monoid (LWindow i a)
-- 
-- -- data WindowR i a = WindowR 
-- --   { windowRHead ∷ 𝐿 (a ∧ i)
-- --   , windowRTail ∷ a
-- --   } deriving (Eq,Ord,Show)
-- -- 
-- -- instance (Null a) ⇒ Null (WindowR i a) where null = WindowR null null
-- -- instance (Append a) ⇒ Append (WindowR i a) where
-- --   WindowR xss₁ x₁ ⧺ WindowR xss₂ x₂ = case xxs₂ of
-- --     Nil → WindowL xss₁ (x₁ ⧺ x₂)
-- --     (x₂' :* s₂) :& xss₂' → WindowL (xss₁ ⧺ ((x₁ ⧺ x₂') :* xss₂)) x₂
-- -- instance (Monoid a) ⇒ Monoid (WindowL i a)
-- -- instance ToStream a (SepL a a) where 
-- --   stream (WindowL x sxs) = stream $ list $ concat
-- --     [ single x 
-- --     , concat $ mapOn (reverse sxs) $ \ (i' :* x') → 
-- --         iter [i',x']
-- --     ]
-- -- instance ToIter a (WindowL a a) where iter = iter ∘ stream
-- -- 
-- -- data WindowL i a = WindowL 
-- --   { windowLHead ∷ a
-- --   , windowLTail ∷ 𝐿 (i ∧ a)
-- --   } deriving (Eq,Ord,Show)
-- -- 
-- -- instance (Null a) ⇒ Null (WindowL i a) where null = WindowL null null
-- -- instance (Append a) ⇒ Append (WindowL i a) where
-- --   WindowL x₁ sxs₁ ⧺ WindowL x₂ sxs₂ = case sxs₁ of
-- --     Nil → WindowL (x₁ ⧺ x₂) sxs₂
-- --     (s₁ :* x₁') :& sxs₁' → WindowL x₁ $ firstN windowWidth $ sxs₂ ⧺ ((s₁ :* (x₁' ⧺ x₂)) :& sxs₁)
-- -- instance (Monoid a) ⇒ Monoid (WindowL i a)
-- -- instance ToStream a (WindowL a a) where 
-- --   stream (WindowL x sxs) = stream $ list $ concat
-- --     [ single x 
-- --     , concat $ mapOn (reverse sxs) $ \ (i' :* x') → 
-- --         iter [i',x']
-- --     ]
-- -- instance ToIter a (WindowL a a) where iter = iter ∘ stream
-- -- 

