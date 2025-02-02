--------
-- 𝕄S --
--------

-- newtype 𝕄S m n a = 𝕄S_UNSAFE { un𝕄S ∷ M.Matrix a }
--
-- deriving instance (M.Container M.Matrix a) ⇒ Eq (𝕄S m n a)
--
-- instance (Element a) ⇒ ToStream a (𝕄S m n a)                              where {-# INLINE stream #-} ; stream = stream𝕄S
-- instance (Element a) ⇒ ToIter a (𝕄S m n a)                                where {-# INLINE iter   #-} ; iter   = iter ∘ stream
-- instance (Element a,Show a) ⇒ Show (𝕄S m n a)                             where {-# INLINE show   #-} ; show   = chars ∘ show𝕄S
-- instance (M.Container M.Matrix a) ⇒ Access (𝕀64 m,𝕀64 n) a (𝕄S m n a)       where {-# INLINE (⋕)    #-} ; (⋕)    = flip $ curry idx𝕄S ∘ frhs
-- instance (𝒩 m,𝒩 n,M.Container M.Matrix a) ⇒ Lookup (ℕ64,ℕ64) a (𝕄S m n a) where {-# INLINE (⋕?)   #-} ; (⋕?)   = flip $ curry idxChecked𝕄S ∘ frhs
-- instance (𝒩 m,𝒩 n,Element a,Null a) ⇒ Null (𝕄S m n a)                   where {-# INLINE null   #-} ; null   = null𝕄S
--
-- {-# INLINE smat #-}
-- smat ∷ ∀ m n a. (𝒩 m,𝒩 n,Element a) ⇒ 𝐼S m (𝐼S n a) → 𝕄S m n a
-- smat xs = 𝕄S_UNSAFE $ ((tohs $ intΩ64 $ unℕ64S $ 𝕟64s @ m) M.>< (tohs $ intΩ64 $ unℕ64S $ 𝕟64s @ n)) $ lazyList $ concat $ un𝐼S $ map un𝐼S xs
--
-- {-# INLINE smatF #-}
-- smatF ∷ ∀ m n a. (𝒩 m,𝒩 n,Element a) ⇒ (𝕀64 m → 𝕀64 n → a) → 𝕄S m n a
-- smatF f = smat $ mapOn upTo𝕀64 $ \ i → mapOn upTo𝕀64 $ \ j → f i j
--
-- {-# INLINE idx𝕄S #-}
-- idx𝕄S ∷ (M.Container M.Matrix a) ⇒ 𝕀64 m → 𝕀64 n → 𝕄S m n a → a
-- idx𝕄S i j xs = M.atIndex (un𝕄S xs) (tohs $ intΩ64 $ un𝕀64 i,tohs $ intΩ64 $ un𝕀64 j)
--
-- {-# INLINE idxChecked𝕄S #-}
-- idxChecked𝕄S ∷ ∀ m n a. (𝒩 m,𝒩 n,M.Container M.Matrix a) ⇒ ℕ64 → ℕ64 → 𝕄S m n a → 𝑂 a
-- idxChecked𝕄S i j xs = do
--   i' ← 𝕚64d i
--   j' ← 𝕚64d j
--   return $ idx𝕄S i' j' xs
--
-- {-# INLINE iter𝕄S #-}
-- iter𝕄S ∷ (Element a) ⇒ 𝕄S m n a → 𝐼S m (𝐼S n a)
-- iter𝕄S xs = map (𝐼S_UNSAFE ∘ iter) $ 𝐼S_UNSAFE $ iter $ M.toLists $ un𝕄S xs
--
-- {-# INLINE stream𝕄S #-}
-- stream𝕄S ∷ (Element a) ⇒ 𝕄S m n a → 𝑆 a
-- stream𝕄S xs = stream $ M.toList $ M.flatten $ un𝕄S xs
--
-- {-# INLINE show𝕄S #-}
-- show𝕄S ∷ (Element a,Show a) ⇒ 𝕄S m n a → 𝕊
-- show𝕄S = showCollection "𝕄S[" "]" "," show𝕊 ∘ iter
--
-- {-# INLINE null𝕄S #-}
-- null𝕄S ∷ (𝒩 m,𝒩 n,Element a,Null a) ⇒ 𝕄S m n a
-- null𝕄S = smatF $ const $ const null
--
-- {-# INLINE map𝕄S #-}
-- map𝕄S ∷ (𝒩 m,𝒩 n,Element a,Element b) ⇒ (a → b) → 𝕄S m n a → 𝕄S m n b
-- map𝕄S f = smat ∘ mapp f ∘ iter𝕄S
--
-- {-# INLINE (✖) #-}
-- (✖) ∷ (M.Numeric a) ⇒ 𝕄S m n a → 𝕄S n o a → 𝕄S m o a
-- xs ✖ ys = 𝕄S_UNSAFE $ un𝕄S xs M.<> un𝕄S ys
--
-- {-# INLINE (✖♯) #-}
-- (✖♯) ∷ (M.Numeric a) ⇒ 𝕄S m n a → 𝕌S n a → 𝕌S m a
-- xs ✖♯ ys = 𝕌S_UNSAFE $ un𝕄S xs M.#> un𝕌S ys
--
-- {-# INLINE (♯✖) #-}
-- (♯✖) ∷ (M.Numeric a) ⇒ 𝕌S m a → 𝕄S m n a → 𝕌S n a
-- xs ♯✖ ys = 𝕌S_UNSAFE $ un𝕌S xs M.<# un𝕄S ys
--
-- {-# INLINE 𝐭 #-}
-- 𝐭 ∷ (M.Transposable (M.Matrix a) (M.Matrix a)) ⇒ 𝕄S m n a → 𝕄S n m a
-- 𝐭 xs = 𝕄S_UNSAFE $ M.tr $ un𝕄S xs
--
-- {-# INLINE mrow #-}
-- mrow ∷ (Storable a) ⇒ 𝕌S n a → 𝕄S 1 n a
-- mrow xs = 𝕄S_UNSAFE $ M.asRow $ un𝕌S xs
--
-- {-# INLINE mcol #-}
-- mcol ∷ (Storable a) ⇒ 𝕌S n a → 𝕄S n 1 a
-- mcol xs = 𝕄S_UNSAFE $ M.asColumn $ un𝕌S xs
--
-- {-# INLINE plus𝕄S #-}
-- plus𝕄S ∷ (M.Container M.Matrix a,HS.Num (M.Vector a),HS.Num a) ⇒ 𝕄S m n a → 𝕄S m n a → 𝕄S m n a
-- plus𝕄S xs ys = 𝕄S_UNSAFE $ un𝕄S xs HS.+ un𝕄S ys
--
-- {-# INLINE times𝕄S #-}
-- times𝕄S ∷ (M.Container M.Matrix a,HS.Num (M.Vector a),HS.Num a) ⇒ 𝕄S m n a → 𝕄S m n a → 𝕄S m n a
-- times𝕄S xs ys = 𝕄S_UNSAFE $ un𝕄S xs HS.+ un𝕄S ys
--
-- {-# INLINE div𝕄S #-}
-- div𝕄S ∷ (M.Container M.Matrix a,HS.Num (M.Vector a),HS.Num a) ⇒ 𝕄S m n a → 𝕄S m n a → 𝕄S m n a
-- div𝕄S xs ys = 𝕄S_UNSAFE $ un𝕄S xs HS.+ un𝕄S ys
--
-- instance (M.Container M.Matrix a,HS.Num (M.Vector a),HS.Num a) ⇒ Plus   (𝕄S m n a) where {-# INLINE (+) #-} ; (+) = plus𝕄S
-- instance (M.Container M.Matrix a,HS.Num (M.Vector a),HS.Num a) ⇒ Times  (𝕄S m n a) where {-# INLINE (×) #-} ; (×) = times𝕄S
-- instance (M.Container M.Matrix a,HS.Num (M.Vector a),HS.Num a) ⇒ Divide (𝕄S m n a) where {-# INLINE (/) #-} ; (/) = div𝕄S

-- -------
-- -- 𝕄 --
-- -------
--
-- newtype 𝕄 (ns ∷ [𝐍]) (a ∷ ★) = 𝕄 { un𝕄 ∷ 𝕌S (Prod ns) a }
--
-- idx𝕄I ∷ 𝐿S ns ℕ64S → 𝐿S ns 𝕀64 → ℕ64S (Prod ns) ∧ 𝕀64 (Prod ns)
-- idx𝕄I ns is = case (ns,is) of
--   (NilS,NilS) → oneS :* 𝕚64 zeroS
--   ((n ∷ ℕ64S n) :&& (ns' ∷ 𝐿S ns' ℕ64S),(i ∷ 𝕀64 n) :&& (is' ∷ 𝐿S ns' 𝕀64)) →
--     let n' :* i' = idx𝕄I ns' is'
--     in
--     -- want i * (prod ns) + idx𝕄I is
--     𝕟64di i $ \ (ni ∷ ℕ64S ni) →
--     -- ni < n
--     𝕟64di i' $ \ (nis' ∷ ℕ64S nis') →
--     -- is' < Prod ns
--     let i'' ∷ ℕ64S (ni × Prod ns' + nis')
--         i'' = ni ×♮ n' +♮ nis'
--     in
--     with (wnlt_UNSAFE @ (ni × Prod ns' + nis') @ (n × Prod ns') P P) $
--     (n ×♮ n') :* 𝕚64 i''
--
-- idx𝕄 ∷ (Storable a) ⇒ 𝐿S ns ℕ64S → 𝐿S ns 𝕀64 → 𝕌S (Prod ns) a → a
-- idx𝕄 ns is xs = xs ⋕ snd (idx𝕄I ns is)
