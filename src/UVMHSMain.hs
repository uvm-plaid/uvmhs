{-# OPTIONS_GHC -Wno-unused-imports #-}
module UVMHSMain where

import UVMHS

import qualified Examples.Lang.Arith as Arith
import qualified Examples.Lang.ArithBlocks as ArithBlocks
import qualified Examples.Lang.SExp as SExp

import qualified Prelude as HS

main ∷ IO ()
main = cleanExit $ do
  pprint $ ppHeader "COLOR TEST"
  pprint colorsDemo
  $$(testModules False
    [ "UVMHS.CoreTests"
    , "UVMHS.Lib.Substitution"
    ])
  pprint $ ppFG teal $ ppString "¯\\_﹙ツ﹚_/¯"

-- -- MONAD STUFF
-- --
-- -- state is threaded between continuations
-- -- M1 A ≈ (A → State S R) → State S R
-- -- M1 A ≈ (A → S → S ∧ R) → S → S ∧ R
-- -- M1 A ≈ (S ∧ A → S ∧ R) → S → S ∧ R
-- --
-- -- if we swap state with writer we get this...
-- -- M1W A ≈ (A → Writer O R) → Writer O R
-- -- M1W A ≈ (A → O ∧ R) → O ∧ R
-- -- hijack ∷ ((A → O ∧ R) → O ∧ R) → (O ∧ A → O ∧ R) → O ∧ R
-- -- ↑↑↑↑↑↑
-- -- not possible to implement in a way that does anything useful
-- newtype M1 a = M1 { unM1 ∷ ContT ℕ (StateT ℕ ID) a }
--   deriving
--   ( Return,Bind,Monad,Functor
--   , MonadState ℕ
--   , MonadCont ℕ
--   )
-- 
-- runM1 ∷ ℕ → M1 ℕ → ℕ ∧ ℕ
-- runM1 n = unID ∘ runStateT n ∘ evalContT ∘ unM1
-- 
-- -- state is passed explicitly
-- -- M2S ≈ S → Cont (S ∧ R) (S ∧ A)
-- -- M2S ≈ S → (S ∧ A → S ∧ R) → S ∧ R
-- -- M2S ≈ (S ∧ A → S ∧ R) → S → S ∧ R
-- --
-- -- M1W ≈ Cont (O ∧ R) (O ∧ A)
-- -- M1W ≈ (O ∧ A → O ∧ R) → O ∧ R
-- newtype M2 a = M2 { unM2 ∷ StateT ℕ (ContT (ℕ ∧ ℕ) ID) a }
--   deriving
--   ( Return,Bind,Monad,Functor
--   , MonadState ℕ
--   , MonadCont ℕ
--   )
-- 
-- runM2 ∷ ℕ → M2 (ℕ ∧ ℕ) → ℕ ∧ ℕ
-- runM2 n = unID ∘ evalContT ∘ map snd ∘ runStateT n ∘ unM2
-- 
-- newtype M3 a = M3 { unM3 ∷ WriterT ℕ (ContT (ℕ ∧ ℕ) ID) a }
--   deriving
--   ( Return,Bind,Monad,Functor
--   , MonadWriter ℕ
--   , MonadCont ℕ
--   )
-- 
-- runM3 ∷ M3 (ℕ ∧ ℕ) → ℕ ∧ ℕ
-- runM3 = unID ∘ evalContT ∘ map snd ∘ unWriterT ∘ unM3
-- 
-- monadTest ∷ IO ()
-- monadTest = do
--   -- state is threaded between continuations
--   pprint $ runM1 0 $ do
--     callCC $ \ 𝓀 → do
--       n₁ ← 𝓀 ()
--       n₂ ← 𝓀 ()
--       n' ← get
--       return $ n' × n₁ × n₂
--     n ← get
--     put $ n + 1
--     return $ n + 10
--   pprint $ 𝕟 2 × 10 × 11
--   pprint $ runM2 0 $ do
--     callCC $ \ 𝓀 → do
--       n₁ ← 𝓀 ()
--       n₂ ← 𝓀 ()
--       n' ← get
--       return $ n' × n₁ × n₂
--     n ← get
--     put $ n + 1 -- doesn't do anything because `map snd` in runM2
--     return $ 
--       (n + 1) -- manually "threaded" state
--       :* 
--       (n + 10)
--   pprint $ 𝕟 2 × 10 × 11
--   pprint $ runM3 $ do
--     n' :* () ← hijack $ callCC $ \ 𝓀 → do
--       n₁₁ :* n₁₂ ← hijack $ 𝓀 ()
--       n₂₁ :* n₂₂ ← hijack $ 𝓀 ()
--       pptraceM $ 𝕤 "n₁₁" :* n₁₁
--       pptraceM $ 𝕤 "n₁₂" :* n₁₂
--       pptraceM $ 𝕤 "n₂₁" :* n₂₁
--       pptraceM $ 𝕤 "n₂₂" :* n₂₂
--       tell 1
--       return $ n₁₂ × n₂₂
--     pptraceM $ 𝕤 "n'" :* n'
--     tell 1000 -- doesn't do anything because `map snd` in runM3
--     return $ 
--       2
--       :* 
--       10
--   
-- 
-- callCC' 
--   ∷ ∀ o r m a.
--     (Monoid o,Monad m,MonadCont r m) 
--   ⇒ ((a → WriterT o m r) → WriterT o m r) → WriterT o m a
-- callCC' kk = WriterT $ callCC $ \ (k ∷ (o ∧ a) → m r) → do
--   _o :* x ← unWriterT $ kk $ \ (x ∷ a) →
--     WriterT $ (null :*) ^$ k $ null :* x
--   return x
