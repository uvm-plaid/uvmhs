module UVMHS.CoreTests (g__TESTS__UVMHS__CoreTests) where

import UVMHS.Core
import UVMHS.Lib.Testing

𝔱 "core:iter" [| isEmpty []           |] [| True  |]
𝔱 "core:iter" [| isEmpty [𝕟 1]        |] [| False |]
𝔱 "core:iter" [| isEmpty Nil          |] [| True  |]
𝔱 "core:iter" [| isEmpty (𝕟 1 :& Nil) |] [| False |]

𝔱 "core:iter" [| list $ range (𝕟 0) (𝕟 0) |] [| list [] |]
𝔱 "core:iter" [| list $ range (𝕟 1) (𝕟 1) |] [| list [] |]
𝔱 "core:iter" [| list $ range (𝕟 0) (𝕟 1) |] [| list [𝕟 0] |]
𝔱 "core:iter" [| list $ range (𝕟 0) (𝕟 2) |] [| list [𝕟 0,𝕟 1] |]
𝔱 "core:iter" [| list $ range (𝕟 1) (𝕟 3) |] [| list [𝕟 1,𝕟 2] |]

𝔱 "core:iter" [| list $ upTo (𝕟 0) |] [| list []  |]
𝔱 "core:iter" [| list $ upTo (𝕟 1) |] [| list [𝕟 0] |]
𝔱 "core:iter" [| list $ upTo (𝕟 2) |] [| list [𝕟 0,𝕟 1] |]

𝔱 "core:iter" [| list $ keepN (𝕟 0) [𝕟 0,𝕟 1] |] [| list [] |]
𝔱 "core:iter" [| list $ keepN (𝕟 1) [𝕟 0,𝕟 1] |] [| list [𝕟 0] |]
𝔱 "core:iter" [| list $ keepN (𝕟 2) [𝕟 0,𝕟 1] |] [| list [𝕟 0,𝕟 1] |]
𝔱 "core:iter" [| list $ keepN (𝕟 3) [𝕟 0,𝕟 1] |] [| list [𝕟 0,𝕟 1] |]

𝔱 "core:iter" [| list $ replicate (𝕟 0) $ 𝕟 42 |] [| list [] |]
𝔱 "core:iter" [| list $ replicate (𝕟 2) $ 𝕟 42 |] [| list [𝕟 42,𝕟 42] |]

𝔱 "core:dict" [| dict [𝕟 1 ↦ 𝕟 2,𝕟 1 ↦ 𝕟 3] |] [| dict [𝕟 1 ↦ 𝕟 2] |]

𝔱 "core:lens" [| alter (keyL $ 𝕤 "x") (map (+ 𝕟 1)) ("x" ↦ 𝕟 1) |] [| "x" ↦ 𝕟 2 |]
𝔱 "core:lens" [| alter (keyL $ 𝕤 "x") (map (+ 𝕟 1)) ("y" ↦ 𝕟 1) |] [| "y" ↦ 𝕟 1 |]
𝔱 "core:lens" [| alter (keyL $ 𝕤 "x") (map (+ 𝕟 1)) (dict ["x" ↦ 𝕟 10,"y" ↦ 𝕟 20]) |] 
              [| dict ["x" ↦ 𝕟 11,"y" ↦ 𝕟 20] |]
𝔱 "core:lens" [| alter (keyL $ 𝕤 "x") (const None) ("x" ↦ 𝕟 1) |] [| dø |]
𝔱 "core:lens" [| alter (keyL $ 𝕤 "x") (const None) ("y" ↦ 𝕟 1) |] [| "y" ↦ 𝕟 1 |]
𝔱 "core:lens" [| alter (keyL $ 𝕤 "x") (const None) (dict ["x" ↦ 𝕟 10,"y" ↦ 𝕟 20]) |] 
              [| dict ["y" ↦ 𝕟 20] |]

newtype CR a = CR { unCR ∷ ContT ℕ64 (ReaderT (ℕ64 ∧ ℕ64) ID) a }
  deriving
  ( Return,Bind,Functor,Monad
  , MonadCont ℕ64
  , MonadReader (ℕ64 ∧ ℕ64)
  )

runCR ∷ ℕ64 → ℕ64 → CR ℕ64 → ℕ64
runCR x y xM = unID $ runReaderT (x :* y) $ evalContT $ unCR xM

𝔱 "core:monads:cr" [| 1  |] [| runCR 1 2 $ do fst ^$ ask |]
𝔱 "core:monads:cr" [| 2  |] [| runCR 1 2 $ do snd ^$ ask |]
𝔱 "core:monads:cr" [| 30 |] [| runCR 1 2 $ do putEnv $ 10 :* 20 ; x :* y ← ask ; return $ x + y |]
𝔱 "core:monads:cr" [| 12 |] [| runCR 1 2 $ do putEnvL fstL 10 ; x :* y ← ask ; return $ x + y |]
𝔱 "core:monads:cr" [| 12 |] [| runCR 1 2 $ do putEnvL fstL 10 ; reset (do x :* y ← ask ; return $ x + y) |]
𝔱 "core:monads:cr" [| 3  |] [| runCR 1 2 $ do _←reset $ (do putEnvL fstL 10;return 0);x:*y←ask;return $ x + y |]
𝔱 "core:monads:cr" [| 110  |] 
  [| runCR 1 2 $ do putEnvL fstL 10;x ← reset $ (do putEnvL fstL 100;askL fstL);y←askL fstL;return $ x + y |]

newtype UR a = UR { unUR ∷ UContT (ReaderT (ℕ64 ∧ ℕ64) ID) a }
  deriving
  ( Return,Bind,Functor,Monad
  , MonadUCont
  , MonadReader (ℕ64 ∧ ℕ64)
  )

runUR ∷ ℕ64 → ℕ64 → UR ℕ64 → ℕ64
runUR x y xM = unID $ runReaderT (x :* y) $ evalUContT $ unUR xM

𝔱 "core:monads:ur" [| 1 |] [| runUR 1 2 $ do fst ^$ ask |]
𝔱 "core:monads:ur" [| 2 |] [| runUR 1 2 $ do snd ^$ ask |]
𝔱 "core:monads:ur" [| 30 |] [| runUR 1 2 $ do uputEnv $ 10 :* 20 ; x :* y ← ask ; return $ x + y |]
𝔱 "core:monads:ur" [| 12 |] [| runUR 1 2 $ do uputEnvL fstL 10 ; x :* y ← ask ; return $ x + y |]
𝔱 "core:monads:ur" [| 12 |] [| runUR 1 2 $ do uputEnvL fstL 10 ; ureset (do x :* y ← ask ; return $ x + y) |]
𝔱 "core:monads:ur" [| 3  |] [| runUR 1 2 $ do _←ureset $ (do uputEnvL fstL 10;return 0);x:*y←ask;return $ x + y |]
𝔱 "core:monads:ur" [| 110  |]
  [| runUR 1 2 $ do uputEnvL fstL 10;x ← ureset $ (do uputEnvL fstL 100;askL fstL);y←askL fstL;return $ x + y |]

buildTests
