module UVMHS.Tests.Core (g__TESTS__UVMHS__Tests__Core) where

import UVMHS.Core
import UVMHS.Lib.Testing

𝔱 "core:iter" [| isEmpty []           |] [| True  |]
𝔱 "core:iter" [| isEmpty [𝕟 1]        |] [| False |]
𝔱 "core:iter" [| isEmpty Nil          |] [| True  |]
𝔱 "core:iter" [| isEmpty (𝕟 1 :& Nil) |] [| False |]

𝔱 "core:iter" [| list $ range (𝕟 0) (𝕟 0) |] [| list []        |]
𝔱 "core:iter" [| list $ range (𝕟 1) (𝕟 1) |] [| list []        |]
𝔱 "core:iter" [| list $ range (𝕟 0) (𝕟 1) |] [| list [𝕟 0]     |]
𝔱 "core:iter" [| list $ range (𝕟 0) (𝕟 2) |] [| list [𝕟 0,𝕟 1] |]
𝔱 "core:iter" [| list $ range (𝕟 1) (𝕟 3) |] [| list [𝕟 1,𝕟 2] |]

𝔱 "core:iter" [| list $ upto (𝕟 0) |] [| list []        |]
𝔱 "core:iter" [| list $ upto (𝕟 1) |] [| list [𝕟 0]     |]
𝔱 "core:iter" [| list $ upto (𝕟 2) |] [| list [𝕟 0,𝕟 1] |]

𝔱 "core:iter" [| list $ keepN (𝕟 0) [𝕟 0,𝕟 1] |] [| list []        |]
𝔱 "core:iter" [| list $ keepN (𝕟 1) [𝕟 0,𝕟 1] |] [| list [𝕟 0]     |]
𝔱 "core:iter" [| list $ keepN (𝕟 2) [𝕟 0,𝕟 1] |] [| list [𝕟 0,𝕟 1] |]
𝔱 "core:iter" [| list $ keepN (𝕟 3) [𝕟 0,𝕟 1] |] [| list [𝕟 0,𝕟 1] |]

𝔱 "core:iter" [| list $ replicate (𝕟 0) $ 𝕟 42 |] [| list [] |]
𝔱 "core:iter" [| list $ replicate (𝕟 2) $ 𝕟 42 |] [| list [𝕟 42,𝕟 42] |]

𝔱 "core:dict" [| dict𝐷 [𝕟 1 ↦♭ 𝕟 2,𝕟 1 ↦♭ 𝕟 3] |] [| dict𝐷 [𝕟 1 ↦♭ 𝕟 2] |]

𝔱 "core:lens" [| alter (keyL $ 𝕤 "x") (map (+ 𝕟 1)) ("x" ↦♭ 𝕟 1) |] [| "x" ↦♭ 𝕟 2 |]
𝔱 "core:lens" [| alter (keyL $ 𝕤 "x") (map (+ 𝕟 1)) ("y" ↦♭ 𝕟 1) |] [| "y" ↦♭ 𝕟 1 |]
𝔱 "core:lens" [| alter (keyL $ 𝕤 "x") (map (+ 𝕟 1)) (dict𝐷 ["x" ↦♭ 𝕟 10,"y" ↦♭ 𝕟 20]) |] 
              [| dict𝐷 ["x" ↦♭ 𝕟 11,"y" ↦♭ 𝕟 20] |]
𝔱 "core:lens" [| alter (keyL $ 𝕤 "x") (const None) ("x" ↦♭ 𝕟 1) |] [| dø𝐷 |]
𝔱 "core:lens" [| alter (keyL $ 𝕤 "x") (const None) ("y" ↦♭ 𝕟 1) |] [| "y" ↦♭ 𝕟 1 |]
𝔱 "core:lens" [| alter (keyL $ 𝕤 "x") (const None) (dict𝐷 ["x" ↦♭ 𝕟 10,"y" ↦♭ 𝕟 20]) |] 
              [| dict𝐷 ["y" ↦♭ 𝕟 20] |]

newtype CR a = CR { unCR ∷ ContT ℕ64 (ReaderT (ℕ64 ∧ ℕ64) ID) a }
  deriving
  ( Return,Bind,Functor,Monad
  , MonadCont ℕ64
  , MonadReader (ℕ64 ∧ ℕ64)
  )

runCR ∷ ℕ64 → ℕ64 → CR ℕ64 → ℕ64
runCR x y xM = unID $ runReaderT (x :* y) $ evalContT $ unCR xM

execCR ∷ CR ℕ64 → ℕ64
execCR = runCR 0 0 

𝔱 "core:monads:cr" [| 0   |] [| execCR $ do fst ^$ ask |]
𝔱 "core:monads:cr" [| 0   |] [| execCR $ do snd ^$ ask |]
𝔱 "core:monads:cr" [| 30  |] [| execCR $ do putEnv $ 10 :* 20 ; x :* y ← ask ; return $ x + y |]
𝔱 "core:monads:cr" [| 10  |] [| execCR $ do putEnvL fstL 10 ; x :* y ← ask ; return $ x + y |]
𝔱 "core:monads:cr" [| 10  |] [| execCR $ do putEnvL fstL 10 ; reset (do x :* y ← ask ; return $ x + y) |]
𝔱 "core:monads:cr" [| 0   |] [| execCR $ do _←reset $ (do putEnvL fstL 10;return $ 𝕟64 0);x:*y←ask;return $ x + y |]
𝔱 "core:monads:cr" [| 110 |] 
  [| execCR $ do putEnvL fstL 10;x ← reset $ (do putEnvL fstL 100;askL fstL);y←askL fstL;return $ x + y |]
-- Note: this is why MonadReader has askL/localL as primitives, and not ask/local
𝔱 "core:monads:cr" [| 2 |] [| execCR $ do localL fstL 1 $ putEnvL sndL 2 ; askL sndL |]

newtype UR a = UR { unUR ∷ UContT (ReaderT (ℕ64 ∧ ℕ64) ID) a }
  deriving
  ( Return,Bind,Functor,Monad
  , MonadUCont
  , MonadReader (ℕ64 ∧ ℕ64)
  )

runUR ∷ ℕ64 → ℕ64 → UR ℕ64 → ℕ64
runUR x y xM = unID $ runReaderT (x :* y) $ evalUContT $ unUR xM

execUR ∷ UR ℕ64 → ℕ64
execUR = runUR 0 0 

𝔱 "core:monads:ur" [| 0   |] [| execUR $ do fst ^$ ask |]
𝔱 "core:monads:ur" [| 0   |] [| execUR $ do snd ^$ ask |]
𝔱 "core:monads:ur" [| 30  |] [| execUR $ do uputEnv $ 10 :* 20 ; x :* y ← ask ; return $ x + y |]
𝔱 "core:monads:ur" [| 10  |] [| execUR $ do uputEnvL fstL 10 ; x :* y ← ask ; return $ x + y |]
𝔱 "core:monads:ur" [| 10  |] [| execUR $ do uputEnvL fstL 10 ; ureset (do x :* y ← ask ; return $ x + y) |]
𝔱 "core:monads:ur" [| 0   |] [| execUR $ do _←ureset $ (do uputEnvL fstL 10;return $ 𝕟64 0);x:*y←ask;return $ x + y |]
𝔱 "core:monads:ur" [| 110 |]
  [| execUR $ do uputEnvL fstL 10;x ← ureset $ (do uputEnvL fstL 100;askL fstL);y←askL fstL;return $ x + y |]
-- Note: this is why MonadReader has askL/localL as primitives, and not ask/local
𝔱 "core:monads:ur" [| 2 |] [| execUR $ do localL fstL 1 $ uputEnvL sndL 2 ; askL sndL |]

buildTests

