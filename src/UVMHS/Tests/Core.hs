module UVMHS.Tests.Core (g__TESTS__UVMHS__Tests__Core) where

import UVMHS.Core
import UVMHS.Lib.Testing

----------
-- ITER --
----------

𝔱 "core:iter:isEmpty" [| isEmpty $ id @(𝐼 ℕ) $ iter []  |] [| True  |]
𝔱 "core:iter:isEmpty" [| isEmpty $ id @(𝐼 ℕ) $ iter [1] |] [| False |]

𝔱 "core:iter:range" [| id @(𝐼 ℕ) $ range 0 0 |] [| iter []    |]
𝔱 "core:iter:range" [| id @(𝐼 ℕ) $ range 1 1 |] [| iter []    |]
𝔱 "core:iter:range" [| id @(𝐼 ℕ) $ range 0 1 |] [| iter [0]   |]
𝔱 "core:iter:range" [| id @(𝐼 ℕ) $ range 0 2 |] [| iter [0,1] |]
𝔱 "core:iter:range" [| id @(𝐼 ℕ) $ range 1 3 |] [| iter [1,2] |]

𝔱 "core:iter:upto" [| id @(𝐼 ℕ) $ upto 0 |] [| iter []    |]
𝔱 "core:iter:upto" [| id @(𝐼 ℕ) $ upto 1 |] [| iter [0]   |]
𝔱 "core:iter:upto" [| id @(𝐼 ℕ) $ upto 2 |] [| iter [0,1] |]

𝔱 "core:iter:bind" [| id @(𝐼 ℕ) $ do m ← iter [1,2] ; n ← iter [10,20] ; return $ m + n |] [| iter [11,21,12,22] |]

𝔱 "core:iter:reverse" [| id @(𝐼 ℕ) $ reverse $ upto 3 |] [| id @(𝐼 ℕ) $ iter [2,1,0] |]

𝔱 "core:iter:dropWhile" [| id @(𝐼 ℕ) $ dropWhile (< 3) $ concat [upto 5,reverse $ upto 5]                  |] [| iter [3,4,4,3,2,1,0]         |]
𝔱 "core:iter:dropWhile" [| id @(𝐼 ℕ) $ dropWhile (< 3) (concat [upto 5,reverse $ upto 5]) ⧺ iter [100,101] |] [| iter [3,4,4,3,2,1,0,100,101] |]

𝔱 "core:iter:takeWhile" [| id @(𝐼 ℕ) $ takeWhile (< 3) $ concat [upto 5,reverse $ upto 5]                  |] [| iter [0,1,2] |]
𝔱 "core:iter:takeWhile" [| id @(𝐼 ℕ) $ takeWhile (< 3) (concat [upto 5,reverse $ upto 5]) ⧺ iter [100,101] |] [| iter [0,1,2,100,101] |]

𝔱 "core:iter:keepN" [| id @(𝐼 ℕ) $ keepN (𝕟 0) [0,1,2] |] [| iter []      |]
𝔱 "core:iter:keepN" [| id @(𝐼 ℕ) $ keepN (𝕟 1) [0,1,2] |] [| iter [0]     |]
𝔱 "core:iter:keepN" [| id @(𝐼 ℕ) $ keepN (𝕟 2) [0,1,2] |] [| iter [0,1]   |]
𝔱 "core:iter:keepN" [| id @(𝐼 ℕ) $ keepN (𝕟 3) [0,1,2] |] [| iter [0,1,2] |]
𝔱 "core:iter:keepN" [| id @(𝐼 ℕ) $ keepN (𝕟 4) [0,1,2] |] [| iter [0,1,2] |]

𝔱 "core:iter:keepN" [| id @(𝐼 ℕ) $ dropN (𝕟 0) [0,1,2] |] [| iter [0,1,2] |]
𝔱 "core:iter:keepN" [| id @(𝐼 ℕ) $ dropN (𝕟 1) [0,1,2] |] [| iter [1,2]   |]
𝔱 "core:iter:keepN" [| id @(𝐼 ℕ) $ dropN (𝕟 2) [0,1,2] |] [| iter [2]     |]
𝔱 "core:iter:keepN" [| id @(𝐼 ℕ) $ dropN (𝕟 3) [0,1,2] |] [| iter []      |]
𝔱 "core:iter:keepN" [| id @(𝐼 ℕ) $ dropN (𝕟 4) [0,1,2] |] [| iter []      |]

𝔱 "core:iter:replicate" [| id @(𝐼 ℕ) $ replicate (𝕟 0) 42 |] [| iter [] |]
𝔱 "core:iter:replicate" [| id @(𝐼 ℕ) $ replicate (𝕟 2) 42 |] [| iter [42,42] |]

𝔱 "core:iter:filterMap" [| id @(𝐼 ℕ) $ filterMap id [None,Some 0,None,Some 1,None] |] [| iter [0,1] |]

𝔱 "core:iter:inbetween" [| id @(𝐼 ℕ) $ inbetween 3 (upto 3) |] [| iter [0,3,1,3,2] |]

𝔱 "core:iter:mapM" [| id @(𝐼 ℕ ∧ 𝐼 ℕ) $ mapM (\ x → do tell (single x) ; return x) $ iter [0,1,2] |] [| iter [0,1,2] :* iter [0,1,2] |]

𝔱 "core:iter:filterM" [| id @(𝐼 ℕ ∧ 𝐼 ℕ) $ filterM (\ x → do tell (single x) ; return $ x < 2) $ iter [0,1,2] |] [| iter [0,1,2] :* iter [0,1] |]

𝔱 "core:iter:uptoStep" [| id @(𝐼 ℕ64) $ uptoStep 10 1  |] [| iter [0,1,2,3,4,5,6,7,8,9] |]
𝔱 "core:iter:uptoStep" [| id @(𝐼 ℕ64) $ uptoStep 10 2  |] [| iter [0,2,4,6,8]           |]
𝔱 "core:iter:uptoStep" [| id @(𝐼 ℕ64) $ uptoStep 10 3  |] [| iter [0,3,6,9]             |]
𝔱 "core:iter:uptoStep" [| id @(𝐼 ℕ64) $ uptoStep 10 4  |] [| iter [0,4,8]               |]
𝔱 "core:iter:uptoStep" [| id @(𝐼 ℕ64) $ uptoStep 10 5  |] [| iter [0,5]                 |]
𝔱 "core:iter:uptoStep" [| id @(𝐼 ℕ64) $ uptoStep 10 6  |] [| iter [0,6]                 |]
𝔱 "core:iter:uptoStep" [| id @(𝐼 ℕ64) $ uptoStep 10 7  |] [| iter [0,7]                 |]
𝔱 "core:iter:uptoStep" [| id @(𝐼 ℕ64) $ uptoStep 10 8  |] [| iter [0,8]                 |]
𝔱 "core:iter:uptoStep" [| id @(𝐼 ℕ64) $ uptoStep 10 9  |] [| iter [0,9]                 |]
𝔱 "core:iter:uptoStep" [| id @(𝐼 ℕ64) $ uptoStep 10 10 |] [| iter [0]                   |]
𝔱 "core:iter:uptoStep" [| id @(𝐼 ℕ64) $ uptoStep 0  1  |] [| iter []                    |]
𝔱 "core:iter:uptoStep" [| id @(𝐼 ℕ64) $ uptoStep 0  2  |] [| iter []                    |]
𝔱 "core:iter:uptoStep" [| id @(𝐼 ℕ64) $ uptoStep 0  3  |] [| iter []                    |]

-----------
-- OTHER --
-----------

𝔱 "core:list:isEmpty" [| isEmpty $ id @(𝐿 ℕ) $ Nil      |] [| True  |]
𝔱 "core:list:isEmpty" [| isEmpty $ id @(𝐿 ℕ) $ 1 :& Nil |] [| False |]

𝔱 "core:dict:dict𝐷" [| id @(ℕ ⇰ ℕ) $ dict𝐷 [1 ↦♭ 2,1 ↦♭ 3] |] [| dict𝐷 [1 ↦♭ 2] |]

𝔱 "core:lens:alter" [| id @(𝕊 ⇰ ℕ) $ alter (keyL "x") (map (+ 1)) ("x" ↦♭ 1)                     |] [| "x" ↦♭ 2                    |]
𝔱 "core:lens:alter" [| id @(𝕊 ⇰ ℕ) $ alter (keyL "x") (map (+ 1)) ("y" ↦♭ 1)                     |] [| "y" ↦♭ 1                    |]
𝔱 "core:lens:alter" [| id @(𝕊 ⇰ ℕ) $ alter (keyL "x") (map (+ 1)) (dict𝐷 ["x" ↦♭ 10,"y" ↦♭ 20])  |] [| dict𝐷 ["x" ↦♭ 11,"y" ↦♭ 20] |]
𝔱 "core:lens:alter" [| id @(𝕊 ⇰ ℕ) $ alter (keyL "x") (const None) ("x" ↦♭ 1)                    |] [| dø𝐷                         |]
𝔱 "core:lens:alter" [| id @(𝕊 ⇰ ℕ) $ alter (keyL "x") (const None) ("y" ↦♭ 1)                    |] [| "y" ↦♭ 1                    |]
𝔱 "core:lens:alter" [| id @(𝕊 ⇰ ℕ) $ alter (keyL "x") (const None) (dict𝐷 ["x" ↦♭ 10,"y" ↦♭ 20]) |] [| dict𝐷 ["y" ↦♭ 20]           |]

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
