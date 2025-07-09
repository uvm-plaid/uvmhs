module UVMHS.Tests.Core (g__TESTS__UVMHS__Tests__Core) where

import UVMHS.Core
import UVMHS.Lib.Testing

----------
-- ITER --
----------

testSection "core:iter:isEmpty"

test [| isEmpty $ id @(𝐼 ℕ) $ iter []  |] [| True  |]
test [| isEmpty $ id @(𝐼 ℕ) $ iter [1] |] [| False |]

testSection "core:iter:range"

test [| id @(𝐼 ℕ) $ range 0 0 |] [| iter []    |]
test [| id @(𝐼 ℕ) $ range 1 1 |] [| iter []    |]
test [| id @(𝐼 ℕ) $ range 0 1 |] [| iter [0]   |]
test [| id @(𝐼 ℕ) $ range 0 2 |] [| iter [0,1] |]
test [| id @(𝐼 ℕ) $ range 1 3 |] [| iter [1,2] |]

testSection "core:iter:upto"

test [| id @(𝐼 ℕ) $ upto 0 |] [| iter []    |]
test [| id @(𝐼 ℕ) $ upto 1 |] [| iter [0]   |]
test [| id @(𝐼 ℕ) $ upto 2 |] [| iter [0,1] |]

testSection "core:iter:bind"

test [| id @(𝐼 ℕ) $ do m ← iter [1,2] ; n ← iter [10,20] ; return $ m + n |] [| iter [11,21,12,22] |]

testSection "core:iter:reverse"

test [| id @(𝐼 ℕ) $ reverse $ upto 3 |] [| id @(𝐼 ℕ) $ iter [2,1,0] |]

testSection "core:iter:dropWhile"

test [| id @(𝐼 ℕ) $ dropWhile (< 3) $ concat [upto 5,reverse $ upto 5]                  |] [| iter [3,4,4,3,2,1,0]         |]
test [| id @(𝐼 ℕ) $ dropWhile (< 3) (concat [upto 5,reverse $ upto 5]) ⧺ iter [100,101] |] [| iter [3,4,4,3,2,1,0,100,101] |]

testSection "core:iter:takeWhile"

test [| id @(𝐼 ℕ) $ takeWhile (< 3) $ concat [upto 5,reverse $ upto 5]                  |] [| iter [0,1,2] |]
test [| id @(𝐼 ℕ) $ takeWhile (< 3) (concat [upto 5,reverse $ upto 5]) ⧺ iter [100,101] |] [| iter [0,1,2,100,101] |]

testSection "core:iter:keepN"

test [| id @(𝐼 ℕ) $ keepN (𝕟 0) [0,1,2] |] [| iter []      |]
test [| id @(𝐼 ℕ) $ keepN (𝕟 1) [0,1,2] |] [| iter [0]     |]
test [| id @(𝐼 ℕ) $ keepN (𝕟 2) [0,1,2] |] [| iter [0,1]   |]
test [| id @(𝐼 ℕ) $ keepN (𝕟 3) [0,1,2] |] [| iter [0,1,2] |]
test [| id @(𝐼 ℕ) $ keepN (𝕟 4) [0,1,2] |] [| iter [0,1,2] |]

testSection "core:iter:dropN"

test [| id @(𝐼 ℕ) $ dropN (𝕟 0) [0,1,2] |] [| iter [0,1,2] |]
test [| id @(𝐼 ℕ) $ dropN (𝕟 1) [0,1,2] |] [| iter [1,2]   |]
test [| id @(𝐼 ℕ) $ dropN (𝕟 2) [0,1,2] |] [| iter [2]     |]
test [| id @(𝐼 ℕ) $ dropN (𝕟 3) [0,1,2] |] [| iter []      |]
test [| id @(𝐼 ℕ) $ dropN (𝕟 4) [0,1,2] |] [| iter []      |]

testSection "core:iter:replicate"

test [| id @(𝐼 ℕ) $ replicate (𝕟 0) 42 |] [| iter [] |]
test [| id @(𝐼 ℕ) $ replicate (𝕟 2) 42 |] [| iter [42,42] |]

testSection "core:iter:filterMap"

test [| id @(𝐼 ℕ) $ filterMap id [None,Some 0,None,Some 1,None] |] [| iter [0,1] |]

testSection "core:iter:inbetween"

test [| id @(𝐼 ℕ) $ inbetween 3 (upto 3) |] [| iter [0,3,1,3,2] |]

testSection "core:iter:mapM"

test [| id @(𝐼 ℕ ∧ 𝐼 ℕ) $ mapM (\ x → do tell (single x) ; return x) $ iter [0,1,2] |] [| iter [0,1,2] :* iter [0,1,2] |]

testSection "core:iter:filterM"

test [| id @(𝐼 ℕ ∧ 𝐼 ℕ) $ filterM (\ x → do tell (single x) ; return $ x < 2) $ iter [0,1,2] |] [| iter [0,1,2] :* iter [0,1] |]

testSection "core:iter:uptoStep"

test [| id @(𝐼 ℕ64) $ uptoStep 10 1  |] [| iter [0,1,2,3,4,5,6,7,8,9] |]
test [| id @(𝐼 ℕ64) $ uptoStep 10 2  |] [| iter [0,2,4,6,8]           |]
test [| id @(𝐼 ℕ64) $ uptoStep 10 3  |] [| iter [0,3,6,9]             |]
test [| id @(𝐼 ℕ64) $ uptoStep 10 4  |] [| iter [0,4,8]               |]
test [| id @(𝐼 ℕ64) $ uptoStep 10 5  |] [| iter [0,5]                 |]
test [| id @(𝐼 ℕ64) $ uptoStep 10 6  |] [| iter [0,6]                 |]
test [| id @(𝐼 ℕ64) $ uptoStep 10 7  |] [| iter [0,7]                 |]
test [| id @(𝐼 ℕ64) $ uptoStep 10 8  |] [| iter [0,8]                 |]
test [| id @(𝐼 ℕ64) $ uptoStep 10 9  |] [| iter [0,9]                 |]
test [| id @(𝐼 ℕ64) $ uptoStep 10 10 |] [| iter [0]                   |]
test [| id @(𝐼 ℕ64) $ uptoStep 0  1  |] [| iter []                    |]
test [| id @(𝐼 ℕ64) $ uptoStep 0  2  |] [| iter []                    |]
test [| id @(𝐼 ℕ64) $ uptoStep 0  3  |] [| iter []                    |]

testSection "core:iter:mapFirst"

test [| id @(𝐼 ℕ64) $ mapFirst ((+) 100) $ upto 5 |] [| iter [100,1,2,3,4] |]

testSection "core:iter:mapAfterFirst"

test [| id @(𝐼 ℕ64) $ mapAfterFirst ((+) 100) $ upto 5 |] [| iter [0,101,102,103,104] |]

testSection "core:iter:mapLast"

test [| id @(𝐼 ℕ64) $ mapLast ((+) 100) $ upto 5 |] [| iter [0,1,2,3,104] |]

testSection "core:iter:mapBeforeLast"

test [| id @(𝐼 ℕ64) $ mapBeforeLast ((+) 100) $ upto 5 |] [| iter [100,101,102,103,4] |]

-----------
-- OTHER --
-----------

testSection "core:list:isEmpty"

test [| isEmpty $ id @(𝐿 ℕ) $ Nil      |] [| True  |]
test [| isEmpty $ id @(𝐿 ℕ) $ 1 :& Nil |] [| False |]

testSection "core:list:dict𝐷"

test [| id @(ℕ ⇰ ℕ) $ dict𝐷 [1 ↦♭ 2,1 ↦♭ 3] |] [| dict𝐷 [1 ↦♭ 2] |]

testSection "core:list:alter𝐷"

test [| id @(𝕊 ⇰ ℕ) $ alter (keyL "x") (map (+ 1)) ("x" ↦♭ 1)                     |] [| "x" ↦♭ 2                    |]
test [| id @(𝕊 ⇰ ℕ) $ alter (keyL "x") (map (+ 1)) ("y" ↦♭ 1)                     |] [| "y" ↦♭ 1                    |]
test [| id @(𝕊 ⇰ ℕ) $ alter (keyL "x") (map (+ 1)) (dict𝐷 ["x" ↦♭ 10,"y" ↦♭ 20])  |] [| dict𝐷 ["x" ↦♭ 11,"y" ↦♭ 20] |]
test [| id @(𝕊 ⇰ ℕ) $ alter (keyL "x") (const None) ("x" ↦♭ 1)                    |] [| dø𝐷                         |]
test [| id @(𝕊 ⇰ ℕ) $ alter (keyL "x") (const None) ("y" ↦♭ 1)                    |] [| "y" ↦♭ 1                    |]
test [| id @(𝕊 ⇰ ℕ) $ alter (keyL "x") (const None) (dict𝐷 ["x" ↦♭ 10,"y" ↦♭ 20]) |] [| dict𝐷 ["y" ↦♭ 20]           |]

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

testSection "core:monads:cr"

test [| 0   |] [| execCR $ do fst ^$ ask |]
test [| 0   |] [| execCR $ do snd ^$ ask |]
test [| 30  |] [| execCR $ do putEnv $ 10 :* 20 ; x :* y ← ask ; return $ x + y |]
test [| 10  |] [| execCR $ do putEnvL fstL 10 ; x :* y ← ask ; return $ x + y |]
test [| 10  |] [| execCR $ do putEnvL fstL 10 ; reset (do x :* y ← ask ; return $ x + y) |]
test [| 0   |] [| execCR $ do _←reset $ (do putEnvL fstL 10;return $ 𝕟64 0);x:*y←ask;return $ x + y |]
test [| 110 |]
  [| execCR $ do putEnvL fstL 10;x ← reset $ (do putEnvL fstL 100;askL fstL);y←askL fstL;return $ x + y |]
-- Note: this is why MonadReader has askL/localL as primitives, and not ask/local
test [| 2 |] [| execCR $ do localL fstL 1 $ putEnvL sndL 2 ; askL sndL |]

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

testSection "core:monads:ur"

test [| 0   |] [| execUR $ do fst ^$ ask |]
test [| 0   |] [| execUR $ do snd ^$ ask |]
test [| 30  |] [| execUR $ do uputEnv $ 10 :* 20 ; x :* y ← ask ; return $ x + y |]
test [| 10  |] [| execUR $ do uputEnvL fstL 10 ; x :* y ← ask ; return $ x + y |]
test [| 10  |] [| execUR $ do uputEnvL fstL 10 ; ureset (do x :* y ← ask ; return $ x + y) |]
test [| 0   |] [| execUR $ do _←ureset $ (do uputEnvL fstL 10;return $ 𝕟64 0);x:*y←ask;return $ x + y |]
test [| 110 |]
  [| execUR $ do uputEnvL fstL 10;x ← ureset $ (do uputEnvL fstL 100;askL fstL);y←askL fstL;return $ x + y |]
-- Note: this is why MonadReader has askL/localL as primitives, and not ask/local
test [| 2 |] [| execUR $ do localL fstL 1 $ uputEnvL sndL 2 ; askL sndL |]

buildTests
