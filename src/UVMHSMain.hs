{-# OPTIONS_GHC -Wno-unused-imports #-}
module UVMHSMain where

import UVMHS 

import qualified Examples.Lang.Arith as Arith
import qualified Examples.Lang.ArithBlocks as ArithBlocks
import qualified Examples.Lang.SExp as SExp

import qualified Prelude as HS

import qualified UVMHS.Lib.NewIter as NI

main ∷ IO ()
main = cleanExit $ do
  pprint $ ppHeader "COLOR TEST"
  pprint colorsDemo
  pprint $ ppFG teal $ ppString "¯\\_﹙ツ﹚_/¯"

  -- let lotsI = NI.upTo' 10000000
  --     -- lotsL = NI.listIter lotsI

  -- -- pprint lotsL

  -- return ()
  -- void $ unFailT $ evalStateT 0 $ NI.mfold𝐼' 
  --   () 
  --   (\ n () → do s ← next
  --                if s ≥ 10
  --                then abort
  --                else io $ pprint n)
  --   lotsI

  -- let someI = NI.upTo' 5
  --     someI' = NI.reverse𝐼' $ NI.upTo' 5
  --     someL₁₁ = NI.listIter someI
  --     someL₁₂ = NI.listIter someI'
  --     someL₂₁ = NI.reverseListIter someI
  --     someL₂₂ = NI.reverseListIter someI'

  -- pprint someL₁₁
  -- pprint someL₁₂
  -- pprint someL₂₁
  -- pprint someL₂₂

  -- pprint $ NI.listIter $ NI.upTo' 5 `NI.append𝐼'` NI.upTo' 7
                 

  -- let lots = list $ upTo 2000000
  -- pprint "A"
  -- pprint $ count @ℕ lots
  -- pprint "B"
  -- let streamed = NI.delayList𝐼' $ NI.iterList lots
  -- pprint $ NI.firstDL streamed
  -- let loop xs = do
  --       case NI.unDelayList xs () of
  --         None → skip
  --         Some (x :* xs') → do
  --           when (x ÷ 10000 ≡ 0) $ pprint x
  --           loop xs'
  -- loop streamed

  -- pprint $ unID $ runReaderT 1 $ evalUContT $ do
  --   r₁ ← ask
  --   r₂ ← local 10 $ do
  --     ask
  --   r₃ ← uwithC return $ do
  --     ucallCC $ \ (𝓀 ∷ () → u) → local 100 $ 𝓀 ()
  --     ask
  --   r₄ ← ask
  --   return $ r₁ + r₂ + r₃ + r₄

  -- pprint $ unID $ unWriterT $ evalUContT $ do
  --   tell 1
  --   o :* () ← hijack $ do
  --     tell 10
  --   return o

