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
  pprint $ ppFG teal $ ppString "¯\\_﹙ツ﹚_/¯"

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

