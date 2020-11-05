{-# OPTIONS_GHC -Wno-unused-imports #-}
module UVMHSMain where

import UVMHS 

import qualified Examples.Lang.Arith as Arith
import qualified Examples.Lang.ArithBlocks as ArithBlocks
import qualified Examples.Lang.SExp as SExp

main ∷ IO ()
main = cleanExit $ do
  pprint $ ppHeader "COLOR TEST"
  colorsDemo
  pprint $ ppFG green $ ppString "¯\\_﹙ツ﹚_/¯"
