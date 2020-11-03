{-# OPTIONS_GHC -Wno-unused-imports #-}
module UVMHSMain where

import UVMHS 

import qualified UVMHSContrib.Lang.Arith as Arith
import qualified UVMHSContrib.Lang.ArithBlocks as ArithBlocks
import qualified UVMHSContrib.Lang.SExp as SExp

main ∷ IO ()
main = cleanExit $ do
  -- ArithBlocks.testParserFailure1
  ArithBlocks.testParserFailure1
  -- colorsDemo
  -- out "¯\\_﹙ツ﹚_/¯"
