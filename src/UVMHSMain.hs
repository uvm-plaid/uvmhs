{-# OPTIONS_GHC -Wno-unused-imports #-}
module UVMHSMain where

import UVMHS 

import qualified UVMHSContrib.Lang.Arith as Arith
import qualified UVMHSContrib.Lang.SExp as SExp

main ∷ IO ()
main = cleanExit $ do
  out "¯\\_﹙ツ﹚_/¯"
