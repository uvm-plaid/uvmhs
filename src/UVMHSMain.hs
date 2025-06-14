{-# OPTIONS_GHC -Wno-unused-imports #-}
module UVMHSMain where

import UVMHS

import qualified Examples.Lang.Arith as Arith
import qualified Examples.Lang.ArithBlocks as ArithBlocks
import qualified Examples.Lang.SExp as SExp

import qualified Prelude as HS

import UVMHS.Tests.Core
import UVMHS.Tests.Substitution
import UVMHS.Tests.Deriving

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Test.QuickCheck (Arbitrary, arbitrary)
import qualified Test.QuickCheck as QC
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Vector as V

import UVMHS.Lang.ULC

import qualified Control.Monad.Logic as L

import qualified GHC.IsList as IsList

import UVMHS.Future.TH
import UVMHS.Future.TH.Deriving

import qualified UVMHS.Tests.Lexer as TestLexer

import UVMHS.Tests.Lexer hiding (lexer)

import UVMHS.Lib.Parser.Blockify
import UVMHS.Lib.Parser.Regex (mkIndentTokenWSBasic,blockTWSBasicL)

main ∷ IO ()
main = out "<UVMHS>"

test ∷ IO ()
test = do
  pprint $ ppHeader "COLOR TEST"
  pprint colorsDemo
  rngSeed 0
  $$(testModules True (fuzzParamsSml 10)
    [ "UVMHS.Tests.Core"
    , "UVMHS.Tests.Substitution"
    , "UVMHS.Tests.Deriving"
    , "UVMHS.Tests.Lexer"
    ])
  -- eachOn (upto 100) $ \ s → do
  --   rngSeed s
  --   pprint $ 𝐤 "SEED" $ 𝐯 $ pretty s
  --   $$(testModules False fuzzParamsTny
  --     [ "UVMHS.Tests.Core"
  --     , "UVMHS.Tests.Substitution"
  --     ])

dev ∷ IO ()
dev = cleanExit $ do
  test
  let s = concat $ inbetween "\n"
       [ "a b"
       , "c d"
       , "e f" 
       , "g h"
       ] 
  let r₁ = map renderParserTokens $ 
        tokenizeWSUnanchored TestLexer.lexer "<>" $ tokens s
      r₂ = do
        ts ← tokenize TestLexer.lexer "<>" $ tokens s
        ts' ← blockify $ TestLexer.blockifyArgs "<>" False $ stream ts
        return $ renderParserTokens $ finalizeTokens $ vec ts'
  let debugThing s' = do
        ts ← tokenize TestLexer.lexer "<>" $ tokens s'
        ts' ← blockify $ blockifyArgs "<>" True $ stream ts
        return $ renderParserTokens $ finalizeTokens $ vec ts'
  pprint r₁
  pprint r₂
  pprint $ r₁ ≡ r₂
  pprint $ debugThing $ concat $ inbetween "\n"
    [ "a b"
    , "+ c d"
    ]
  pprint $ debugThing $ concat $ inbetween "\n"
    [ "a b"
    , ") c d"
    ]

  -- FUTURE
  -- out $(thShowDecs ds₁)
  -- out $(thShowDecs ds₂)
  -- shout $ $(thShowDecs ds₁) ≡ $(thShowDecs ds₂)
  -- out $(thShowDecs $ map thStripModuleNamesDec ^$ createFuzzyInstance ["a"] [] ''(∨))
  -- out $(thShowDecs $ map thStripModuleNamesDec ^$ createFuzzyInstance [] ''OtherList)
  -- out $(thShowDecs $ map thStripModuleNamesDec ^$ createFuzzyInstance ["a"] ''OtherList)
  -- out $(thShowDecs $ map thStripModuleNamesDec ^$ createShrinkyInstance ''(∧))
  -- out $(thShowDecs $ map thStripModuleNamesDec ^$ createShrinkyInstance ''(∨))
  -- out $(thShowDecs $ map thStripModuleNamesDec ^$ createShrinkyInstance ''𝑆)
