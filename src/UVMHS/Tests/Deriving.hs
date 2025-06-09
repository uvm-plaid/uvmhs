module UVMHS.Tests.Deriving (g__TESTS__UVMHS__Tests__Deriving) where

import UVMHS.Core
import UVMHS.Lib.Testing
import UVMHS.Lib.Fuzzy
import UVMHS.Future.TH
import UVMHS.Future.TH.Deriving

𝔱 "deriving:fuzzy" 
  [| id @𝕊 $(thShowDecs $ createFuzzyInstance [] ''𝐿) |]
  [| id @𝕊 $(thShowDecs $ 
       [d| instance (Fuzzy a, Fuzzy (𝐿 a)) => Fuzzy (𝐿 a) where 
             fuzzy = do 
               d ← fuzzyDepth
               wrchoose 
                 [ \ () → one :* do return Nil
                 , \ () → d :* do x0 ← fuzzy @a
                                  x1 ← fuzzyRec @(𝐿 a)
                                  return ((:&) x0 x1)
                 ]
       |]) 
  |]
𝔱 "deriving:fuzzy"
  [| id @𝕊 $(thShowDecs $ createFuzzyInstance [] ''(∧)) |]
  [| id @𝕊 $(thShowDecs $ 
       [d| instance (Fuzzy a, Fuzzy b) => Fuzzy (a ∧ b) where 
             fuzzy = wrchoose 
               [ \ () → one :* do x0 ← fuzzy @a
                                  x1 ← fuzzy @b
                                  return ((:*) x0 x1)
               ]
       |]) 
  |]

buildTests

