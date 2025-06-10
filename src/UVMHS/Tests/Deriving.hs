module UVMHS.Tests.Deriving (g__TESTS__UVMHS__Tests__Deriving) where

import UVMHS.Core
import UVMHS.Lib.Testing
import UVMHS.Lib.Fuzzy
import UVMHS.Lib.Shrinky
import UVMHS.Lib.Pretty
import UVMHS.Future.TH
import UVMHS.Future.TH.Deriving

𝔱 "deriving:fuzzy"
  [| 𝕤 $(thShowDecs $ createFuzzyInstance [] [] ''(∧)) |]
  [| 𝕤 $(thShowDecs
       [d| instance (Fuzzy a,Fuzzy b) => Fuzzy (a ∧ b) where 
             fuzzy = wrchoose 
               [ (:*) one $ \ () → do 
                   x0 ← fuzzy @a
                   x1 ← fuzzy @b
                   return $ (:*) x0 x1
               ]
       |]) 
  |]
𝔱 "deriving:fuzzy"
  [| 𝕤 $(thShowDecs $ createFuzzyInstance [] [] ''(∨)) |]
  [| 𝕤 $(thShowDecs
       [d| instance (Fuzzy a,Fuzzy b) => Fuzzy (a ∨ b) where 
             fuzzy = wrchoose 
               [ (:*) one $ \ () → do 
                   x0 ← fuzzy @a
                   return $ Inl x0
               , (:*) one $ \ () → do 
                   x0 ← fuzzy @b
                   return $ Inr x0
               ]
       |]) 
  |]
𝔱 "deriving:fuzzy" 
  [| 𝕤 $(thShowDecs $ createFuzzyInstance [] [] ''𝐿) |]
  [| 𝕤 $(thShowDecs
       [d| instance (Fuzzy a,Fuzzy (𝐿 a)) => Fuzzy (𝐿 a) where 
             fuzzy = do 
               d ← fuzzyDepth
               wrchoose 
                 [ (:*) one $ \ () → do 
                     return $ Nil
                 , (:*) d $ \ () → do 
                     x0 ← fuzzy @a
                     x1 ← fuzzyRec @(𝐿 a)
                     return $ (:&) x0 x1
                 ]
       |]) 
  |]

𝔱 "deriving:shrinky"
  [| 𝕤 $(thShowDecs $ createShrinkyInstance ''(∧)) |]
  [| 𝕤 $(thShowDecs
       [d| instance (Shrinky a,Shrinky b) ⇒ Shrinky (a ∧ b) where
            shrink = \case
              (:*) x0 x1 → concat
                [ do x' ← shrink x0 ; return $ (:*) x' x1
                , do x' ← shrink x1 ; return $ (:*) x0 x'
                ]
       |])
  |]
𝔱 "deriving:shrinky"
  [| 𝕤 $(thShowDecs $ createShrinkyInstance ''(∨)) |]
  [| 𝕤 $(thShowDecs
       [d| instance (Shrinky a,Shrinky b) ⇒ Shrinky (a ∨ b) where
            shrink = \case
              Inl x0 → concat [ do x' ← shrink x0 ; return $ Inl x' ]
              Inr x0 → concat [ do x' ← shrink x0 ; return $ Inr x' ]
       |])
  |]
𝔱 "deriving:shrinky"
  [| ppString $(thShowDecs $ createShrinkyInstance ''𝐿) |]
  [| ppString $(thShowDecs
       [d| instance (Shrinky a,Shrinky (𝐿 a)) ⇒ Shrinky (𝐿 a) where
            shrink = \case
              Nil → null
              (:&) x0 x1 → concat 
                [ do x' ← shrink x0 ; return $ (:&) x' x1
                , do x' ← shrink x1 ; return $ (:&) x0 x' 
                ]
       |])
  |]

buildTests
