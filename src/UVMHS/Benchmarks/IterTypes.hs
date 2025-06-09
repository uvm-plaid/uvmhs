module UVMHS.Benchmarks.IterTypes where

import UVMHS

bakel ∷ 𝐼 a → 𝐼 a
bakel = foldFromWith null $ \ x xs → xs ⧺ single x

baker ∷ 𝐼 a → 𝐼 a
baker = foldrFromWith null $ \ x xs → single x ⧺ xs

bakels ∷ 𝑆 a → 𝑆 a
bakels = foldFromWith null $ \ x xs → xs ⧺ single x

bakers ∷ 𝑆 a → 𝑆 a
bakers = foldrFromWith null $ \ x xs → single x ⧺ xs

benchmarkIterTypes ∷ IO ()
benchmarkIterTypes = do
  out "<UVMHS>"
  args ← iargs
  pprint $ ppHorizontal $ map ppHeader args
  let n ∷ ℕ64
      n = read𝕊 $ args ⋕! 0
      args' = dropN (𝕟64 1) args
  result ← profileIOLog "<>" $ case lazyList args' of
    ["iterl"]         → \ () → return $ foldOnFrom (upto n) 0 (+)
    ["iterr"]         → \ () → return $ foldrOnFrom (upto n) 0 (+)
    ["iterl-bakel"]   → \ () → return $ foldOnFrom (bakel $ upto n) 0 (+)
    ["iterl-baker"]   → \ () → return $ foldOnFrom (baker $ upto n) 0 (+)
    ["iterr-bakel"]   → \ () → return $ foldrOnFrom (bakel $ upto n) 0 (+)
    ["iterr-baker"]   → \ () → return $ foldrOnFrom (baker $ upto n) 0 (+)
    ["streaml"]       → \ () → return $ foldOnFrom (stream $ upto n) 0 (+)
    ["streamr"]       → \ () → return $ foldrOnFrom (stream $ upto n) 0 (+)
    ["streaml-bakel"] → \ () → return $ foldOnFrom (bakels $ stream $ upto n) 0 (+)
    ["streaml-baker"] → \ () → return $ foldOnFrom (bakers $ stream $ upto n) 0 (+)
    ["streamr-bakel"] → \ () → return $ foldrOnFrom (bakels $ stream $ upto n) 0 (+)
    ["streamr-baker"] → \ () → return $ foldrOnFrom (bakers $ stream $ upto n) 0 (+)
    _ → \ () → do
      pprint $ ppHeader "UNKNOWN COMMAND"
      return 0
  pprint result
  out "<DONE>"

