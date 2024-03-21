module UVMHS.Lib.Options
  ( module UVMHS.Lib.Options
  , module System.Console.GetOpt
  ) where

import UVMHS.Core

import System.Console.GetOpt (OptDescr,ArgDescr)

import System.Console.GetOpt as Opt

option ∷ 𝐿 ℂ → 𝐿 𝕊 → ArgDescr a → 𝕊 → OptDescr a
option cs ss ad s = Opt.Option (tohs cs) (tohs $ map tohsChars ss) ad $ tohsChars s

noArg ∷ a → ArgDescr a
noArg = Opt.NoArg

reqArg ∷ (𝕊 → a) → 𝕊 → ArgDescr a
reqArg f s = Opt.ReqArg (f ∘ string) $ tohsChars s

optArg ∷ (𝑂 𝕊 → a) → 𝕊 → ArgDescr a
optArg f s = Opt.OptArg (f ∘ map string ∘ frhs) $ tohsChars s

optUsageInfo ∷ 𝕊 → 𝐿 (OptDescr a) → 𝕊
optUsageInfo s ds = string $ Opt.usageInfo (tohsChars s) $ tohs ds

parseOptions ∷ 𝐿 (OptDescr a) → 𝐿 𝕊 → (𝐿 a ∧ 𝐿 𝕊 ∧ 𝐿 𝕊)
parseOptions opts args =
  mapPair (mapSnd $ map string) (map string) $
    frhs $ Opt.getOpt Opt.RequireOrder (tohs opts) $ tohs $ map tohsChars args
