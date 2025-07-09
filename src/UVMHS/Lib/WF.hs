module UVMHS.Lib.WF where

import UVMHS.Core

class WF a where
  wf ∷ a → 𝔹
