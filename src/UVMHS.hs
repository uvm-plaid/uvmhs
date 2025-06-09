module UVMHS
  ( module UVMHS
  , module UVMHS.Core
  , module UVMHS.Lang.ULC
  , module UVMHS.Lib.Annotated
  , module UVMHS.Lib.Dataframe
  , module UVMHS.Lib.Errors
  , module UVMHS.Lib.Fuzzy
  , module UVMHS.Lib.Graph
  , module UVMHS.Lib.Logging
  , module UVMHS.Lib.MMSP
  , module UVMHS.Lib.Options
  , module UVMHS.Lib.Parser
  , module UVMHS.Lib.Pipeline
  , module UVMHS.Lib.Pretty
  , module UVMHS.Lib.Rand
  , module UVMHS.Lib.Shrinky
  , module UVMHS.Lib.StreamM
  , module UVMHS.Lib.Substitution
  , module UVMHS.Lib.Testing
  , module UVMHS.Lib.TreeAnnote
  , module UVMHS.Lib.TreeNested
  , module UVMHS.Lib.Window
  ) where

import UVMHS.Core hiding (thTyVarBndrName)
import UVMHS.Lang.ULC
import UVMHS.Lib.Annotated
import UVMHS.Lib.Dataframe
import UVMHS.Lib.Errors
import UVMHS.Lib.Fuzzy
import UVMHS.Lib.Graph
import UVMHS.Lib.Logging
import UVMHS.Lib.MMSP
import UVMHS.Lib.Options
import UVMHS.Lib.Parser
import UVMHS.Lib.Pipeline
import UVMHS.Lib.Pretty
import UVMHS.Lib.Rand
import UVMHS.Lib.Shrinky
import UVMHS.Lib.StreamM
import UVMHS.Lib.Substitution
import UVMHS.Lib.THLiftInstances ()
import UVMHS.Lib.Testing
import UVMHS.Lib.TreeAnnote
import UVMHS.Lib.TreeNested
import UVMHS.Lib.Window


import qualified Language.Haskell.TH as TH

import UVMHS.Future.TH
import UVMHS.Future.TH.Deriving

ds₁ ∷ TH.DecsQ
ds₁ = map thStripModuleNamesDec ^$
  [d| instance (Fuzzy a, Fuzzy (𝐿 a)) => Fuzzy (𝐿 a) where 
        fuzzy = do 
          d <- fuzzyDepth
          wrchoose 
            [ \(()) -> one :* do return Nil
            , \(()) -> d :* do x0 <- fuzzy @a
                               x1 <- fuzzyRec @(𝐿 a)
                               return ((:&) x0 x1)
            ]
  |]

ds₂ ∷ TH.DecsQ
ds₂ = map thStripModuleNamesDec ^$ createFuzzyInstance [] ''𝐿
