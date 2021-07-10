module UVMHS.Lib.VariablesNew where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Parser

data 𝕏 = 𝕏
  { 𝕩name ∷ 𝕊
  , 𝕩mark ∷ 𝑂 ℕ64
  } deriving (Eq,Ord,Show)
makeLenses ''𝕏

var ∷ 𝕊 → 𝕏
var x = 𝕏 x None

instance Pretty 𝕏 where
  pretty (𝕏 x nO) = concat
    [ ppString x
    , elim𝑂 null (\ n → concat [ppPun "#",ppPun $ show𝕊 n]) nO
    ]

cpName ∷ CParser TokenBasic 𝕏
cpName = var ^$ cpShaped $ view nameTBasicL

cpNameWS ∷ CParser TokenWSBasic 𝕏
cpNameWS = var ^$ cpShaped $ view nameTWSBasicL

-----------------------------------------
-- LOCALLY NAMELESS WITH SHIFTED NAMES --
-----------------------------------------

data 𝕐 =
    GlobalVar 𝕏
  | NamedVar 𝕏 ℕ64
  | NamelessVar ℕ64
  deriving (Eq,Ord,Show)
makePrisms ''𝕐

gvar ∷ 𝕏 → 𝕐
gvar = GlobalVar

nvar ∷ 𝕏 → 𝕐
nvar x = NamedVar x zero

gvarL ∷ 𝕐 ⌲ 𝕏
gvarL = prism gvar $ \case
  GlobalVar x → return x
  _ → abort

nvarL ∷ 𝕐 ⌲ 𝕏
nvarL = prism nvar $ \case
  NamedVar x n | n ≡ zero → return x
  _ → abort

instance Pretty 𝕐 where
  pretty = \case
    GlobalVar x → pretty x
    NamedVar x n → concat
      [ pretty x
      , if n ≡ zero then null else concat [ppPun "@",ppPun $ show𝕊 n]
      ]
    NamelessVar n → concat [ppPun "!",ppString $ show𝕊 n]

data Subst a = Subst
  { globalSubs ∷ 𝕏 ⇰ a
  , namedSubs ∷ 𝕏 ⇰ ℕ64 ∧ (ℕ64 ⇰ a)
  , namelessSubs ∷ ℕ64 ⇰ a
  , namelessShift ∷ 𝔹 ∧ ℕ64
  }

class FromVar s a | a → s where
  frvar ∷ 𝑃 SrcCxt → s → 𝕐 → 𝑂 a

nullSubst ∷ Subst a
nullSubst = Subst null null null null

applySubstVar ∷ (𝕐 → 𝑂 a) → Subst a → 𝕐 → 𝑂 a
applySubstVar mkvar (Subst g𝓈 n𝓈 i𝓈 (sd :* sn)) y =
  let shft = 
        if sd
        then (+) sn
        else (-) sn
  in tries
    [ do x ← view globalVarL y
         g𝓈 ⋕? x
    , do x :* n ← view namedVarL y
         mn :* nes ← n𝓈 ⋕? x
         if n ≤ mn
         then return $ nes ⋕! n
         else mkvar $ NamedVar x $ n - mn
    , do n ← view namelessVarL y
         tries
           [ i𝓈 ⋕? n
           , mkvar $ NamelessVar $ shft n
           ]
    , mkvar y
    ]
