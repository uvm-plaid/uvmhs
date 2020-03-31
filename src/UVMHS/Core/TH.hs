module UVMHS.Core.TH where

import UVMHS.Core.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data

import UVMHS.Core.Lens
import UVMHS.Core.Effects

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import qualified Prelude as HS

class MonadQ (m ∷ ★ → ★) where qio ∷ TH.Q a → m a

instance Functor TH.Q where map = mmap
instance Return TH.Q where return = HS.return
instance Bind TH.Q where (≫=) = (HS.>>=)
instance Monad TH.Q
instance MonadIO TH.Q where io = TH.runIO
instance MonadQ TH.Q where qio = id

instance Apply TH.Exp where (⊙) = TH.AppE
instance Tup TH.Exp where tup = TH.TupE ∘ lazyList

instance Tup TH.Pat where tup = TH.TupP ∘ lazyList

instance Apply TH.Type where (⊙) = TH.AppT
instance Tup TH.Type where tup ts = TH.TupleT (tohs $ intΩ64 $ count ts) ⊙⋆ ts
instance Arrow TH.Type where f ⇨ x = TH.ArrowT ⊙ f ⊙ x

thString ∷ 𝕊 → TH.Exp
thString = TH.LitE ∘ TH.StringL ∘ lazyList
      
thConNames ∷ TH.Con → 𝐿 TH.Name
thConNames (TH.NormalC n _) = single n
thConNames (TH.RecC n _) = single n
thConNames (TH.InfixC _ n _) = single n
thConNames (TH.ForallC _ _ c) = thConNames c
thConNames (TH.GadtC (frhs → ns) _ _) = ns
thConNames (TH.RecGadtC (frhs → ns) _ _) = ns

thTyVarBndrName ∷ TH.TyVarBndr → TH.Name
thTyVarBndrName (TH.PlainTV name) = name
thTyVarBndrName (TH.KindedTV name _) = name

thSingleClause ∷ 𝐿 TH.Pat → TH.Exp → TH.Clause
thSingleClause p b = TH.Clause (tohs p) (TH.NormalB b) []

thSingleMatch ∷ TH.Pat → TH.Exp → TH.Match
thSingleMatch p b = TH.Match p (TH.NormalB b) []

thViewSimpleCon ∷ TH.Con → 𝑂 (TH.Name ∧ 𝐿 TH.Type)
thViewSimpleCon (TH.NormalC name (frhs → strictTypes)) = Some (name :* map snd strictTypes)
thViewSimpleCon (TH.RecC name (frhs → varStrictTypes)) = Some (name :* map snd varStrictTypes)
thViewSimpleCon (TH.InfixC (_,typeL) name (_,typeR)) = Some (name :* list [typeL,typeR])
thViewSimpleCon (TH.ForallC _ _ _) = None
thViewSimpleCon (TH.GadtC _ _ _) = None
thViewSimpleCon (TH.RecGadtC _ _ _) = None

thTyConIL ∷ TH.Info ⌲ TH.Dec
thTyConIL = Prism
  { view = \case
      TH.TyConI d → Some d
      _ → None
  , construct = TH.TyConI
  }

thDataDL ∷ TH.Dec ⌲ TH.Cxt ∧ TH.Name ∧ 𝐿 TH.TyVarBndr ∧ 𝑂 TH.Kind ∧ 𝐿 TH.Con ∧ 𝐿 TH.DerivClause
thDataDL = Prism
  { view = \case
      TH.DataD cx t (frhs → args) (frhs → kM) (frhs → cs) (frhs → ders) → Some (cx :* t :* args :* kM :* cs :* ders)
      _ → None
  , construct = \ (cx :* t :* args :* kM :* cs :* ders) → TH.DataD cx t (tohs args) (tohs kM) (tohs cs) (tohs ders)
  }

thNewtypeDL ∷ TH.Dec ⌲ TH.Cxt ∧ TH.Name ∧ 𝐿 TH.TyVarBndr ∧ 𝑂 TH.Kind ∧ TH.Con ∧ 𝐿 TH.DerivClause
thNewtypeDL = Prism
  { view = \case
      TH.NewtypeD cx t (frhs → args) (frhs → kM) (frhs → c) (frhs → ders) → Some (cx :* t :* args :* kM :* c :* ders)
      _ → None
  , construct = \ (cx :* t :* args :* kM :* c :* ders) → TH.NewtypeD cx t (tohs args) (tohs kM) (tohs c) (tohs ders)
  }

thViewADT ∷ TH.Dec → 𝑂 (TH.Cxt ∧ TH.Name ∧ 𝐿 TH.TyVarBndr ∧ 𝑂 TH.Kind ∧ 𝐿 TH.Con ∧ 𝐿 TH.DerivClause)
thViewADT d =
  view thDataDL d
  ⎅
  (ff ^∘ view thNewtypeDL) d
  where
    ff (cx :* t :* args :* kM :* c :* ders) = (cx :* t :* args :* kM :* single c :* ders)

thViewSingleConADT ∷ TH.Dec → 𝑂 (TH.Cxt ∧ TH.Name ∧ 𝐿 TH.TyVarBndr ∧ 𝑂 TH.Kind ∧ TH.Con ∧ 𝐿 TH.DerivClause)
thViewSingleConADT dec = do
  (cx :* t :* args :* kM :* cs :* ders) ← thViewADT dec
  c ← view singleL cs
  return (cx :* t :* args :* kM :* c :* ders)

thRecCL ∷ TH.Con ⌲ TH.Name ∧ 𝐿 TH.VarStrictType
thRecCL = Prism
  { view = \case
      TH.RecC n (frhs → fs) → Some (n :* fs)
      _ → None
  , construct = \ (n :* fs) → TH.RecC n (tohs fs)
  }

