module UVMHS.Core.LensDeriving where

import UVMHS.Core.Init
import UVMHS.Core.Classes
import UVMHS.Core.Data

import UVMHS.Core.Effects
import UVMHS.Core.IO
import UVMHS.Core.TH

import qualified Language.Haskell.TH as TH

-- makeLensLogic [C₁,…,Cₙ] ty [a₁,…,aₙ] field fieldty ≔
--   [| fieldL ∷ ∀ a₁ … aₙ. (C₁,…,Cₙ) ⇒ ty a₁ … aₙ ⟢ fieldty
--      fieldL ≔ lens field (\ x s → s { field = x })
--   |]
makeLensLogic ∷ TH.Cxt → TH.Name → 𝐿 (TH.TyVarBndr ()) → TH.Name → TH.Type → TH.Q (𝐿 TH.Dec)
makeLensLogic cx ty tyargs field fieldty = do
  let lensName = TH.mkName $ tohsChars $ string (TH.nameBase field) ⧺ "L"
      tyargVars = map (TH.VarT ∘ thTyVarBndrName) tyargs
      tyargs' = mapOn tyargs $ \case
        TH.PlainTV x () → TH.PlainTV x TH.SpecifiedSpec
        TH.KindedTV x () κ → TH.KindedTV x TH.SpecifiedSpec κ
  tmpˣ ← TH.newName $ tohsChars "x"
  tmpˢ ← TH.newName $ tohsChars "s"
  return $ list
    [ TH.PragmaD $ TH.InlineP lensName TH.Inline TH.FunLike TH.AllPhases
    , TH.SigD lensName $
        TH.ForallT (tohs tyargs') cx $
          TH.ConT ''(⟢) ⊙ (TH.ConT ty ⊙⋆ tyargVars) ⊙ fieldty
    , TH.FunD lensName $ single $ thSingleClause null $
        TH.VarE 'lens ⊙ TH.VarE field ⊙$ TH.LamE [TH.VarP tmpˢ,TH.VarP tmpˣ] $ TH.RecUpdE (TH.VarE tmpˢ) [(field,TH.VarE tmpˣ)]
    ]

makeLenses ∷ TH.Name → TH.Q [TH.Dec]
makeLenses name = do
  (cx :* ty :* tyargs :* _ :* c :* _) ← ifNoneM (io abortIO) ∘ (thViewSingleConADT *∘ view thTyConIL) *$ TH.reify name
  (_ :* fields) ← ifNoneM (io abortIO) $ view thRecCL c
  map (tohs ∘ concat) $ mapMOn fields $ \ (frhs → (field :* _ :* fieldty)) → makeLensLogic cx ty tyargs field fieldty

-- makePrismLogic [C₁,…,Cₙ] ty [a₁,…,aₙ] con (fieldty₁,…,fieldtyₙ) ≔
--   [| fieldL ∷ ∀ a₁ … aₙ. (C₁,…,Cₙ) ⇒ ty a₁ … aₙ ⌲ (fieldty₁,…,fieldtyₙ)
--      fieldL ≔ Prism
--        { inject = con
--        , view = \ v → case v of
--            con x₁ … xₙ → Some (x₁,…,xₙ)
--            _ → None
--        }
--   |]
makePrismLogic ∷ TH.Cxt → TH.Name → 𝐿 (TH.TyVarBndr ()) → TH.Name → 𝐿 TH.Type → ℕ → TH.Q (𝐿 TH.Dec)
makePrismLogic cx ty tyargs con fieldtys numcons = do
  let prismName = TH.mkName $ tohsChars $ (string $ mapFirst toLower $ TH.nameBase con) ⧺ "L"
      tyargVars = map (TH.VarT ∘ thTyVarBndrName) tyargs
      tyargs' = mapOn tyargs $ \case
        TH.PlainTV x () → TH.PlainTV x TH.SpecifiedSpec
        TH.KindedTV x () κ → TH.KindedTV x TH.SpecifiedSpec κ
  tmpˣ ← TH.newName $ tohsChars "x"
  tmpˣˢ ← mapMOn fieldtys $ const $ TH.newName $ tohsChars "x"
  return $
    list
    [ TH.PragmaD $ TH.InlineP prismName TH.Inline TH.FunLike TH.AllPhases
    , TH.SigD prismName $
        TH.ForallT (tohs tyargs') cx $
          TH.ConT ''(⌲) ⊙ (TH.ConT ty ⊙⋆ tyargVars) ⊙ tup fieldtys
    , TH.FunD prismName $ single $ thSingleClause null $
        TH.ConE 'Prism
        ⊙ (TH.LamE [tup $ map TH.VarP tmpˣˢ] $ TH.ConE con ⊙⋆ map TH.VarE tmpˣˢ)
        ⊙ (TH.LamE [TH.VarP tmpˣ] $
            TH.CaseE (TH.VarE tmpˣ) $ concat
              [ single $ thSingleMatch (TH.ConP con [] $ tohs (map TH.VarP tmpˣˢ)) $
                  TH.ConE 'Some ⊙ tup (map TH.VarE tmpˣˢ)
              , case numcons ≤ 1 of
                  -- avoids generating code that has a dead branch
                  True → []
                  False → single $ thSingleMatch TH.WildP $ TH.ConE 'None
              ])
    ]

makePrisms ∷ TH.Name → TH.Q [TH.Dec]
makePrisms name = do
  (cx :* ty :* tyargs :* _ :* cs :* _) ← ifNoneM (io abortIO) ∘ (thViewADT *∘ view thTyConIL) *$ TH.reify name
  scs ← mapM (ifNoneM (io abortIO) ∘ thViewSimpleCon) cs
  let numcons = count scs
  map (tohs ∘ concat) $ mapMOn scs $ \ (con :* fieldtys) → makePrismLogic cx ty tyargs con fieldtys numcons
