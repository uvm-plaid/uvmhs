module UVMHS.Lib.Pretty.Deriving where

import UVMHS.Core

import UVMHS.Lib.Pretty.Core

import qualified Language.Haskell.TH as TH

import qualified Data.Text as Text

-- makePrettySumLogic [C₁,…,Cₙ] ty [a₁,…,aₙ] [(con₁,[conty₁₁,…,conty₁⸤n₁⸥]),…,(conₘ,[contyₘ₁,…,contyₘ⸤nₘ⸥])] ≔ 
--   [| instance 
--        (C₁,…,Cₙ
--        ,Pretty conty₁₁,…,Pretty conty₁⸤n₁⸥,…,Pretty contyₘ₁,…,Pretty contyₘ⸤nₘ⸥
--        ) ⇒ Pretty (ty a₁ … aₙ) where
--          pretty (con₁ (x₁₁ ∷ conty₁₁) … x₁⸤n₁⸥) = app [con "con₁",pretty x₁₁,…,pretty x₁⸤n₁⸥]
--          …
--          pretty (conₘ (xₘ₁ ∷ contyₘ₁) … xₘ⸤nₘ⸥) = app [con "conₘ",pretty xₘ₁,…,pretty xₘ⸤nₘ⸥]
--   |]
makePrettySumLogic ∷ TH.Cxt → TH.Name → 𝐿 TH.TyVarBndr → 𝐿 (TH.Name ∧ 𝐿 TH.Type) → TH.Q (𝐿 TH.Dec)
makePrettySumLogic cx ty tyargs concontys = do
  conxs ∷ 𝐿 (TH.Name ∧ 𝐿 TH.Name) ← mapMOn concontys $ \ (con :* contys) → do
    tmpˣˢ ← mapMOn contys $ const $ TH.newName $ chars "x"
    return (con :* tmpˣˢ)
  let tyargVars ∷ 𝐿 TH.Type
      tyargVars = map (TH.VarT ∘ thTyVarBndrName) tyargs
      instanceCx ∷ 𝐿 TH.Pred
      instanceCx = list $ uniques $ concat 
        [ frhs cx
        , map (\ x → TH.ConT ''Pretty ⊙ x) $ concat $ map snd $ concontys
        ]
      instanceTy ∷ TH.Type
      instanceTy = TH.ConT ''Pretty ⊙ (TH.ConT ty ⊙⋆ tyargVars)
      instanceDec ∷ TH.Dec
      instanceDec = TH.FunD 'pretty $ tohs $ mapOn conxs $ \ (con :* tmpˣˢ) →
        let conString = thString $ string $ TH.nameBase con
            prettyCon = TH.VarE 'ppCon ⊙ conString
            prettyXs = mapOn tmpˣˢ $ \ x → TH.VarE 'pretty ⊙ TH.VarE x
        in thSingleClause (single $ TH.ConP con $ tohs $ map TH.VarP tmpˣˢ) $ TH.VarE 'ppApp ⊙ prettyCon ⊙$ TH.VarE 'list ⊙$ TH.ListE (tohs prettyXs)
  return $ single $ TH.InstanceD (tohs None) (tohs instanceCx) instanceTy $ single instanceDec

makePrettySum ∷ TH.Name → TH.Q [TH.Dec]
makePrettySum name = do
  (cx :* ty :* tyargs :* _ :* cs :* _) ← return𝑂 (io abortIO) ∘ (thViewADT *∘ view thTyConIL) *$ TH.reify name
  scs ← mapM (return𝑂 (io abortIO) ∘ thViewSimpleCon) cs
  map tohs $ makePrettySumLogic cx ty tyargs scs

-- makePrettyUnionLogic [C₁,…,Cₙ] ty [a₁,…,aₙ] [(con₁,[conty₁₁,…,conty₁⸤n₁⸥]),…,(conₘ,[contyₘ₁,…,contyₘ⸤nₘ⸥])] ≔ 
--   [| instance 
--        (C₁,…,Cₙ
--        ,Pretty conty₁₁,…,Pretty conty₁⸤n₁⸥,…,Pretty contyₘ₁,…,Pretty contyₘ⸤nₘ⸥
--        ) ⇒ Pretty (ty a₁ … aₙ) where
--          pretty (con₁ (x₁₁ ∷ conty₁₁) … x₁⸤n₁⸥) = tup [pretty x₁₁,…,pretty x₁⸤n₁⸥]
--          …
--          pretty (conₘ (xₘ₁ ∷ contyₘ₁) … xₘ⸤nₘ⸥) = tup [pretty xₘ₁,…,pretty xₘ⸤nₘ⸥]
--   |]
makePrettyUnionLogic ∷ TH.Cxt → TH.Name → 𝐿 TH.TyVarBndr → 𝐿 (TH.Name ∧ 𝐿 TH.Type) → TH.Q (𝐿 TH.Dec)
makePrettyUnionLogic cx ty tyargs concontys = do
  conxs ∷ 𝐿 (TH.Name ∧ 𝐿 TH.Name) ← mapMOn concontys $ \ (con :* fieldtys) → do
    tmpˣˢ ← mapMOn fieldtys $ const $ TH.newName $ chars "x"
    return (con :* tmpˣˢ)
  let tyargVars = map (TH.VarT ∘ thTyVarBndrName) tyargs
      instanceCx ∷ 𝐿 TH.Pred
      instanceCx = list $ uniques $ concat [frhs cx,map (\ x → TH.ConT ''Pretty ⊙ x) $ concat $ map snd concontys]
      instanceTy ∷ TH.Type
      instanceTy = TH.ConT ''Pretty ⊙ (TH.ConT ty ⊙⋆ tyargVars)
      instanceDec ∷ TH.Dec
      instanceDec = TH.FunD 'pretty $ tohs $ mapOn conxs $ \ (con :* tmpˣˢ) → 
        thSingleClause (single $ TH.ConP con $ tohs $ map TH.VarP tmpˣˢ) $  case tmpˣˢ of
          Nil → TH.VarE 'pretty ⊙ TH.ConE '()
          x :& Nil → TH.VarE 'pretty ⊙ TH.VarE x
          _ → 
            let prettyXs = mapOn tmpˣˢ $ \ x → TH.VarE 'pretty ⊙ TH.VarE x
            in 
            TH.VarE 'ppCollection 
            ⊙ (TH.VarE 'ppPun ⊙ thString "⟨") 
            ⊙ (TH.VarE 'ppPun ⊙ thString "⟩") 
            ⊙ (TH.VarE 'ppPun ⊙ thString ",") 
            ⊙$ TH.VarE 'list 
            ⊙$ TH.ListE (tohs prettyXs)
  return $ single $ TH.InstanceD (tohs None) (tohs instanceCx) instanceTy $ single $ instanceDec

makePrettyUnion ∷ TH.Name → TH.Q [TH.Dec]
makePrettyUnion name = do
  (cx :* ty :* tyargs :* _ :* cs :* _) ← return𝑂 (io abortIO) ∘ (thViewADT *∘ view thTyConIL) *$ TH.reify name
  scs ← mapM (return𝑂 (io abortIO) ∘ thViewSimpleCon) cs
  map tohs $ makePrettyUnionLogic cx ty tyargs scs

-- makePrettyRecordLogic [C₁,…,Cₙ] ty [a₁,…,aₙ] con [(field₁,fieldty₁),…,(fieldₙ,fieldtyₙ)] ≔
--   [| instance 
--        (C₁,…,Cₙ
--        ,Pretty fieldty₁,…,Pretty fieldtyₙ
--        ) ⇒ Pretty (ty a₁ … aₙ) where
--          pretty (con {field₁ = tmp₁;fieldₙ = tmpₙ}) = app [con "con",record [("field₁",tmp₁),…,("fieldₙ",tmpₙ)
--   |]
makePrettyRecordLogic ∷ TH.Cxt → TH.Name → 𝐿 TH.TyVarBndr → TH.Name → 𝐿 (TH.Name ∧ TH.Type) → TH.Q (𝐿 TH.Dec)
makePrettyRecordLogic cx ty tyargs con fieldfieldtys = do
  let conPrefix = string $ mapFirst toLower $ TH.nameBase con
  fieldNameTmps ← mapMOn fieldfieldtys $ \ (field :* _) → do
    let (_prefix :* afterPrefix) = frhs $ Text.breakOnEnd conPrefix $ string $ TH.nameBase field
        loweredAfterPrefix = string $ mapFirst toLower afterPrefix
    tmpˣ ← TH.newName $ chars "x"
    return (field :* loweredAfterPrefix :* tmpˣ)
  let tyargVars = map (TH.VarT ∘ thTyVarBndrName) tyargs
      instanceCx ∷ 𝐿 TH.Pred
      instanceCx = list $ uniques $ concat 
        [ frhs cx
        , map (\ x → TH.ConT ''Pretty ⊙ x) $ map snd fieldfieldtys
        ]
      instanceTy ∷ TH.Type
      instanceTy = TH.ConT ''Pretty ⊙ (TH.ConT ty ⊙⋆ tyargVars)
      instanceDec ∷ TH.Dec
      instanceDec = 
        TH.FunD 'pretty 
        $ single 
        $ thSingleClause (single $ TH.RecP con $ tohs $ mapOn fieldNameTmps $ \ (field :* _name :* tmpˣ) → (field :* TH.VarP tmpˣ)) 
        $ TH.VarE 'ppApp 
          ⊙ (TH.VarE 'ppCon ⊙ (thString $ string $ TH.nameBase con)) 
          ⊙$ TH.VarE 'list 
          ⊙$ TH.ListE 
             $ single 
             $ TH.VarE 'ppRecord 
               ⊙ (TH.VarE 'ppPun ⊙ thString "⇒") 
               ⊙$ TH.VarE 'list 
               ⊙$ TH.ListE 
                  $ tohs 
                  $ mapOn fieldNameTmps $ \ (frhs → _field :* name :* tmpˣ) → 
                      TH.ConE '(:*)
                      ⊙ (TH.VarE 'ppString ⊙ (thString name))
                      ⊙ (TH.VarE 'pretty ⊙ TH.VarE tmpˣ)
  return $ single $ TH.InstanceD (tohs None) (tohs instanceCx) instanceTy $ single $ instanceDec

makePrettyRecord ∷ TH.Name → TH.Q [TH.Dec]
makePrettyRecord name = do
  (cx :* ty :* tyargs :* _ :* c :* _) ← return𝑂 (io abortIO) ∘ (thViewSingleConADT *∘ view thTyConIL) *$ TH.reify name
  (con :* fields) ← return𝑂 (io abortIO) $ view thRecCL c
  let fieldfieldtys = mapOn fields $ \ (frhs → field :* _ :* fieldty) → (field :* fieldty)
  map tohs $ makePrettyRecordLogic cx ty tyargs con fieldfieldtys

