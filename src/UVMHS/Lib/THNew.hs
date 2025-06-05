-- ================ --
-- TEMPLATE HASKELL --
-- ================ --

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import qualified Prelude as HS
import qualified Data.List.NonEmpty as HS (NonEmpty(..)) 
import Prelude (Maybe(..))

---------------------
-- GENERAL HELPERS --
---------------------

thGensym ∷ TH.Q TH.Name
thGensym = TH.newName $ tohsChars "x"

thTupsT ∷ [TH.Type] → TH.Type
thTupsT τs = apply TH.AppT (TH.TupleT (HS.length τs)) τs

thTupsTQ ∷ [TH.TypeQ] → TH.TypeQ
thTupsTQ τQs = do
  τs ← exchange τQs
  return $ thTupsT τs

thTyVarBndrName ∷ ∀ flag. TH.TyVarBndr flag → TH.Name
thTyVarBndrName = \case
  TH.PlainTV (name ∷ TH.Name) (_flag ∷ flag) → name
  TH.KindedTV (name ∷ TH.Name) (_flag ∷ flag) (_kind ∷ TH.Kind) → name

thNameString ∷ TH.Name → 𝕊
thNameString (TH.Name (occName ∷ TH.OccName) (_nameFlavor ∷ TH.NameFlavour)) = let TH.OccName s = occName in string s

thShowDecs ∷ TH.DecsQ → TH.ExpQ
thShowDecs decsQ = do
  decs ← decsQ
  let s = TH.pprint decs
  [| s |]

thAllTermNamesTypeWith ∷ (TH.Name → 𝔹) → TH.Type → 𝑃 TH.Name
thAllTermNamesTypeWith f = 
  let loop = \case
        TH.ForallT (_tyVarBndrs ∷ [TH.TyVarBndr TH.Specificity]) (_cxt ∷ TH.Cxt) (ty ∷ TH.Type) → loop ty
        TH.ForallVisT (_tyVarBndrs ∷ [TH.TyVarBndr ()]) (ty ∷ TH.Type) → loop ty
        TH.AppT (ty₁ ∷ TH.Type) (ty₂ ∷ TH.Type) → unions [loop ty₁,loop ty₂]
        TH.AppKindT (ty ∷ TH.Type) (_kind ∷ TH.Kind) → loop ty
        TH.SigT (ty ∷ TH.Type) (_kind ∷ TH.Kind) → loop ty
        TH.VarT (name ∷ TH.Name) → if f name then single name else null
        TH.ConT (name ∷ TH.Name) → if f name then single name else null
        TH.PromotedT (_name ∷ TH.Name) → null
        TH.InfixT (ty₁ ∷ TH.Type) (name ∷ TH.Name) (ty₂ ∷ TH.Type) → unions [if f name then single name else null,loop ty₁,loop ty₂]
        TH.UInfixT (ty₁ ∷ TH.Type) (name ∷ TH.Name) (ty₂ ∷ TH.Type) → unions [if f name then single name else null,loop ty₁,loop ty₂]
        TH.PromotedInfixT (_ty₁ ∷ TH.Type) (_name ∷ TH.Name) (_ty₂ ∷ TH.Type) → null
        TH.PromotedUInfixT (_ty₁ ∷ TH.Type) (_name ∷ TH.Name) (_ty₂ ∷ TH.Type) → null
        TH.ParensT (ty ∷ TH.Type) → loop ty
        TH.TupleT (_i ∷ HS.Int) → null
        TH.UnboxedTupleT (_i ∷ HS.Int) → null
        TH.UnboxedSumT (_i ∷ HS.Int) → null
        TH.ArrowT → null
        TH.MulArrowT → null
        TH.EqualityT → null
        TH.ListT → null
        TH.PromotedTupleT (_i ∷ HS.Int) → null
        TH.PromotedNilT → null
        TH.PromotedConsT → null
        TH.StarT → null
        TH.ConstraintT → null
        TH.LitT (_l ∷ TH.TyLit) → null
        TH.WildCardT → null
        TH.ImplicitParamT (_s ∷ HS.String) (ty ∷ TH.Type) → loop ty
  in loop

thAnyNameOccursInType ∷ 𝑃 TH.Name → TH.Type → 𝔹
thAnyNameOccursInType names τ  = thAllTermNamesTypeWith (∈ names) τ ≢ null

------------------------
-- STRIP MODULE NAMES --
------------------------

thStripModuleNamesNameFlavour ∷ TH.NameFlavour → TH.NameFlavour
thStripModuleNamesNameFlavour = \case
  TH.NameS → 
    TH.NameS
  TH.NameQ (modName ∷ TH.ModName) →
    TH.NameQ modName
  TH.NameU (uniq ∷ TH.Uniq) →
    TH.NameU uniq
  TH.NameL (uniq ∷ TH.Uniq) →
    TH.NameL uniq
  -- THE PRIMARY OPERATION
  TH.NameG (_nameSpace ∷ TH.NameSpace) (_pkgName ∷ TH.PkgName) (_modName ∷ TH.ModName) →
    TH.NameS

thStripModuleNamesName ∷ TH.Name → TH.Name
thStripModuleNamesName = \case
  TH.Name (occName ∷ TH.OccName) (nameFlavor ∷ TH.NameFlavour) →
    TH.Name occName $ thStripModuleNamesNameFlavour nameFlavor

thStripModuleNamesTyVarBndr ∷ ∀ flag. TH.TyVarBndr flag → TH.TyVarBndr flag
thStripModuleNamesTyVarBndr = \case
  TH.PlainTV (name ∷ TH.Name) (flag ∷ flag) →
    TH.PlainTV (thStripModuleNamesName name) flag
  TH.KindedTV (name ∷ TH.Name) (flag ∷ flag) (kind ∷ TH.Kind) →
    TH.KindedTV (thStripModuleNamesName name) flag $ thStripModuleNamesType kind

thStripModuleNamesType ∷ TH.Type → TH.Type
thStripModuleNamesType = \case
  TH.ForallT (tyVarBndrs ∷ [TH.TyVarBndr TH.Specificity]) (cxt ∷ TH.Cxt) (ty ∷ TH.Type) → 
    TH.ForallT (map thStripModuleNamesTyVarBndr tyVarBndrs) (map thStripModuleNamesType cxt) $ thStripModuleNamesType ty
  TH.ForallVisT (tyVarBndrs ∷ [TH.TyVarBndr ()]) (ty ∷ TH.Type) → 
    TH.ForallVisT (map thStripModuleNamesTyVarBndr tyVarBndrs) $ thStripModuleNamesType ty
  TH.AppT (ty₁ ∷ TH.Type) (ty₂ ∷ TH.Type) → TH.AppT (thStripModuleNamesType ty₁) $ thStripModuleNamesType ty₂
  TH.AppKindT (ty ∷ TH.Type) (kind ∷ TH.Kind) → TH.AppKindT (thStripModuleNamesType ty) $ thStripModuleNamesType kind
  TH.SigT (ty ∷ TH.Type) (kind ∷ TH.Kind) → TH.SigT (thStripModuleNamesType ty) $ thStripModuleNamesType kind
  TH.VarT (name ∷ TH.Name) → TH.VarT $ thStripModuleNamesName name
  TH.ConT (name ∷ TH.Name) → TH.ConT $ thStripModuleNamesName name
  TH.PromotedT (name ∷ TH.Name) → TH.PromotedT $ thStripModuleNamesName name
  TH.InfixT (ty₁ ∷ TH.Type) (name ∷ TH.Name) (ty₂ ∷ TH.Type) → 
    TH.InfixT (thStripModuleNamesType ty₁) (thStripModuleNamesName name) $ thStripModuleNamesType ty₂
  TH.UInfixT (ty₁ ∷ TH.Type) (name ∷ TH.Name) (ty₂ ∷ TH.Type) → 
    TH.UInfixT (thStripModuleNamesType ty₁) (thStripModuleNamesName name) $ thStripModuleNamesType ty₂
  TH.PromotedInfixT (ty₁ ∷ TH.Type) (name ∷ TH.Name) (ty₂ ∷ TH.Type) →
    TH.PromotedInfixT (thStripModuleNamesType ty₁) (thStripModuleNamesName name) $ thStripModuleNamesType ty₂
  TH.PromotedUInfixT (ty₁ ∷ TH.Type) (name ∷ TH.Name) (ty₂ ∷ TH.Type) →
    TH.PromotedUInfixT (thStripModuleNamesType ty₁) (thStripModuleNamesName name) $ thStripModuleNamesType ty₂
  TH.ParensT (ty ∷ TH.Type) → TH.ParensT $ thStripModuleNamesType ty
  TH.TupleT (i ∷ HS.Int) → TH.TupleT i
  TH.UnboxedTupleT (i ∷ HS.Int) → TH.UnboxedTupleT i
  TH.UnboxedSumT (i ∷ HS.Int) → TH.UnboxedSumT i
  TH.ArrowT → TH.ArrowT
  TH.MulArrowT → TH.MulArrowT
  TH.EqualityT → TH.EqualityT
  TH.ListT → TH.ListT
  TH.PromotedTupleT (i ∷ HS.Int) → TH.PromotedTupleT i
  TH.PromotedNilT → TH.PromotedNilT
  TH.PromotedConsT → TH.PromotedConsT
  TH.StarT → TH.StarT
  TH.ConstraintT → TH.ConstraintT
  TH.LitT (l ∷ TH.TyLit) → TH.LitT l
  TH.WildCardT → TH.WildCardT
  TH.ImplicitParamT (s ∷ HS.String) (ty ∷ TH.Type) → TH.ImplicitParamT s $ thStripModuleNamesType ty

thStripModuleNamesPat ∷ TH.Pat → TH.Pat
thStripModuleNamesPat = \case
  TH.LitP (lit ∷ TH.Lit) → TH.LitP lit
  TH.VarP (name ∷ TH.Name) → TH.VarP $ thStripModuleNamesName name
  TH.TupP (patList ∷ [TH.Pat]) → TH.TupP $ map thStripModuleNamesPat patList
  TH.UnboxedTupP (patList ∷ [TH.Pat]) → TH.UnboxedTupP $ map thStripModuleNamesPat patList
  TH.UnboxedSumP (pat ∷ TH.Pat) (sumAlt ∷ TH.SumAlt) (sumArity ∷ TH.SumArity) →
    TH.UnboxedSumP (thStripModuleNamesPat pat) sumAlt sumArity
  TH.ConP (name ∷ TH.Name) (tyList ∷ [TH.Type]) (patList ∷ [TH.Pat]) →
    TH.ConP (thStripModuleNamesName name) (map thStripModuleNamesType tyList) $ map thStripModuleNamesPat patList
  TH.InfixP (pat₁ ∷ TH.Pat) (name ∷ TH.Name) (pat₂ ∷ TH.Pat) →
    TH.InfixP (thStripModuleNamesPat pat₁) (thStripModuleNamesName name) $ thStripModuleNamesPat pat₂
  TH.UInfixP (pat₁ ∷ TH.Pat) (name ∷ TH.Name) (pat₂ ∷ TH.Pat) →
    TH.UInfixP (thStripModuleNamesPat pat₁) (thStripModuleNamesName name) $ thStripModuleNamesPat pat₂
  TH.ParensP (pat ∷ TH.Pat) → TH.ParensP $ thStripModuleNamesPat pat
  TH.TildeP (pat ∷ TH.Pat) → TH.TildeP $ thStripModuleNamesPat pat
  TH.BangP (pat ∷ TH.Pat) → TH.BangP $ thStripModuleNamesPat pat
  TH.AsP (name ∷ TH.Name) (pat ∷ TH.Pat) → TH.AsP (thStripModuleNamesName name) $ thStripModuleNamesPat pat
  TH.WildP → TH.WildP
  TH.RecP (name ∷ TH.Name) (fieldPatList ∷ [TH.FieldPat]) → 
    TH.RecP (thStripModuleNamesName name) $ mapOn fieldPatList $ \ (nameᵢ ∷ TH.Name,pat ∷ TH.Pat) → 
      (thStripModuleNamesName nameᵢ,thStripModuleNamesPat pat)
  TH.ListP (patList ∷ [TH.Pat]) → TH.ListP $ map thStripModuleNamesPat patList
  TH.SigP (pat ∷ TH.Pat) (ty ∷ TH.Type) → TH.SigP (thStripModuleNamesPat pat) $ thStripModuleNamesType ty
  TH.ViewP (exp ∷ TH.Exp) (pat ∷ TH.Pat) → TH.ViewP (thStripModuleNamesExp exp) $ thStripModuleNamesPat pat

thStripModuleNamesStmt ∷ TH.Stmt → TH.Stmt
thStripModuleNamesStmt = \case
  TH.BindS (pat ∷ TH.Pat) (exp ∷ TH.Exp) → TH.BindS (thStripModuleNamesPat pat) $ thStripModuleNamesExp exp
  TH.LetS (decList ∷ [TH.Dec]) → TH.LetS $ map thStripModuleNamesDec decList
  TH.NoBindS (exp ∷ TH.Exp) → TH.NoBindS $ thStripModuleNamesExp exp
  TH.ParS (stmtListList ∷ [[TH.Stmt]]) → TH.ParS $ mapp thStripModuleNamesStmt stmtListList
  TH.RecS (stmtList ∷ [TH.Stmt]) → TH.RecS $ map thStripModuleNamesStmt stmtList

thStripModuleNamesGuard ∷ TH.Guard → TH.Guard
thStripModuleNamesGuard = \case
  TH.NormalG (exp ∷ TH.Exp) → TH.NormalG $ thStripModuleNamesExp exp
  TH.PatG (stmtList ∷ [TH.Stmt]) → TH.PatG $ map thStripModuleNamesStmt stmtList

thStripModuleNamesBody ∷ TH.Body → TH.Body
thStripModuleNamesBody = \case
  TH.GuardedB (guardExpList ∷ [(TH.Guard,TH.Exp)]) →
    TH.GuardedB $ mapOn guardExpList $ \ (guard_ ∷ TH.Guard,exp ∷ TH.Exp) → 
      (thStripModuleNamesGuard guard_,thStripModuleNamesExp exp)
  TH.NormalB (exp ∷ TH.Exp) → TH.NormalB $ thStripModuleNamesExp exp

thStripModuleNamesClause ∷ TH.Clause → TH.Clause
thStripModuleNamesClause = \case
  TH.Clause (patList ∷ [TH.Pat]) (body ∷ TH.Body) (decList ∷ [TH.Dec]) →
    TH.Clause (map thStripModuleNamesPat patList) (thStripModuleNamesBody body) $ map thStripModuleNamesDec decList

thStripModuleNamesMatch ∷ TH.Match → TH.Match
thStripModuleNamesMatch = \case
  TH.Match (pat ∷ TH.Pat) (body ∷ TH.Body) (decList ∷ [TH.Dec]) →
    TH.Match (thStripModuleNamesPat pat) (thStripModuleNamesBody body) $ map thStripModuleNamesDec decList

thStripModuleNamesRange ∷ TH.Range → TH.Range
thStripModuleNamesRange = \case
  TH.FromR (exp ∷ TH.Exp) → TH.FromR $ thStripModuleNamesExp exp
  TH.FromThenR (exp₁ ∷ TH.Exp) (exp₂ ∷ TH.Exp) → TH.FromThenR (thStripModuleNamesExp exp₁) $ thStripModuleNamesExp exp₂
  TH.FromToR (exp₁ ∷ TH.Exp) (exp₂ ∷ TH.Exp) → TH.FromToR (thStripModuleNamesExp exp₁) $ thStripModuleNamesExp exp₂
  TH.FromThenToR (exp₁ ∷ TH.Exp) (exp₂ ∷ TH.Exp) (exp₃ ∷ TH.Exp) → 
    TH.FromThenToR (thStripModuleNamesExp exp₁) (thStripModuleNamesExp exp₂) $ thStripModuleNamesExp exp₃

thStripModuleNamesExp ∷ TH.Exp → TH.Exp
thStripModuleNamesExp = \case
  TH.VarE (name ∷ TH.Name) → TH.VarE $ thStripModuleNamesName name
  TH.ConE (name ∷ TH.Name) → TH.ConE $ thStripModuleNamesName name
  TH.LitE (lit ∷ TH.Lit) → TH.LitE lit
  TH.AppE (exp₁ ∷ TH.Exp) (exp₂ ∷ TH.Exp) → TH.AppE (thStripModuleNamesExp exp₁) $ thStripModuleNamesExp exp₂
  TH.AppTypeE (exp ∷ TH.Exp) (ty ∷ TH.Type) → TH.AppTypeE (thStripModuleNamesExp exp) $ thStripModuleNamesType ty
  TH.InfixE (expMaybe₁ ∷ Maybe TH.Exp) (exp ∷ TH.Exp) (expMaybe₂ ∷ Maybe TH.Exp) →
    TH.InfixE (HS.fmap thStripModuleNamesExp expMaybe₁) (thStripModuleNamesExp exp) $ HS.fmap thStripModuleNamesExp expMaybe₂
  TH.UInfixE (exp₁ ∷ TH.Exp) (exp₂ ∷ TH.Exp) (exp₃ ∷ TH.Exp) →
    TH.UInfixE (thStripModuleNamesExp exp₁) (thStripModuleNamesExp exp₂) $ thStripModuleNamesExp exp₃
  TH.ParensE (exp ∷ TH.Exp) → TH.ParensE $ thStripModuleNamesExp exp
  TH.LamE (patList ∷ [TH.Pat]) (exp ∷ TH.Exp) → TH.LamE (map thStripModuleNamesPat patList) $ thStripModuleNamesExp exp
  TH.LamCaseE (matchList ∷ [TH.Match]) → TH.LamCaseE $ map thStripModuleNamesMatch matchList
  TH.LamCasesE (clauseList ∷ [TH.Clause]) → TH.LamCasesE $ map thStripModuleNamesClause clauseList
  TH.TupE (expMaybeList ∷ [Maybe TH.Exp]) → TH.TupE $ map (HS.fmap thStripModuleNamesExp) expMaybeList
  TH.UnboxedTupE (expMaybeList ∷ [Maybe TH.Exp]) → TH.UnboxedTupE $ map (HS.fmap thStripModuleNamesExp) expMaybeList
  TH.UnboxedSumE (exp ∷ TH.Exp) (sumAlt ∷ TH.SumAlt) (sumArity ∷ TH.SumArity) → 
    TH.UnboxedSumE (thStripModuleNamesExp exp) sumAlt sumArity
  TH.CondE (exp₁ ∷ TH.Exp) (exp₂ ∷ TH.Exp) (exp₃ ∷ TH.Exp) →
    TH.CondE (thStripModuleNamesExp exp₁) (thStripModuleNamesExp exp₂) $ thStripModuleNamesExp exp₃
  TH.MultiIfE (guardExpList ∷ [(TH.Guard,TH.Exp)]) →
    TH.MultiIfE $ mapOn guardExpList $ \ (grd ∷ TH.Guard,exp ∷ TH.Exp) → (thStripModuleNamesGuard grd,thStripModuleNamesExp exp)
  TH.LetE (decList ∷ [TH.Dec]) (exp ∷ TH.Exp) → TH.LetE (map thStripModuleNamesDec decList) $ thStripModuleNamesExp exp
  TH.CaseE (exp ∷ TH.Exp) (matchList ∷ [TH.Match]) → TH.CaseE (thStripModuleNamesExp exp) $ map thStripModuleNamesMatch matchList
  TH.DoE (modNameMaybe ∷ Maybe TH.ModName) (stmtList ∷ [TH.Stmt]) → TH.DoE modNameMaybe $ map thStripModuleNamesStmt stmtList
  TH.MDoE (modNameMaybe ∷ Maybe TH.ModName) (stmtList ∷ [TH.Stmt]) → TH.MDoE modNameMaybe $ map thStripModuleNamesStmt stmtList
  TH.CompE (stmtList ∷ [TH.Stmt]) → TH.CompE $ map thStripModuleNamesStmt stmtList
  TH.ArithSeqE (range_ ∷ TH.Range) → TH.ArithSeqE $ thStripModuleNamesRange range_
  TH.ListE (expList ∷ [TH.Exp]) → TH.ListE $ map thStripModuleNamesExp expList
  TH.SigE (exp ∷ TH.Exp) (ty ∷ TH.Type) → TH.SigE (thStripModuleNamesExp exp) $ thStripModuleNamesType ty
  TH.RecConE (name ∷ TH.Name) (fieldExpList ∷ [TH.FieldExp]) →
    TH.RecConE (thStripModuleNamesName name) $ mapOn fieldExpList $ \ (nameᵢ ∷ TH.Name,exp ∷ TH.Exp) → 
      (thStripModuleNamesName nameᵢ,thStripModuleNamesExp exp)
  TH.RecUpdE (exp ∷ TH.Exp) (fieldExpList ∷ [TH.FieldExp]) →
    TH.RecUpdE (thStripModuleNamesExp exp) $ mapOn fieldExpList $ \ (nameᵢ ∷ TH.Name,expᵢ ∷ TH.Exp) → 
      (thStripModuleNamesName nameᵢ,thStripModuleNamesExp expᵢ)
  TH.StaticE (exp ∷ TH.Exp) → TH.StaticE $ thStripModuleNamesExp exp
  TH.UnboundVarE (name ∷ TH.Name) → TH.UnboundVarE $ thStripModuleNamesName name
  TH.LabelE (charList ∷ [ℂ]) → TH.LabelE charList
  TH.ImplicitParamVarE (charList ∷ [ℂ]) → TH.ImplicitParamVarE charList
  TH.GetFieldE (exp ∷ TH.Exp) (charList ∷ [ℂ]) → TH.GetFieldE (thStripModuleNamesExp exp) charList
  TH.ProjectionE (stringNonEmpty ∷ HS.NonEmpty [ℂ]) → TH.ProjectionE stringNonEmpty
  TH.TypedBracketE (exp ∷ TH.Exp) → TH.TypedBracketE $ thStripModuleNamesExp exp
  TH.TypedSpliceE (exp ∷ TH.Exp) → TH.TypedSpliceE $ thStripModuleNamesExp exp

thStripModuleNamesCon ∷ TH.Con → TH.Con
thStripModuleNamesCon = \case
  TH.NormalC (name ∷ TH.Name) (bangTypeList ∷ [TH.BangType]) → 
    TH.NormalC (thStripModuleNamesName name) $ mapOn bangTypeList $ \ (bang ∷ TH.Bang,ty ∷ TH.Type) → 
      (bang,thStripModuleNamesType ty)
  TH.RecC (name ∷ TH.Name) (varBangTypeList ∷ [TH.VarBangType]) →
    TH.RecC (thStripModuleNamesName name) $ mapOn varBangTypeList $ \ (nameᵢ ∷ TH.Name,bang ∷ TH.Bang,ty ∷ TH.Type) →
      (thStripModuleNamesName nameᵢ,bang,thStripModuleNamesType ty)
  TH.InfixC (bang₁ ∷ TH.Bang,ty₁ ∷ TH.Type) (name ∷ TH.Name) (bang₂ ∷ TH.Bang,ty₂ ∷ TH.Type) → 
    TH.InfixC (bang₁,thStripModuleNamesType ty₁) (thStripModuleNamesName name) (bang₂,thStripModuleNamesType ty₂)
  TH.ForallC (tyVarBndrList ∷ [TH.TyVarBndr TH.Specificity]) (cxt ∷ TH.Cxt) (con ∷ TH.Con) →
    TH.ForallC (map thStripModuleNamesTyVarBndr tyVarBndrList) (map thStripModuleNamesType cxt) $ thStripModuleNamesCon con
  TH.GadtC (nameList ∷ [TH.Name]) (bangTypeList ∷ [TH.BangType]) (ty ∷ TH.Type) →
    let bangTypeList' = mapOn bangTypeList $ \ (bang ∷ TH.Bang,tyᵢ ∷ TH.Type) → (bang,thStripModuleNamesType tyᵢ) in
    TH.GadtC (map thStripModuleNamesName nameList) bangTypeList' $ thStripModuleNamesType ty
  TH.RecGadtC (nameList ∷ [TH.Name]) (varBangTypeList ∷ [TH.VarBangType]) (ty ∷ TH.Type) →
    let varBangTypeList' = mapOn varBangTypeList $ \ (name ∷ TH.Name,bang ∷ TH.Bang,tyᵢ ∷ TH.Type) → 
          (thStripModuleNamesName name,bang,thStripModuleNamesType tyᵢ) 
    in
    TH.RecGadtC (map thStripModuleNamesName nameList) varBangTypeList' $ thStripModuleNamesType ty

thStripModuleNamesDerivClause ∷ TH.DerivClause → TH.DerivClause
thStripModuleNamesDerivClause = \case
  TH.DerivClause (derivStrategyMaybe ∷ Maybe TH.DerivStrategy) (cxt ∷ TH.Cxt) →
    TH.DerivClause derivStrategyMaybe $ map thStripModuleNamesType cxt

thStripModuleNamesFunDep ∷ TH.FunDep → TH.FunDep
thStripModuleNamesFunDep = \case
  TH.FunDep (names₁ ∷ [TH.Name]) (names₂ ∷ [TH.Name]) →
    TH.FunDep (map thStripModuleNamesName names₁) $ map thStripModuleNamesName names₂

thStripModuleNamesForeign ∷ TH.Foreign → TH.Foreign
thStripModuleNamesForeign = \case
  TH.ImportF (callconv ∷ TH.Callconv) (safety ∷ TH.Safety) (charList ∷ [ℂ]) (name ∷ TH.Name) (ty ∷ TH.Type) →
    TH.ImportF callconv safety charList (thStripModuleNamesName name) $ thStripModuleNamesType ty
  TH.ExportF (callconv ∷ TH.Callconv) (charList ∷ [ℂ]) (name ∷ TH.Name) (ty ∷ TH.Type) →
    TH.ExportF callconv charList (thStripModuleNamesName name) $ thStripModuleNamesType ty

thStripModuleNamesRuleBndr ∷ TH.RuleBndr → TH.RuleBndr
thStripModuleNamesRuleBndr = \case
  TH.RuleVar (name ∷ TH.Name) → TH.RuleVar $ thStripModuleNamesName name
  TH.TypedRuleVar (name ∷ TH.Name) (ty ∷ TH.Type) → TH.TypedRuleVar (thStripModuleNamesName name) $ thStripModuleNamesType ty

thStripModuleNamesAnnTarget ∷ TH.AnnTarget → TH.AnnTarget
thStripModuleNamesAnnTarget = \case
  TH.ModuleAnnotation → TH.ModuleAnnotation
  TH.TypeAnnotation (name ∷ TH.Name) → TH.TypeAnnotation $ thStripModuleNamesName name
  TH.ValueAnnotation (name ∷ TH.Name) → TH.ValueAnnotation $ thStripModuleNamesName name

thStripModuleNamesPragma ∷ TH.Pragma → TH.Pragma
thStripModuleNamesPragma = \case
  TH.InlineP (name ∷ TH.Name) (inline ∷ TH.Inline) (ruleMatch ∷ TH.RuleMatch) (phases ∷ TH.Phases) →
    TH.InlineP (thStripModuleNamesName name) inline ruleMatch phases
  TH.OpaqueP (name ∷ TH.Name) → TH.OpaqueP $ thStripModuleNamesName name
  TH.SpecialiseP (name ∷ TH.Name) (ty ∷ TH.Type) (inlineMaybe ∷ Maybe TH.Inline) (phases ∷ TH.Phases) →
    TH.SpecialiseP (thStripModuleNamesName name) (thStripModuleNamesType ty) inlineMaybe phases
  TH.SpecialiseInstP (ty ∷ TH.Type) → TH.SpecialiseInstP $ thStripModuleNamesType ty
  TH.RuleP 
    (charList ∷ [ℂ]) (tyVarBndrListMaybe ∷ Maybe [TH.TyVarBndr ()]) (ruleBndrList ∷ [TH.RuleBndr]) 
    (exp₁ ∷ TH.Exp) (exp₂ ∷ TH.Exp) (phases ∷ TH.Phases) →
      TH.RuleP 
        charList (HS.fmap (map thStripModuleNamesTyVarBndr) tyVarBndrListMaybe) (map thStripModuleNamesRuleBndr ruleBndrList) 
        (thStripModuleNamesExp exp₁) (thStripModuleNamesExp exp₂) phases
  TH.AnnP (annTarget ∷ TH.AnnTarget) (exp ∷ TH.Exp) → TH.AnnP (thStripModuleNamesAnnTarget annTarget) $ thStripModuleNamesExp exp
  TH.LineP (i ∷ HS.Int) (charList ∷ [ℂ]) → TH.LineP i charList
  TH.CompleteP (nameList ∷ [TH.Name]) (nameMaybe ∷ Maybe TH.Name) →
    TH.CompleteP (map thStripModuleNamesName nameList) $ HS.fmap thStripModuleNamesName  nameMaybe

thStripModuleNamesTySynEqn ∷ TH.TySynEqn → TH.TySynEqn
thStripModuleNamesTySynEqn = \case
  TH.TySynEqn (tyVarBndrListMaybe ∷ Maybe [TH.TyVarBndr ()]) (ty₁ ∷ TH.Type) (ty₂ ∷ TH.Type) →
    TH.TySynEqn (HS.fmap (map thStripModuleNamesTyVarBndr) tyVarBndrListMaybe) (thStripModuleNamesType ty₁) $ thStripModuleNamesType ty₂

thStripModuleNamesFamilyResultSig ∷ TH.FamilyResultSig → TH.FamilyResultSig
thStripModuleNamesFamilyResultSig = \case
  TH.NoSig →
    TH.NoSig
  TH.KindSig (kind ∷ TH.Kind) →
    TH.KindSig $ thStripModuleNamesType kind
  TH.TyVarSig (tyVarBndr ∷ TH.TyVarBndr ()) →
    TH.TyVarSig $ thStripModuleNamesTyVarBndr tyVarBndr

thStripModuleNamesInjectivityAnn ∷ TH.InjectivityAnn → TH.InjectivityAnn
thStripModuleNamesInjectivityAnn = \case
  TH.InjectivityAnn (name ∷ TH.Name) (nameList ∷ [TH.Name]) →
    TH.InjectivityAnn (thStripModuleNamesName name) $ map thStripModuleNamesName nameList

thStripModuleNamesTypeFamilyHead ∷ TH.TypeFamilyHead → TH.TypeFamilyHead
thStripModuleNamesTypeFamilyHead = \case
  TH.TypeFamilyHead 
    (name ∷ TH.Name) 
    (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis]) 
    (familyResultSig ∷ TH.FamilyResultSig)
    (injectivityAnnMaybe ∷ Maybe TH.InjectivityAnn) →
      TH.TypeFamilyHead 
        (thStripModuleNamesName name) 
        (map thStripModuleNamesTyVarBndr tyVarBndrList)
        (thStripModuleNamesFamilyResultSig familyResultSig) $
        HS.fmap thStripModuleNamesInjectivityAnn injectivityAnnMaybe

thStripModuleNamesDerivStrategy ∷ TH.DerivStrategy → TH.DerivStrategy
thStripModuleNamesDerivStrategy = \case
  TH.StockStrategy → TH.StockStrategy
  TH.AnyclassStrategy → TH.AnyclassStrategy
  TH.NewtypeStrategy → TH.NewtypeStrategy
  TH.ViaStrategy (ty ∷ TH.Type) → TH.ViaStrategy $ thStripModuleNamesType ty

thStripModuleNamesPatSynArgs ∷ TH.PatSynArgs → TH.PatSynArgs
thStripModuleNamesPatSynArgs = \case
  TH.PrefixPatSyn (nameList ∷ [TH.Name]) →
    TH.PrefixPatSyn $ map thStripModuleNamesName nameList
  TH.InfixPatSyn (name₁ ∷ TH.Name) (name₂ ∷ TH.Name) →
    TH.InfixPatSyn (thStripModuleNamesName name₁) $ thStripModuleNamesName name₂
  TH.RecordPatSyn (nameList ∷ [TH.Name]) →
    TH.RecordPatSyn $ map thStripModuleNamesName nameList

thStripModuleNamesPatSynDir ∷ TH.PatSynDir → TH.PatSynDir
thStripModuleNamesPatSynDir = \case
  TH.Unidir →
    TH.Unidir
  TH.ImplBidir →
    TH.ImplBidir
  TH.ExplBidir (clauseList ∷ [TH.Clause]) →
    TH.ExplBidir $ map thStripModuleNamesClause clauseList

thStripModuleNamesDec ∷ TH.Dec → TH.Dec
thStripModuleNamesDec = \case
  TH.FunD (name ∷ TH.Name) (clauseList ∷ [TH.Clause]) → 
    TH.FunD (thStripModuleNamesName name) $ map thStripModuleNamesClause clauseList
  TH.ValD (pat ∷ TH.Pat) (body ∷ TH.Body) (decList ∷ [TH.Dec]) →
    TH.ValD (thStripModuleNamesPat pat) (thStripModuleNamesBody body) $ map thStripModuleNamesDec decList
  TH.DataD 
    (cxt ∷ TH.Cxt) (name ∷ TH.Name) (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis]) 
    (kindMaybe ∷ Maybe TH.Kind) (conList ∷ [TH.Con]) (derivClauseList ∷ [TH.DerivClause]) →
      TH.DataD 
        (map thStripModuleNamesType cxt) (thStripModuleNamesName name) (map thStripModuleNamesTyVarBndr tyVarBndrList) 
        (HS.fmap thStripModuleNamesType kindMaybe) (map thStripModuleNamesCon conList) $ map thStripModuleNamesDerivClause derivClauseList
  TH.NewtypeD
    (cxt ∷ TH.Cxt) (name ∷ TH.Name) (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis]) 
    (kindMaybe ∷ Maybe TH.Kind) (con ∷ TH.Con) (derivClauseList ∷ [TH.DerivClause]) →
      TH.NewtypeD 
        (map thStripModuleNamesType cxt) (thStripModuleNamesName name) (map thStripModuleNamesTyVarBndr tyVarBndrList) 
        (HS.fmap thStripModuleNamesType kindMaybe) (thStripModuleNamesCon con) $ map thStripModuleNamesDerivClause derivClauseList
  TH.TypeDataD (name ∷ TH.Name) (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis]) (kindMaybe ∷ Maybe TH.Kind) (conList ∷ [TH.Con]) →
    TH.TypeDataD 
      (thStripModuleNamesName name) (map thStripModuleNamesTyVarBndr tyVarBndrList) 
      (HS.fmap thStripModuleNamesType kindMaybe) $ map thStripModuleNamesCon conList
  TH.TySynD (name ∷ TH.Name) (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis]) (ty ∷ TH.Type) →
      TH.TySynD (thStripModuleNamesName name) (map thStripModuleNamesTyVarBndr tyVarBndrList) $ thStripModuleNamesType ty
  TH.ClassD (cxt ∷ TH.Cxt) (name ∷ TH.Name) (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis]) (funDepList ∷ [TH.FunDep]) (decList ∷ [TH.Dec]) →
    TH.ClassD 
      (map thStripModuleNamesType cxt) (thStripModuleNamesName name) (map thStripModuleNamesTyVarBndr tyVarBndrList) 
      (map thStripModuleNamesFunDep funDepList) $ map thStripModuleNamesDec decList
  TH.InstanceD (overlapMaybe ∷ Maybe TH.Overlap) (cxt ∷ TH.Cxt) (ty ∷ TH.Type) (decList ∷ [TH.Dec]) →
    TH.InstanceD overlapMaybe (map thStripModuleNamesType cxt) (thStripModuleNamesType ty) $ map thStripModuleNamesDec decList
  TH.SigD (name ∷ TH.Name) (ty ∷ TH.Type) → TH.SigD (thStripModuleNamesName name) $ thStripModuleNamesType ty
  TH.KiSigD (name ∷ TH.Name) (kind ∷ TH.Kind) → TH.KiSigD (thStripModuleNamesName name) $ thStripModuleNamesType kind
  TH.ForeignD (foreign_ ∷ TH.Foreign) → TH.ForeignD $ thStripModuleNamesForeign foreign_
  TH.InfixD (fixity ∷ TH.Fixity) (name ∷ TH.Name) → TH.InfixD fixity $ thStripModuleNamesName name
  TH.DefaultD (tyList ∷ [TH.Type]) → TH.DefaultD $ map thStripModuleNamesType tyList
  TH.PragmaD (pragma ∷ TH.Pragma) → TH.PragmaD $ thStripModuleNamesPragma pragma
  TH.DataFamilyD (name ∷ TH.Name) (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis]) (kindMaybe ∷ Maybe TH.Kind) →
      TH.DataFamilyD (thStripModuleNamesName name) (map thStripModuleNamesTyVarBndr tyVarBndrList) $ HS.fmap thStripModuleNamesType kindMaybe
  TH.DataInstD
    (cxt ∷ TH.Cxt) (tyVarBndrListMaybe ∷ Maybe [TH.TyVarBndr ()]) (ty ∷ TH.Type)
    (kindMaybe ∷ Maybe TH.Kind) (conList ∷ [TH.Con]) (derivClauseList ∷ [TH.DerivClause]) →
      TH.DataInstD 
        (map thStripModuleNamesType cxt) (HS.fmap (map thStripModuleNamesTyVarBndr) tyVarBndrListMaybe) 
        (thStripModuleNamesType ty) (HS.fmap thStripModuleNamesType kindMaybe)
        (map thStripModuleNamesCon conList) $ map thStripModuleNamesDerivClause derivClauseList
  TH.NewtypeInstD
    (cxt ∷ TH.Cxt) (tyVarBndrListMaybe ∷ Maybe [TH.TyVarBndr ()]) (ty ∷ TH.Type)
    (kindMaybe ∷ Maybe TH.Kind) (con ∷ TH.Con) (derivClauseList ∷ [TH.DerivClause]) →
      TH.NewtypeInstD 
        (map thStripModuleNamesType cxt) (HS.fmap (map thStripModuleNamesTyVarBndr) tyVarBndrListMaybe) 
        (thStripModuleNamesType ty) (HS.fmap thStripModuleNamesType kindMaybe) (thStripModuleNamesCon con) $
        map thStripModuleNamesDerivClause derivClauseList
  TH.TySynInstD (tySynEqn ∷ TH.TySynEqn) → TH.TySynInstD $ thStripModuleNamesTySynEqn tySynEqn
  TH.OpenTypeFamilyD (typeFamilyHead ∷ TH.TypeFamilyHead) → TH.OpenTypeFamilyD $ thStripModuleNamesTypeFamilyHead typeFamilyHead
  TH.ClosedTypeFamilyD (typeFamilyHead ∷ TH.TypeFamilyHead) (tySynEqnList ∷ [TH.TySynEqn]) →
      TH.ClosedTypeFamilyD (thStripModuleNamesTypeFamilyHead typeFamilyHead) $ map thStripModuleNamesTySynEqn tySynEqnList
  TH.RoleAnnotD (name ∷ TH.Name) (roleList ∷ [TH.Role]) → TH.RoleAnnotD (thStripModuleNamesName name) roleList
  TH.StandaloneDerivD (derivStrategyMaybe ∷ Maybe TH.DerivStrategy) (cxt ∷ TH.Cxt) (ty ∷ TH.Type) →
      TH.StandaloneDerivD 
        (HS.fmap thStripModuleNamesDerivStrategy derivStrategyMaybe) (map thStripModuleNamesType cxt) $ thStripModuleNamesType ty
  TH.DefaultSigD (name ∷ TH.Name) (ty ∷ TH.Type) →
    TH.DefaultSigD (thStripModuleNamesName name) $ thStripModuleNamesType ty
  TH.PatSynD (name ∷ TH.Name) (patSynArgs ∷ TH.PatSynArgs) (patSynDir ∷ TH.PatSynDir) (pat ∷ TH.Pat) →
      TH.PatSynD 
        (thStripModuleNamesName name) (thStripModuleNamesPatSynArgs patSynArgs) 
        (thStripModuleNamesPatSynDir patSynDir) $ thStripModuleNamesPat pat
  TH.PatSynSigD (name ∷ TH.Name) (patSynType ∷ TH.PatSynType) → TH.PatSynSigD (thStripModuleNamesName name) $ thStripModuleNamesType patSynType
  TH.ImplicitParamBindD (charList ∷ [ℂ]) (exp ∷ TH.Exp) → TH.ImplicitParamBindD charList $ thStripModuleNamesExp exp

------------------------
-- IDENTITY FUNCTIONS --
------------------------

thIdOccName ∷ TH.OccName → TH.OccName
thIdOccName = \case
  TH.OccName (charList ∷ [ℂ]) →
    TH.OccName charList

thIdNameFlavour ∷ TH.NameFlavour → TH.NameFlavour
thIdNameFlavour = \case
  TH.NameS → 
    TH.NameS
  TH.NameQ (modName ∷ TH.ModName) →
    TH.NameQ modName
  TH.NameU (uniq ∷ TH.Uniq) →
    TH.NameU uniq
  TH.NameL (uniq ∷ TH.Uniq) →
    TH.NameL uniq
  TH.NameG (nameSpace ∷ TH.NameSpace) (pkgName ∷ TH.PkgName) (modName ∷ TH.ModName) →
    TH.NameG nameSpace pkgName modName

thIdName ∷ TH.Name → TH.Name
thIdName = \case
  TH.Name (occName ∷ TH.OccName) (nameFlavor ∷ TH.NameFlavour) →
    TH.Name occName nameFlavor
  
thIdTyVarBndr ∷ ∀ flag. TH.TyVarBndr flag → TH.TyVarBndr flag
thIdTyVarBndr = \case
  TH.PlainTV (name ∷ TH.Name) (flag ∷ flag) →
    TH.PlainTV name flag
  TH.KindedTV (name ∷ TH.Name) (flag ∷ flag) (kind ∷ TH.Kind) →
    TH.KindedTV name flag kind

thIdType ∷ TH.Type → TH.Type
thIdType = \case
  TH.ForallT (tyVarBndrs ∷ [TH.TyVarBndr TH.Specificity]) (cxt ∷ TH.Cxt) (ty ∷ TH.Type) → 
    TH.ForallT tyVarBndrs cxt ty
  TH.ForallVisT (tyVarBndrs ∷ [TH.TyVarBndr ()]) (ty ∷ TH.Type) → 
    TH.ForallVisT tyVarBndrs ty
  TH.AppT (ty₁ ∷ TH.Type) (ty₂ ∷ TH.Type) → 
    TH.AppT ty₁ ty₂
  TH.AppKindT (ty ∷ TH.Type) (kind ∷ TH.Kind) → 
    TH.AppKindT ty kind
  TH.SigT (ty ∷ TH.Type) (kind ∷ TH.Kind) → 
    TH.SigT ty kind
  TH.VarT (name ∷ TH.Name) →
    TH.VarT name
  TH.ConT (name ∷ TH.Name) →
    TH.ConT name
  TH.PromotedT (name ∷ TH.Name) →
    TH.PromotedT name
  TH.InfixT (ty₁ ∷ TH.Type) (name ∷ TH.Name) (ty₂ ∷ TH.Type) → 
    TH.InfixT ty₁ name ty₂
  TH.UInfixT (ty₁ ∷ TH.Type) (name ∷ TH.Name) (ty₂ ∷ TH.Type) → 
    TH.UInfixT ty₁ name ty₂
  TH.PromotedInfixT (ty₁ ∷ TH.Type) (name ∷ TH.Name) (ty₂ ∷ TH.Type) →
    TH.PromotedInfixT ty₁ name ty₂
  TH.PromotedUInfixT (ty₁ ∷ TH.Type) (name ∷ TH.Name) (ty₂ ∷ TH.Type) →
    TH.PromotedUInfixT ty₁ name ty₂
  TH.ParensT (ty ∷ TH.Type) →
    TH.ParensT ty
  TH.TupleT (i ∷ HS.Int) → 
    TH.TupleT i
  TH.UnboxedTupleT (i ∷ HS.Int) →
    TH.UnboxedTupleT i
  TH.UnboxedSumT (i ∷ HS.Int) → 
    TH.UnboxedSumT i
  TH.ArrowT → 
    TH.ArrowT
  TH.MulArrowT → 
    TH.MulArrowT
  TH.EqualityT → 
    TH.EqualityT
  TH.ListT → 
    TH.ListT
  TH.PromotedTupleT (i ∷ HS.Int) → 
    TH.PromotedTupleT i
  TH.PromotedNilT → 
    TH.PromotedNilT
  TH.PromotedConsT → 
    TH.PromotedConsT
  TH.StarT → 
    TH.StarT
  TH.ConstraintT → 
    TH.ConstraintT
  TH.LitT (l ∷ TH.TyLit) → 
    TH.LitT l
  TH.WildCardT → 
    TH.WildCardT
  TH.ImplicitParamT (s ∷ HS.String) (ty ∷ TH.Type) → 
    TH.ImplicitParamT s ty

thIdPat ∷ TH.Pat → TH.Pat
thIdPat = \case
  TH.LitP (lit ∷ TH.Lit) →
    TH.LitP lit
  TH.VarP (name ∷ TH.Name) →
    TH.VarP name
  TH.TupP (patList ∷ [TH.Pat]) →
    TH.TupP patList
  TH.UnboxedTupP (patList ∷ [TH.Pat]) →
    TH.UnboxedTupP patList
  TH.UnboxedSumP (pat ∷ TH.Pat) (sumAlt ∷ TH.SumAlt) (sumArity ∷ TH.SumArity) →
    TH.UnboxedSumP pat sumAlt sumArity
  TH.ConP (name ∷ TH.Name) (tyList ∷ [TH.Type]) (patList ∷ [TH.Pat]) →
    TH.ConP name tyList patList
  TH.InfixP (pat₁ ∷ TH.Pat) (name ∷ TH.Name) (pat₂ ∷ TH.Pat) →
    TH.InfixP pat₁ name pat₂
  TH.UInfixP (pat₁ ∷ TH.Pat) (name ∷ TH.Name) (pat₂ ∷ TH.Pat) →
    TH.UInfixP pat₁ name pat₂
  TH.ParensP (pat ∷ TH.Pat) →
    TH.ParensP pat
  TH.TildeP (pat ∷ TH.Pat) →
    TH.TildeP pat
  TH.BangP (pat ∷ TH.Pat) →
    TH.BangP pat
  TH.AsP (name ∷ TH.Name) (pat ∷ TH.Pat) →
    TH.AsP name pat
  TH.WildP →
    TH.WildP
  TH.RecP (name ∷ TH.Name) (fieldPatList ∷ [TH.FieldPat]) →
    TH.RecP name fieldPatList
  TH.ListP (patList ∷ [TH.Pat]) →
    TH.ListP patList
  TH.SigP (pat ∷ TH.Pat) (ty ∷ TH.Type) →
    TH.SigP pat ty
  TH.ViewP (exp ∷ TH.Exp) (pat ∷ TH.Pat) →
    TH.ViewP exp pat

thIdStmt ∷ TH.Stmt → TH.Stmt
thIdStmt = \case
  TH.BindS (pat ∷ TH.Pat) (exp ∷ TH.Exp) →
    TH.BindS pat exp
  TH.LetS (decList ∷ [TH.Dec]) →
    TH.LetS decList
  TH.NoBindS (exp ∷ TH.Exp) →
    TH.NoBindS exp
  TH.ParS (stmtListList ∷ [[TH.Stmt]]) →
    TH.ParS stmtListList
  TH.RecS (stmtList ∷ [TH.Stmt]) →
    TH.RecS stmtList

thIdGuard ∷ TH.Guard → TH.Guard
thIdGuard = \case
  TH.NormalG (exp ∷ TH.Exp) → 
    TH.NormalG exp
  TH.PatG (stmtList ∷ [TH.Stmt]) →
    TH.PatG stmtList

thIdBody ∷ TH.Body → TH.Body
thIdBody = \case
  TH.GuardedB (guardExpList ∷ [(TH.Guard,TH.Exp)]) →
    TH.GuardedB guardExpList
  TH.NormalB (exp ∷ TH.Exp) →
    TH.NormalB exp

thIdClause ∷ TH.Clause → TH.Clause
thIdClause = \case
  TH.Clause (patList ∷ [TH.Pat]) (body ∷ TH.Body) (decList ∷ [TH.Dec]) →
    TH.Clause patList body decList

thIdMatch ∷ TH.Match → TH.Match
thIdMatch = \case
  TH.Match (pat ∷ TH.Pat) (body ∷ TH.Body) (decList ∷ [TH.Dec]) →
    TH.Match pat body decList

thIdRange ∷ TH.Range → TH.Range
thIdRange = \case
  TH.FromR (exp ∷ TH.Exp) → TH.FromR exp
  TH.FromThenR (exp₁ ∷ TH.Exp) (exp₂ ∷ TH.Exp) → TH.FromThenR exp₁ exp₂
  TH.FromToR (exp₁ ∷ TH.Exp) (exp₂ ∷ TH.Exp) → TH.FromToR exp₁ exp₂
  TH.FromThenToR (exp₁ ∷ TH.Exp) (exp₂ ∷ TH.Exp) (exp₃ ∷ TH.Exp) → TH.FromThenToR exp₁ exp₂ exp₃

thIdExp ∷ TH.Exp → TH.Exp
thIdExp = \case
  TH.VarE (name ∷ TH.Name) →
    TH.VarE name
  TH.ConE (name ∷ TH.Name) →
    TH.ConE name
  TH.LitE (lit ∷ TH.Lit) →
    TH.LitE lit
  TH.AppE (exp₁ ∷ TH.Exp) (exp₂ ∷ TH.Exp) →
    TH.AppE exp₁ exp₂
  TH.AppTypeE (exp ∷ TH.Exp) (ty ∷ TH.Type) →
    TH.AppTypeE exp ty
  TH.InfixE (expMaybe₁ ∷ Maybe TH.Exp) (exp ∷ TH.Exp) (expMaybe₂ ∷ Maybe TH.Exp) →
    TH.InfixE expMaybe₁ exp expMaybe₂
  TH.UInfixE (exp₁ ∷ TH.Exp) (exp₂ ∷ TH.Exp) (exp₃ ∷ TH.Exp) →
    TH.UInfixE exp₁ exp₂ exp₃
  TH.ParensE (exp ∷ TH.Exp) →
    TH.ParensE exp
  TH.LamE (patList ∷ [TH.Pat]) (exp ∷ TH.Exp) →
    TH.LamE patList exp
  TH.LamCaseE (matchList ∷ [TH.Match]) →
    TH.LamCaseE matchList
  TH.LamCasesE (clauseList ∷ [TH.Clause]) →
    TH.LamCasesE clauseList
  TH.TupE (expMaybeList ∷ [Maybe TH.Exp]) →
    TH.TupE expMaybeList
  TH.UnboxedTupE (expMaybeList ∷ [Maybe TH.Exp]) →
    TH.UnboxedTupE expMaybeList
  TH.UnboxedSumE (exp ∷ TH.Exp) (sumAlt ∷ TH.SumAlt) (sumArity ∷ TH.SumArity) →
    TH.UnboxedSumE exp sumAlt sumArity
  TH.CondE (exp₁ ∷ TH.Exp) (exp₂ ∷ TH.Exp) (exp₃ ∷ TH.Exp) →
    TH.CondE exp₁ exp₂ exp₃
  TH.MultiIfE (guardExpList ∷ [(TH.Guard,TH.Exp)]) →
    TH.MultiIfE guardExpList
  TH.LetE (decList ∷ [TH.Dec]) (exp ∷ TH.Exp) →
    TH.LetE decList exp
  TH.CaseE (exp ∷ TH.Exp) (matchList ∷ [TH.Match]) →
    TH.CaseE exp matchList
  TH.DoE (modNameMaybe ∷ Maybe TH.ModName) (stmtList ∷ [TH.Stmt]) →
    TH.DoE modNameMaybe stmtList
  TH.MDoE (modNameMaybe ∷ Maybe TH.ModName) (stmtList ∷ [TH.Stmt]) →
    TH.MDoE modNameMaybe stmtList
  TH.CompE (stmtList ∷ [TH.Stmt]) →
    TH.CompE stmtList
  TH.ArithSeqE (range_ ∷ TH.Range) →
    TH.ArithSeqE range_
  TH.ListE (expList ∷ [TH.Exp]) →
    TH.ListE expList
  TH.SigE (exp ∷ TH.Exp) (ty ∷ TH.Type) →
    TH.SigE exp ty
  TH.RecConE (name ∷ TH.Name) (fieldExpList ∷ [TH.FieldExp]) →
    TH.RecConE name fieldExpList
  TH.RecUpdE (exp ∷ TH.Exp) (fieldExpList ∷ [TH.FieldExp]) →
    TH.RecUpdE exp fieldExpList
  TH.StaticE (exp ∷ TH.Exp) →
    TH.StaticE exp
  TH.UnboundVarE (name ∷ TH.Name) →
    TH.UnboundVarE name
  TH.LabelE (charList ∷ [ℂ]) →
    TH.LabelE charList
  TH.ImplicitParamVarE (charList ∷ [ℂ]) →
    TH.ImplicitParamVarE charList
  TH.GetFieldE (exp ∷ TH.Exp) (charList ∷ [ℂ]) →
    TH.GetFieldE exp charList
  TH.ProjectionE (stringNonEmpty ∷ HS.NonEmpty [ℂ]) →
    TH.ProjectionE stringNonEmpty
  TH.TypedBracketE (exp ∷ TH.Exp) →
    TH.TypedBracketE exp
  TH.TypedSpliceE (exp ∷ TH.Exp) →
    TH.TypedSpliceE exp

thIdCon ∷ TH.Con → TH.Con
thIdCon = \case
  TH.NormalC (name ∷ TH.Name) (bangTypeList ∷ [TH.BangType]) → 
    TH.NormalC name bangTypeList
  TH.RecC (name ∷ TH.Name) (varBangTypeList ∷ [TH.VarBangType]) →
    TH.RecC name varBangTypeList
  TH.InfixC (bangType₁ ∷ TH.BangType) (name ∷ TH.Name) (bangType₂ ∷ TH.BangType) → 
    TH.InfixC bangType₁ name bangType₂
  TH.ForallC (tyVarBndrList ∷ [TH.TyVarBndr TH.Specificity]) (cxt ∷ TH.Cxt) (con ∷ TH.Con) →
    TH.ForallC tyVarBndrList cxt con
  TH.GadtC (nameList ∷ [TH.Name]) (bangTypeList ∷ [TH.BangType]) (ty ∷ TH.Type) →
    TH.GadtC nameList bangTypeList ty
  TH.RecGadtC (nameList ∷ [TH.Name]) (varBangTypeList ∷ [TH.VarBangType]) (ty ∷ TH.Type) →
    TH.RecGadtC nameList varBangTypeList ty

thIdDerivClause ∷ TH.DerivClause → TH.DerivClause
thIdDerivClause = \case
  TH.DerivClause (derivStrategyMaybe ∷ Maybe TH.DerivStrategy) (cxt ∷ TH.Cxt) →
    TH.DerivClause derivStrategyMaybe cxt

thIdFunDep ∷ TH.FunDep → TH.FunDep
thIdFunDep = \case
  TH.FunDep (names₁ ∷ [TH.Name]) (names₂ ∷ [TH.Name]) →
    TH.FunDep names₁ names₂

thIdForeign ∷ TH.Foreign → TH.Foreign
thIdForeign = \case
  TH.ImportF (callconv ∷ TH.Callconv) (safety ∷ TH.Safety) (charList ∷ [ℂ]) (name ∷ TH.Name) (ty ∷ TH.Type) →
    TH.ImportF callconv safety charList name ty
  TH.ExportF (callconv ∷ TH.Callconv) (charList ∷ [ℂ]) (name ∷ TH.Name) (ty ∷ TH.Type) →
    TH.ExportF callconv charList name ty

thIdRuleBndr ∷ TH.RuleBndr → TH.RuleBndr
thIdRuleBndr = \case
  TH.RuleVar (name ∷ TH.Name) →
    TH.RuleVar name
  TH.TypedRuleVar (name ∷ TH.Name) (ty ∷ TH.Type) →
    TH.TypedRuleVar name ty

thIdAnnTarget ∷ TH.AnnTarget → TH.AnnTarget
thIdAnnTarget = \case
  TH.ModuleAnnotation → 
    TH.ModuleAnnotation
  TH.TypeAnnotation (name ∷ TH.Name) →
   TH.TypeAnnotation name
  TH.ValueAnnotation (name ∷ TH.Name) →
   TH.ValueAnnotation name

thIdPragma ∷ TH.Pragma → TH.Pragma
thIdPragma = \case
  TH.InlineP (name ∷ TH.Name) (inline ∷ TH.Inline) (ruleMatch ∷ TH.RuleMatch) (phases ∷ TH.Phases) →
    TH.InlineP name inline ruleMatch phases
  TH.OpaqueP (name ∷ TH.Name) →
    TH.OpaqueP name
  TH.SpecialiseP (name ∷ TH.Name) (ty ∷ TH.Type) (inlineMaybe ∷ Maybe TH.Inline) (phases ∷ TH.Phases) →
    TH.SpecialiseP name ty inlineMaybe phases
  TH.SpecialiseInstP (ty ∷ TH.Type) →
    TH.SpecialiseInstP ty
  TH.RuleP 
    (charList ∷ [ℂ]) 
    (tyVarBndrListMaybe ∷ Maybe [TH.TyVarBndr ()]) 
    (ruleBndrList ∷ [TH.RuleBndr]) 
    (exp₁ ∷ TH.Exp) 
    (exp₂ ∷ TH.Exp)
    (phases ∷ TH.Phases) →
      TH.RuleP charList tyVarBndrListMaybe ruleBndrList exp₁ exp₂ phases
  TH.AnnP (annTarget ∷ TH.AnnTarget) (exp ∷ TH.Exp) →
    TH.AnnP annTarget exp
  TH.LineP (i ∷ HS.Int) (charList ∷ [ℂ]) →
    TH.LineP i charList
  TH.CompleteP (nameList ∷ [TH.Name]) (nameMaybe ∷ Maybe TH.Name) →
    TH.CompleteP nameList nameMaybe

thIdTySynEqn ∷ TH.TySynEqn → TH.TySynEqn
thIdTySynEqn = \case
  TH.TySynEqn (tyVarBndrListMaybe ∷ Maybe [TH.TyVarBndr ()]) (ty₁ ∷ TH.Type) (ty₂ ∷ TH.Type) →
    TH.TySynEqn tyVarBndrListMaybe ty₁ ty₂

thIdFamilyResultSig ∷ TH.FamilyResultSig → TH.FamilyResultSig
thIdFamilyResultSig = \case
  TH.NoSig →
    TH.NoSig
  TH.KindSig (kind ∷ TH.Kind) →
    TH.KindSig kind
  TH.TyVarSig (tyVarBndr ∷ TH.TyVarBndr ()) →
    TH.TyVarSig tyVarBndr

thIdInjectivityAnn ∷ TH.InjectivityAnn → TH.InjectivityAnn
thIdInjectivityAnn = \case
  TH.InjectivityAnn (name ∷ TH.Name) (nameList ∷ [TH.Name]) →
    TH.InjectivityAnn name nameList

thIdTypeFamilyHead ∷ TH.TypeFamilyHead → TH.TypeFamilyHead
thIdTypeFamilyHead = \case
  TH.TypeFamilyHead 
    (name ∷ TH.Name) 
    (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis]) 
    (familyResultSig ∷ TH.FamilyResultSig)
    (injectivityAnnMaybe ∷ Maybe TH.InjectivityAnn) →
      TH.TypeFamilyHead name tyVarBndrList familyResultSig injectivityAnnMaybe

thIdDerivStrategy ∷ TH.DerivStrategy → TH.DerivStrategy
thIdDerivStrategy = \case
  TH.StockStrategy → 
    TH.StockStrategy
  TH.AnyclassStrategy → 
    TH.AnyclassStrategy
  TH.NewtypeStrategy → 
    TH.NewtypeStrategy
  TH.ViaStrategy (ty ∷ TH.Type) → 
    TH.ViaStrategy ty

thIdPatSynArgs ∷ TH.PatSynArgs → TH.PatSynArgs
thIdPatSynArgs = \case
  TH.PrefixPatSyn (nameList ∷ [TH.Name]) →
    TH.PrefixPatSyn nameList
  TH.InfixPatSyn (name₁ ∷ TH.Name) (name₂ ∷ TH.Name) →
    TH.InfixPatSyn name₁ name₂
  TH.RecordPatSyn (nameList ∷ [TH.Name]) →
    TH.RecordPatSyn nameList

thIdPatSynDir ∷ TH.PatSynDir → TH.PatSynDir
thIdPatSynDir = \case
  TH.Unidir →
    TH.Unidir
  TH.ImplBidir →
    TH.ImplBidir
  TH.ExplBidir (clauseList ∷ [TH.Clause]) →
    TH.ExplBidir clauseList

thIdDec ∷ TH.Dec → TH.Dec
thIdDec = \case
  TH.FunD 
    (name ∷ TH.Name) 
    (clauseList ∷ [TH.Clause]) →
      TH.FunD name clauseList
  TH.ValD 
    (pat ∷ TH.Pat) 
    (body ∷ TH.Body) 
    (decList ∷ [TH.Dec]) →
      TH.ValD pat body decList
  TH.DataD 
    (cxt ∷ TH.Cxt) 
    (name ∷ TH.Name) 
    (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis]) 
    (kindMaybe ∷ Maybe TH.Kind) 
    (conList ∷ [TH.Con]) 
    (derivClauseList ∷ [TH.DerivClause]) →
      TH.DataD cxt name tyVarBndrList kindMaybe conList derivClauseList
  TH.NewtypeD
    (cxt ∷ TH.Cxt) 
    (name ∷ TH.Name) 
    (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis]) 
    (kindMaybe ∷ Maybe TH.Kind) 
    (con ∷ TH.Con) 
    (derivClauseList ∷ [TH.DerivClause]) →
      TH.NewtypeD cxt name tyVarBndrList kindMaybe con derivClauseList
  TH.TypeDataD 
    (name ∷ TH.Name) 
    (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis]) 
    (kindMaybe ∷ Maybe TH.Kind) 
    (conList ∷ [TH.Con]) →
      TH.TypeDataD name tyVarBndrList kindMaybe conList
  TH.TySynD 
    (name ∷ TH.Name)
    (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis])
    (ty ∷ TH.Type) →
      TH.TySynD name tyVarBndrList ty
  TH.ClassD 
    (cxt ∷ TH.Cxt)
    (name ∷ TH.Name)
    (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis])
    (funDepList ∷ [TH.FunDep])
    (decList ∷ [TH.Dec]) →
      TH.ClassD cxt name tyVarBndrList funDepList decList
  TH.InstanceD 
    (overlapMaybe ∷ Maybe TH.Overlap)
    (cxt ∷ TH.Cxt)
    (ty ∷ TH.Type)
    (decList ∷ [TH.Dec]) →
      TH.InstanceD overlapMaybe cxt ty decList
  TH.SigD
    (name ∷ TH.Name)
    (ty ∷ TH.Type) →
      TH.SigD name ty
  TH.KiSigD
    (name ∷ TH.Name)
    (kind ∷ TH.Kind) →
      TH.KiSigD name kind
  TH.ForeignD
    (foreign_ ∷ TH.Foreign) →
      TH.ForeignD foreign_
  TH.InfixD
    (fixity ∷ TH.Fixity)
    (name ∷ TH.Name) →
      TH.InfixD fixity name
  TH.DefaultD
    (tyList ∷ [TH.Type]) →
      TH.DefaultD tyList
  TH.PragmaD 
    (pragma ∷ TH.Pragma) →
      TH.PragmaD pragma
  TH.DataFamilyD
    (name ∷ TH.Name)
    (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis])
    (kindMaybe ∷ Maybe TH.Kind) →
      TH.DataFamilyD name tyVarBndrList kindMaybe
  TH.DataInstD
    (cxt ∷ TH.Cxt)
    (tyVarBndrListMaybe ∷ Maybe [TH.TyVarBndr ()])
    (ty ∷ TH.Type)
    (kindMaybe ∷ Maybe TH.Kind)
    (conList ∷ [TH.Con]) 
    (derivClauseList ∷ [TH.DerivClause]) →
      TH.DataInstD cxt tyVarBndrListMaybe ty kindMaybe conList derivClauseList
  TH.NewtypeInstD
    (cxt ∷ TH.Cxt)
    (tyVarBndrListMaybe ∷ Maybe [TH.TyVarBndr ()])
    (ty ∷ TH.Type)
    (kindMaybe ∷ Maybe TH.Kind)
    (con ∷ TH.Con) 
    (derivClauseList ∷ [TH.DerivClause]) →
      TH.NewtypeInstD cxt tyVarBndrListMaybe ty kindMaybe con derivClauseList
  TH.TySynInstD
    (tySynEqn ∷ TH.TySynEqn) →
      TH.TySynInstD tySynEqn
  TH.OpenTypeFamilyD
    (typeFamilyHead ∷ TH.TypeFamilyHead) →
      TH.OpenTypeFamilyD typeFamilyHead
  TH.ClosedTypeFamilyD
    (typeFamilyHead ∷ TH.TypeFamilyHead) 
    (tySynEqnList ∷ [TH.TySynEqn]) →
      TH.ClosedTypeFamilyD typeFamilyHead tySynEqnList
  TH.RoleAnnotD
    (name ∷ TH.Name)
    (roleList ∷ [TH.Role]) →
      TH.RoleAnnotD name roleList
  TH.StandaloneDerivD
    (derivStrategyMaybe ∷ Maybe TH.DerivStrategy)
    (cxt ∷ TH.Cxt)
    (ty ∷ TH.Type) →
      TH.StandaloneDerivD derivStrategyMaybe cxt ty
  TH.DefaultSigD
    (name ∷ TH.Name)
    (ty ∷ TH.Type) →
      TH.DefaultSigD name ty
  TH.PatSynD
    (name ∷ TH.Name)
    (patSynArgs ∷ TH.PatSynArgs)
    (patSynDir ∷ TH.PatSynDir)
    (pat ∷ TH.Pat) →
      TH.PatSynD name patSynArgs patSynDir pat
  TH.PatSynSigD
    (name ∷ TH.Name)
    (patSynType ∷ TH.PatSynType) →
      TH.PatSynSigD  name patSynType
  TH.ImplicitParamBindD
    (charList ∷ [ℂ])
    (exp ∷ TH.Exp) →
      TH.ImplicitParamBindD charList exp

----------------
-- ADTConInfo --
----------------

data ADTConInfo = ADTConInfo
  { adtConInfoName     ∷ TH.Name
  , adtConInfoArgTypes ∷ [TH.Type]
  } deriving (Eq,Ord,Show)

adtConInfo ∷ 𝕊 → TH.Con → TH.Q ADTConInfo
adtConInfo fname c = case c of
  TH.NormalC 
    (name ∷ TH.Name) 
    (bangTypes ∷ [TH.BangType]) → 
      return $ ADTConInfo name $ mapOn bangTypes $ \ (_bang ∷ TH.Bang,ty ∷ TH.Type) → ty
  TH.RecC
    (name ∷ TH.Name)
    (varBangTypes ∷ [TH.VarBangType]) →
      return $ ADTConInfo name $ mapOn varBangTypes $ \ (_nameField ∷ TH.Name,_bang ∷ TH.Bang,ty ∷ TH.Type) → ty
  TH.InfixC
    (bangType₁ ∷ TH.BangType)
    (name ∷ TH.Name)
    (bangType₂ ∷ TH.BangType) →
      return $ ADTConInfo name $ mapOn [bangType₁,bangType₂] $ \ (_bang ∷ TH.Bang,ty ∷ TH.Type) → ty
  _ → fail𝕊 err_adtConInfo_NOT_SIMPLE_ADT
  where
    err_adtConInfo_NOT_SIMPLE_ADT ∷ 𝕊
    err_adtConInfo_NOT_SIMPLE_ADT = concat $ inbetween " "
      [ "<"
      , fname
      , ">"
      , "[["
      , string $ TH.pprint c
      , "]]"
      , "wanted a simple ADT constructor,"
      , "but this is either"
      , "a forall quantified constructor `∀ a. Cxt a ⇒ C a`"
      , "or a GADT constructor `C ∷ a → T b`,"
      , "neither of which are supported by UVMHS Template Haskell helpers"
      , "that operate over simple ADTs."
      ]

-------------
-- ADTInfo --
-------------

data ADTInfo = ADTInfo
  { adtInfoCxt      ∷ TH.Cxt
  , adtInfoName     ∷ TH.Name
  , adtInfoTypeArgs ∷ [TH.TyVarBndr TH.BndrVis]
  , adtInfoCons     ∷ [ADTConInfo]
  } deriving (Eq,Ord,Show)

adtInfo ∷ 𝕊 → TH.Name → TH.Q ADTInfo
adtInfo fname nameADT = do
  𝒾 ∷ TH.Info
    ← TH.reify nameADT
  dec ∷ TH.Dec
      ← case 𝒾 of
          TH.TyConI (dec ∷ TH.Dec) → return dec
          _ → fail𝕊 err_adtInfo_NOT_USER_DEFINED_TYPE
  case dec of
    TH.DataD 
      (cxt ∷ TH.Cxt) 
      (nameCon ∷ TH.Name)
      (tyVarBndrs ∷ [TH.TyVarBndr TH.BndrVis])
      (kindO ∷ Maybe (TH.Kind))
      (cons ∷ [TH.Con])
      (_derivClauses ∷ [TH.DerivClause]) → do
        when (not $ kindO ≡ Nothing ⩔ kindO ≡ Just TH.StarT) $ \ () → 
          fail𝕊 $ err_adtInfo_BAD_KIND kindO
        ADTInfo cxt nameCon tyVarBndrs ^$ mapM (adtConInfo fname) cons
    TH.NewtypeD 
      (cxt ∷ TH.Cxt) 
      (nameCon ∷ TH.Name) 
      (tyVarBndrs ∷ [TH.TyVarBndr TH.BndrVis])
      (kindO ∷ Maybe (TH.Kind))
      (con ∷ TH.Con)
      (_derivClauses ∷ [TH.DerivClause]) → do
        when (not $ kindO ≡ Nothing ⩔ kindO ≡ Just TH.StarT) $ \ () → 
          fail𝕊 $ err_adtInfo_BAD_KIND kindO
        ADTInfo cxt nameCon tyVarBndrs ∘ single ^$ adtConInfo fname con
    _ → fail𝕊 err_adtInfo_NOT_DATA_NEWTYPE
  where
    err_adtInfo_NOT_USER_DEFINED_TYPE ∷ 𝕊
    err_adtInfo_NOT_USER_DEFINED_TYPE = concat $ inbetween " "
      [ "<"
      , fname
      , ">"
      , "[["
      , string $ TH.pprint nameADT
      , "]]"
      , "not a user-defined type."
      ] 
    err_adtInfo_BAD_KIND ∷ Maybe TH.Kind → 𝕊
    err_adtInfo_BAD_KIND kindO = concat $ inbetween " "
      [ "<"
      , fname
      , ">"
      , "[["
      , string $ TH.pprint nameADT
      , "∷"
      , case kindO of Nothing → "<no-kind-annotation>" ; Just kind → string $ TH.pprint kind
      , "]]"
      , "datatype declaration with kind annotations"
      , "at kind other than ★"
      , "are not supported."
      ]
    err_adtInfo_NOT_DATA_NEWTYPE ∷ 𝕊
    err_adtInfo_NOT_DATA_NEWTYPE = concat $ inbetween " "
      [ "<"
      , fname
      , ">"
      , "[["
      , string $ TH.pprint nameADT
      , "]]"
      , "wanted a datatype or newtype declaration,"
      , "but this is neither."
      ]

adtInfoTypeArgsNameMap ∷ ADTInfo → 𝕊 ⇰ TH.Name
adtInfoTypeArgsNameMap 𝒾 =
  dict $ mapOn (adtInfoTypeArgs 𝒾) $ \ (tyVarBndr ∷ TH.TyVarBndr TH.BndrVis) → 
    let name = thTyVarBndrName tyVarBndr in 
    thNameString name ↦ name

adtInfoAllConArgs ∷ ADTInfo → [TH.Type]
adtInfoAllConArgs = 
  lazyList 
  ∘ uniques𝑃 
  ∘ concat 
  ∘ map adtConInfoArgTypes
  ∘ adtInfoCons

adtInfoAllConArgsQ ∷ ADTInfo → [TH.TypeQ]
adtInfoAllConArgsQ = map return ∘ adtInfoAllConArgs

adtInfoTypeArgVars ∷ ADTInfo → [TH.Type]
adtInfoTypeArgVars = map (TH.VarT ∘ thTyVarBndrName) ∘ adtInfoTypeArgs

adtInfoTypeArgVarsQ ∷ ADTInfo → [TH.TypeQ]
adtInfoTypeArgVarsQ = map return ∘ adtInfoTypeArgVars

adtInfoFullType ∷ ADTInfo → TH.Type
adtInfoFullType 𝒾 = apply TH.AppT (TH.ConT (adtInfoName 𝒾)) $ adtInfoTypeArgVars 𝒾

adtInfoFullTypeQ ∷ ADTInfo → TH.TypeQ
adtInfoFullTypeQ = return ∘ adtInfoFullType

adtInfoCasesQ ∷ ADTInfo → (TH.ExpQ → [TH.ExpQ] → TH.ExpQ) → TH.ExpQ
adtInfoCasesQ 𝒾 f = TH.LamCaseE ^$ mapMOn (adtInfoCons 𝒾) $ \ 𝒾C → do
  xs ← mapMOn (adtConInfoArgTypes 𝒾C) $ const $ thGensym
  let pat ∷ TH.Pat
      pat = TH.ConP (adtConInfoName 𝒾C) [] $ map TH.VarP xs
  body ∷ TH.Body
       ← TH.NormalB ^$ f (return $ TH.VarE $ adtConInfoName 𝒾C) $ map (return ∘ TH.VarE) xs
  return $ TH.Match pat body []

adtInfoConssQ ∷ ADTInfo → (TH.ExpQ → [TH.TypeQ] → TH.ExpQ) → [TH.ExpQ]
adtInfoConssQ 𝒾 f = mapOn (adtInfoCons 𝒾) $ \ 𝒾C → f (TH.varE $ adtConInfoName 𝒾C) $ map return $ adtConInfoArgTypes 𝒾C

-----------------
-- ADTProdInfo --
-----------------

data ADTProdInfo = ADTProdInfo
  { adtProdInfoCxt      ∷ TH.Cxt
  , adtProdInfoName     ∷ TH.Name
  , adtProdInfoTypeArgs ∷ [TH.TyVarBndr TH.BndrVis]
  , adtProdInfoCon      ∷ ADTConInfo
  } deriving (Eq,Ord,Show)

adtProdInfo ∷ 𝕊 → TH.Name → TH.Q ADTProdInfo
adtProdInfo fname name = do
  ADTInfo cxt nameCon typeArgs cons ← adtInfo fname name
  con ← case view singleL $ list cons of
    None → fail𝕊 err_adtProdInfo_NOT_SINGLE_CONSTRUCTOR
    Some con → return con
  return $ ADTProdInfo cxt nameCon typeArgs con
  where
    err_adtProdInfo_NOT_SINGLE_CONSTRUCTOR ∷ 𝕊
    err_adtProdInfo_NOT_SINGLE_CONSTRUCTOR = concat $ inbetween " "
      [ "<"
      , fname
      , ">"
      , "[["
      , string $ TH.pprint name
      , "]]"
      , "not a datatype with a single constructor"
      ]

adtProdInfoAllConArgs ∷ ADTProdInfo → [TH.Type]
adtProdInfoAllConArgs = 
  lazyList 
  ∘ uniques𝑃 
  ∘ adtConInfoArgTypes
  ∘ adtProdInfoCon

adtProdInfoAllConArgsQ ∷ ADTProdInfo → [TH.TypeQ]
adtProdInfoAllConArgsQ = map return ∘ adtProdInfoAllConArgs

adtProdInfoTypeArgVars ∷ ADTProdInfo → [TH.Type]
adtProdInfoTypeArgVars = map (TH.VarT ∘ thTyVarBndrName) ∘ adtProdInfoTypeArgs

adtProdInfoTypeArgVarsQ ∷ ADTProdInfo → [TH.TypeQ]
adtProdInfoTypeArgVarsQ = map return ∘ adtProdInfoTypeArgVars

adtProdInfoFullType ∷ ADTProdInfo → TH.Type
adtProdInfoFullType 𝒾 = apply TH.AppT (TH.ConT (adtProdInfoName 𝒾)) $ adtProdInfoTypeArgVars 𝒾

adtProdInfoFullTypeQ ∷ ADTProdInfo → TH.TypeQ
adtProdInfoFullTypeQ = return ∘ adtProdInfoFullType

adtProdInfoLetQ ∷ ADTProdInfo → TH.ExpQ → ([TH.ExpQ] → TH.ExpQ) → TH.ExpQ
adtProdInfoLetQ 𝒾 e f = do
  let 𝒾C = adtProdInfoCon 𝒾
  xs ← mapMOn (adtConInfoArgTypes 𝒾C) $ const $ thGensym
  [| let $(TH.conP (adtConInfoName 𝒾C) $ map TH.varP xs) = $e in 
     $(f $ map TH.varE xs) 
   |]

adtProdInfoConsQ ∷ ADTProdInfo → (TH.ExpQ → [TH.TypeQ] → TH.ExpQ) → TH.ExpQ
adtProdInfoConsQ 𝒾 f = 
  let 𝒾C = adtProdInfoCon 𝒾 in
  f (TH.varE $ adtConInfoName 𝒾C) $ map return $ adtConInfoArgTypes 𝒾C

----------------------
-- ADTRecordConInfo --
----------------------

data ADTRecordConInfo = ADTRecordConInfo
  { adtRecordConInfoName ∷ TH.Name
  , adtRecordConInfoArgTypes ∷ [(TH.Name,TH.Type)]
  } deriving (Eq,Ord,Show)

-------------------
-- ADTRecordInfo --
-------------------

data ADTRecordInfo = ADTRecordInfo
  { adtRecordInfoCxt ∷ TH.Cxt
  , adtRecordInfoName ∷ TH.Name
  , adtRecordInfoTypeArgs ∷ [TH.TyVarBndr TH.BndrVis]
  , adtRecordInfoCon ∷ ADTRecordConInfo
  } deriving (Eq,Ord,Show)


-------------------
-- DERIVING LENS --
-------------------

-- createLensDefs ∷ TH.Name → TH.DecsQ
-- createLensDefs name = do
--   𝒾 ← adtRecordInfo name
--   [d| instance
--         $(thTupsTQ $ mapOn (adtProdInfoAllConArgsQ 𝒾) $ \ τ → [t| Null $τ |])
--         ⇒
--         Null $(adtProdInfoFullTypeQ 𝒾)
--         where
--           null = $(adtProdInfoConsQ 𝒾 $ \ mk τs →
--             foldOnFrom τs mk $ \ τ eQ → [| $eQ (null @($τ)) |])
--       instance
--         $(thTupsTQ $ mapOn (adtProdInfoAllConArgsQ 𝒾) $ \ τ → [t| Append $τ |])
--         ⇒
--         Append $(adtProdInfoFullTypeQ 𝒾)
--         where
--           x ⧺ y = $(adtProdInfoLetQ 𝒾 [| x |] $ \ xs → adtProdInfoLetQ 𝒾 [| y |] $ \ ys →
--             adtProdInfoConsQ 𝒾 $ \ mk _τs →
--               foldOnFrom (zip xs ys) mk $ uncurry $ \ xᵢ yᵢ eQ → [| $eQ ($xᵢ ⧺ $yᵢ) |])
--       instance
--         $(thTupsTQ $ mapOn (adtProdInfoAllConArgsQ 𝒾) $ \ τ → [t| Monoid $τ |])
--         ⇒
--         Monoid $(adtProdInfoFullTypeQ 𝒾)
--    |]

---------------------
-- DERIVING MONOID --
---------------------

createMonoidInstance ∷ TH.Name → TH.DecsQ
createMonoidInstance name = do
  𝒾 ← adtProdInfo "createMonoidInstance" name
  [d| instance
        $(thTupsTQ $ mapOn (adtProdInfoAllConArgsQ 𝒾) $ \ τ → [t| Null $τ |])
        ⇒
        Null $(adtProdInfoFullTypeQ 𝒾)
        where
          null = $(adtProdInfoConsQ 𝒾 $ \ mk τs →
            foldOnFrom τs mk $ \ τ eQ → [| $eQ (null @($τ)) |])
      instance
        $(thTupsTQ $ mapOn (adtProdInfoAllConArgsQ 𝒾) $ \ τ → [t| Append $τ |])
        ⇒
        Append $(adtProdInfoFullTypeQ 𝒾)
        where
          x ⧺ y = $(adtProdInfoLetQ 𝒾 [| x |] $ \ xs → adtProdInfoLetQ 𝒾 [| y |] $ \ ys →
            adtProdInfoConsQ 𝒾 $ \ mk _τs →
              foldOnFrom (zip xs ys) mk $ uncurry $ \ xᵢ yᵢ eQ → [| $eQ ($xᵢ ⧺ $yᵢ) |])
      instance
        $(thTupsTQ $ mapOn (adtProdInfoAllConArgsQ 𝒾) $ \ τ → [t| Monoid $τ |])
        ⇒
        Monoid $(adtProdInfoFullTypeQ 𝒾)
   |]

--------------------
-- DERIVING FUZZY --
--------------------

createFuzzyInstance ∷ [𝕊] → TH.Name → TH.DecsQ
createFuzzyInstance recNamesS name = do
  when (count @ℕ64 (pow𝑃 recNamesS) ≢ count recNamesS) $ \ () →
    fail $ err_MSG_DUPLICATE_REC_NAMES ()
  𝒾 ← adtInfo "createFuzzyInstance" name
  let nameMap ∷ 𝕊 ⇰ TH.Name
      nameMap = adtInfoTypeArgsNameMap 𝒾
  recNames ∷ 𝑃 TH.Name
            ← pow ^$ mapMOn recNamesS $ \ nameᵢ → do
    case nameMap ⋕? nameᵢ of
      None → fail $ err_MSG_INVALID_REC_NAME nameMap
      Some nameᵣ → return nameᵣ
  let recNames' ∷ 𝑃 TH.Name
      recNames' = recNames ∪ single name
  [d| instance 
        $(thTupsTQ $ mapOn (adtInfoAllConArgsQ 𝒾) $ \ τ → [t| Fuzzy $τ |])
        ⇒
        Fuzzy $(adtInfoFullTypeQ 𝒾) 
        where
          fuzzy = do
            d ← fuzzyDepth
            wrchoose 
              $(TH.listE $ adtInfoConssQ 𝒾 $ \ mk τs → do
                  (con :* (anyRec :* nonRec :* stmts)) 
                    ∷ TH.ExpQ ∧ (𝔹 ∧ ℕ64 ∧ 𝐼 TH.StmtQ)
                    ← evalRWST () mk $ retStateOut $ eachOn τs $ \ τ → UVMHS.Core.do
                      x' ← lift thGensym
                      modify $ \ eQ → [| $eQ $(TH.varE x') |]
                      isRec ∷ 𝔹
                            ← lift $ thAnyNameOccursInType recNames' ^$ τ
                      tellL (fstL ⊚ fstL) isRec
                      when (not isRec) $ \ () →
                        tellL (sndL ⊚ fstL) one
                      tellL sndL $ single $ TH.bindS (TH.varP x') $ if not isRec then [| fuzzy @($τ) |] else [| fuzzyRec @($τ) |]
                  when (anyRec ⩓ nonRec ≡ zero) $ \ () →
                    fail $ "not ok to have only recursive fields in constructor: " ⧺ show anyRec ⧺ " " ⧺ show nonRec
                  let weight = if anyRec then [| d |] else [| one |]
                  [| \ () → $(weight) :* $(TH.doE $ lazyList $ concat [stmts,single $ TH.noBindS [| return $con |]]) |])
   |]
  where
    err_MSG_DUPLICATE_REC_NAMES ∷ () → [ℂ]
    err_MSG_DUPLICATE_REC_NAMES () = concat $ inbetween " "
      [ "createFuzzyInstance:"
      , "[["
      , TH.pprint name
      , ","
      , lazyList $ ppRenderNoFmt $ pretty recNamesS
      , "]]"
      , "the list of provided names"
      , "should not include any duplicates"
      , "(purely as a sanity check)."
      ]
    err_MSG_INVALID_REC_NAME ∷ 𝕊 ⇰ TH.Name → [ℂ]
    err_MSG_INVALID_REC_NAME nameMap = concat $ inbetween " " $
      [ "createFuzzyInstance:"
      , "[["
      , TH.pprint name
      , ","
      , lazyList $ ppRenderNoFmt $ pretty recNamesS
      , "]]"
      , "each provided name must must match"
      , "the name of a type variable argument"
      , "for the declared data type."
      , "valid options are:"
      , show $ dkeys nameMap
      ]

data OtherList a b =
    NilA a b
  | NilB b
  | ConsB b (OtherList a b)
