module UVMHS.Future.TH.StripModuleNames where

import UVMHS.Core

import qualified Prelude as HS
import qualified Data.List.NonEmpty as HS

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

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
  TH.ForallT (tyVarBndrs ∷ [TH.TyVarBndr TH.Specificity]) (cxt ∷ TH.Cxt) (type_ ∷ TH.Type) → 
    TH.ForallT (map thStripModuleNamesTyVarBndr tyVarBndrs) (map thStripModuleNamesType cxt) $ thStripModuleNamesType type_
  TH.ForallVisT (tyVarBndrs ∷ [TH.TyVarBndr ()]) (type_ ∷ TH.Type) → 
    TH.ForallVisT (map thStripModuleNamesTyVarBndr tyVarBndrs) $ thStripModuleNamesType type_
  TH.AppT (type_₁ ∷ TH.Type) (type_₂ ∷ TH.Type) → TH.AppT (thStripModuleNamesType type_₁) $ thStripModuleNamesType type_₂
  TH.AppKindT (type_ ∷ TH.Type) (kind ∷ TH.Kind) → TH.AppKindT (thStripModuleNamesType type_) $ thStripModuleNamesType kind
  TH.SigT (type_ ∷ TH.Type) (kind ∷ TH.Kind) → TH.SigT (thStripModuleNamesType type_) $ thStripModuleNamesType kind
  TH.VarT (name ∷ TH.Name) → TH.VarT $ thStripModuleNamesName name
  TH.ConT (name ∷ TH.Name) → TH.ConT $ thStripModuleNamesName name
  TH.PromotedT (name ∷ TH.Name) → TH.PromotedT $ thStripModuleNamesName name
  TH.InfixT (type_₁ ∷ TH.Type) (name ∷ TH.Name) (type_₂ ∷ TH.Type) → 
    TH.InfixT (thStripModuleNamesType type_₁) (thStripModuleNamesName name) $ thStripModuleNamesType type_₂
  TH.UInfixT (type_₁ ∷ TH.Type) (name ∷ TH.Name) (type_₂ ∷ TH.Type) → 
    TH.UInfixT (thStripModuleNamesType type_₁) (thStripModuleNamesName name) $ thStripModuleNamesType type_₂
  TH.PromotedInfixT (type_₁ ∷ TH.Type) (name ∷ TH.Name) (type_₂ ∷ TH.Type) →
    TH.PromotedInfixT (thStripModuleNamesType type_₁) (thStripModuleNamesName name) $ thStripModuleNamesType type_₂
  TH.PromotedUInfixT (type_₁ ∷ TH.Type) (name ∷ TH.Name) (type_₂ ∷ TH.Type) →
    TH.PromotedUInfixT (thStripModuleNamesType type_₁) (thStripModuleNamesName name) $ thStripModuleNamesType type_₂
  TH.ParensT (type_ ∷ TH.Type) → TH.ParensT $ thStripModuleNamesType type_
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
  TH.ImplicitParamT (s ∷ HS.String) (type_ ∷ TH.Type) → TH.ImplicitParamT s $ thStripModuleNamesType type_

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
  TH.SigP (pat ∷ TH.Pat) (type_ ∷ TH.Type) → TH.SigP (thStripModuleNamesPat pat) $ thStripModuleNamesType type_
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
  TH.AppTypeE (exp ∷ TH.Exp) (type_ ∷ TH.Type) → TH.AppTypeE (thStripModuleNamesExp exp) $ thStripModuleNamesType type_
  TH.InfixE (expMaybe₁ ∷ HS.Maybe TH.Exp) (exp ∷ TH.Exp) (expMaybe₂ ∷ HS.Maybe TH.Exp) →
    TH.InfixE (HS.fmap thStripModuleNamesExp expMaybe₁) (thStripModuleNamesExp exp) $ HS.fmap thStripModuleNamesExp expMaybe₂
  TH.UInfixE (exp₁ ∷ TH.Exp) (exp₂ ∷ TH.Exp) (exp₃ ∷ TH.Exp) →
    TH.UInfixE (thStripModuleNamesExp exp₁) (thStripModuleNamesExp exp₂) $ thStripModuleNamesExp exp₃
  TH.ParensE (exp ∷ TH.Exp) → TH.ParensE $ thStripModuleNamesExp exp
  TH.LamE (patList ∷ [TH.Pat]) (exp ∷ TH.Exp) → TH.LamE (map thStripModuleNamesPat patList) $ thStripModuleNamesExp exp
  TH.LamCaseE (matchList ∷ [TH.Match]) → TH.LamCaseE $ map thStripModuleNamesMatch matchList
  TH.LamCasesE (clauseList ∷ [TH.Clause]) → TH.LamCasesE $ map thStripModuleNamesClause clauseList
  TH.TupE (expMaybeList ∷ [HS.Maybe TH.Exp]) → TH.TupE $ map (HS.fmap thStripModuleNamesExp) expMaybeList
  TH.UnboxedTupE (expMaybeList ∷ [HS.Maybe TH.Exp]) → TH.UnboxedTupE $ map (HS.fmap thStripModuleNamesExp) expMaybeList
  TH.UnboxedSumE (exp ∷ TH.Exp) (sumAlt ∷ TH.SumAlt) (sumArity ∷ TH.SumArity) → 
    TH.UnboxedSumE (thStripModuleNamesExp exp) sumAlt sumArity
  TH.CondE (exp₁ ∷ TH.Exp) (exp₂ ∷ TH.Exp) (exp₃ ∷ TH.Exp) →
    TH.CondE (thStripModuleNamesExp exp₁) (thStripModuleNamesExp exp₂) $ thStripModuleNamesExp exp₃
  TH.MultiIfE (guardExpList ∷ [(TH.Guard,TH.Exp)]) →
    TH.MultiIfE $ mapOn guardExpList $ \ (grd ∷ TH.Guard,exp ∷ TH.Exp) → (thStripModuleNamesGuard grd,thStripModuleNamesExp exp)
  TH.LetE (decList ∷ [TH.Dec]) (exp ∷ TH.Exp) → TH.LetE (map thStripModuleNamesDec decList) $ thStripModuleNamesExp exp
  TH.CaseE (exp ∷ TH.Exp) (matchList ∷ [TH.Match]) → TH.CaseE (thStripModuleNamesExp exp) $ map thStripModuleNamesMatch matchList
  TH.DoE (modNameMaybe ∷ HS.Maybe TH.ModName) (stmtList ∷ [TH.Stmt]) → TH.DoE modNameMaybe $ map thStripModuleNamesStmt stmtList
  TH.MDoE (modNameMaybe ∷ HS.Maybe TH.ModName) (stmtList ∷ [TH.Stmt]) → TH.MDoE modNameMaybe $ map thStripModuleNamesStmt stmtList
  TH.CompE (stmtList ∷ [TH.Stmt]) → TH.CompE $ map thStripModuleNamesStmt stmtList
  TH.ArithSeqE (range_ ∷ TH.Range) → TH.ArithSeqE $ thStripModuleNamesRange range_
  TH.ListE (expList ∷ [TH.Exp]) → TH.ListE $ map thStripModuleNamesExp expList
  TH.SigE (exp ∷ TH.Exp) (type_ ∷ TH.Type) → TH.SigE (thStripModuleNamesExp exp) $ thStripModuleNamesType type_
  TH.RecConE (name ∷ TH.Name) (fieldExpList ∷ [TH.FieldExp]) →
    TH.RecConE (thStripModuleNamesName name) $ mapOn fieldExpList $ \ (nameᵢ ∷ TH.Name,exp ∷ TH.Exp) → 
      (thStripModuleNamesName nameᵢ,thStripModuleNamesExp exp)
  TH.RecUpdE (exp ∷ TH.Exp) (fieldExpList ∷ [TH.FieldExp]) →
    TH.RecUpdE (thStripModuleNamesExp exp) $ mapOn fieldExpList $ \ (nameᵢ ∷ TH.Name,expᵢ ∷ TH.Exp) → 
      (thStripModuleNamesName nameᵢ,thStripModuleNamesExp expᵢ)
  TH.StaticE (exp ∷ TH.Exp) → TH.StaticE $ thStripModuleNamesExp exp
  TH.UnboundVarE (name ∷ TH.Name) → TH.UnboundVarE $ thStripModuleNamesName name
  TH.LabelE (string_ ∷ [HS.Char]) → TH.LabelE string_
  TH.ImplicitParamVarE (string_ ∷ [HS.Char]) → TH.ImplicitParamVarE string_
  TH.GetFieldE (exp ∷ TH.Exp) (string_ ∷ [HS.Char]) → TH.GetFieldE (thStripModuleNamesExp exp) string_
  TH.ProjectionE (stringNonEmpty ∷ HS.NonEmpty [HS.Char]) → TH.ProjectionE stringNonEmpty
  TH.TypedBracketE (exp ∷ TH.Exp) → TH.TypedBracketE $ thStripModuleNamesExp exp
  TH.TypedSpliceE (exp ∷ TH.Exp) → TH.TypedSpliceE $ thStripModuleNamesExp exp

thStripModuleNamesCon ∷ TH.Con → TH.Con
thStripModuleNamesCon = \case
  TH.NormalC (name ∷ TH.Name) (bangTypeList ∷ [TH.BangType]) → 
    TH.NormalC (thStripModuleNamesName name) $ mapOn bangTypeList $ \ (bang ∷ TH.Bang,type_ ∷ TH.Type) → 
      (bang,thStripModuleNamesType type_)
  TH.RecC (name ∷ TH.Name) (varBangTypeList ∷ [TH.VarBangType]) →
    TH.RecC (thStripModuleNamesName name) $ mapOn varBangTypeList $ \ (nameᵢ ∷ TH.Name,bang ∷ TH.Bang,type_ ∷ TH.Type) →
      (thStripModuleNamesName nameᵢ,bang,thStripModuleNamesType type_)
  TH.InfixC (bang₁ ∷ TH.Bang,type_₁ ∷ TH.Type) (name ∷ TH.Name) (bang₂ ∷ TH.Bang,type_₂ ∷ TH.Type) → 
    TH.InfixC (bang₁,thStripModuleNamesType type_₁) (thStripModuleNamesName name) (bang₂,thStripModuleNamesType type_₂)
  TH.ForallC (tyVarBndrList ∷ [TH.TyVarBndr TH.Specificity]) (cxt ∷ TH.Cxt) (con ∷ TH.Con) →
    TH.ForallC (map thStripModuleNamesTyVarBndr tyVarBndrList) (map thStripModuleNamesType cxt) $ thStripModuleNamesCon con
  TH.GadtC (nameList ∷ [TH.Name]) (bangTypeList ∷ [TH.BangType]) (type_ ∷ TH.Type) →
    let bangTypeList' = mapOn bangTypeList $ \ (bang ∷ TH.Bang,tyᵢ ∷ TH.Type) → (bang,thStripModuleNamesType tyᵢ) in
    TH.GadtC (map thStripModuleNamesName nameList) bangTypeList' $ thStripModuleNamesType type_
  TH.RecGadtC (nameList ∷ [TH.Name]) (varBangTypeList ∷ [TH.VarBangType]) (type_ ∷ TH.Type) →
    let varBangTypeList' = mapOn varBangTypeList $ \ (name ∷ TH.Name,bang ∷ TH.Bang,tyᵢ ∷ TH.Type) → 
          (thStripModuleNamesName name,bang,thStripModuleNamesType tyᵢ) 
    in
    TH.RecGadtC (map thStripModuleNamesName nameList) varBangTypeList' $ thStripModuleNamesType type_

thStripModuleNamesDerivClause ∷ TH.DerivClause → TH.DerivClause
thStripModuleNamesDerivClause = \case
  TH.DerivClause (derivStrategyMaybe ∷ HS.Maybe TH.DerivStrategy) (cxt ∷ TH.Cxt) →
    TH.DerivClause derivStrategyMaybe $ map thStripModuleNamesType cxt

thStripModuleNamesFunDep ∷ TH.FunDep → TH.FunDep
thStripModuleNamesFunDep = \case
  TH.FunDep (names₁ ∷ [TH.Name]) (names₂ ∷ [TH.Name]) →
    TH.FunDep (map thStripModuleNamesName names₁) $ map thStripModuleNamesName names₂

thStripModuleNamesForeign ∷ TH.Foreign → TH.Foreign
thStripModuleNamesForeign = \case
  TH.ImportF (callconv ∷ TH.Callconv) (safety ∷ TH.Safety) (string_ ∷ [HS.Char]) (name ∷ TH.Name) (type_ ∷ TH.Type) →
    TH.ImportF callconv safety string_ (thStripModuleNamesName name) $ thStripModuleNamesType type_
  TH.ExportF (callconv ∷ TH.Callconv) (string_ ∷ [HS.Char]) (name ∷ TH.Name) (type_ ∷ TH.Type) →
    TH.ExportF callconv string_ (thStripModuleNamesName name) $ thStripModuleNamesType type_

thStripModuleNamesRuleBndr ∷ TH.RuleBndr → TH.RuleBndr
thStripModuleNamesRuleBndr = \case
  TH.RuleVar (name ∷ TH.Name) → TH.RuleVar $ thStripModuleNamesName name
  TH.TypedRuleVar (name ∷ TH.Name) (type_ ∷ TH.Type) → TH.TypedRuleVar (thStripModuleNamesName name) $ thStripModuleNamesType type_

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
  TH.SpecialiseP (name ∷ TH.Name) (type_ ∷ TH.Type) (inlineMaybe ∷ HS.Maybe TH.Inline) (phases ∷ TH.Phases) →
    TH.SpecialiseP (thStripModuleNamesName name) (thStripModuleNamesType type_) inlineMaybe phases
  TH.SpecialiseInstP (type_ ∷ TH.Type) → TH.SpecialiseInstP $ thStripModuleNamesType type_
  TH.RuleP 
    (string_ ∷ [HS.Char]) (tyVarBndrListMaybe ∷ HS.Maybe [TH.TyVarBndr ()]) (ruleBndrList ∷ [TH.RuleBndr]) 
    (exp₁ ∷ TH.Exp) (exp₂ ∷ TH.Exp) (phases ∷ TH.Phases) →
      TH.RuleP 
        string_ (HS.fmap (map thStripModuleNamesTyVarBndr) tyVarBndrListMaybe) (map thStripModuleNamesRuleBndr ruleBndrList) 
        (thStripModuleNamesExp exp₁) (thStripModuleNamesExp exp₂) phases
  TH.AnnP (annTarget ∷ TH.AnnTarget) (exp ∷ TH.Exp) → TH.AnnP (thStripModuleNamesAnnTarget annTarget) $ thStripModuleNamesExp exp
  TH.LineP (i ∷ HS.Int) (string_ ∷ [HS.Char]) → TH.LineP i string_
  TH.CompleteP (nameList ∷ [TH.Name]) (nameMaybe ∷ HS.Maybe TH.Name) →
    TH.CompleteP (map thStripModuleNamesName nameList) $ HS.fmap thStripModuleNamesName  nameMaybe

thStripModuleNamesTySynEqn ∷ TH.TySynEqn → TH.TySynEqn
thStripModuleNamesTySynEqn = \case
  TH.TySynEqn (tyVarBndrListMaybe ∷ HS.Maybe [TH.TyVarBndr ()]) (type_₁ ∷ TH.Type) (type_₂ ∷ TH.Type) →
    TH.TySynEqn (HS.fmap (map thStripModuleNamesTyVarBndr) tyVarBndrListMaybe) (thStripModuleNamesType type_₁) $ thStripModuleNamesType type_₂

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
    (injectivityAnnMaybe ∷ HS.Maybe TH.InjectivityAnn) →
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
  TH.ViaStrategy (type_ ∷ TH.Type) → TH.ViaStrategy $ thStripModuleNamesType type_

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
    (kindMaybe ∷ HS.Maybe TH.Kind) (conList ∷ [TH.Con]) (derivClauseList ∷ [TH.DerivClause]) →
      TH.DataD 
        (map thStripModuleNamesType cxt) (thStripModuleNamesName name) (map thStripModuleNamesTyVarBndr tyVarBndrList) 
        (HS.fmap thStripModuleNamesType kindMaybe) (map thStripModuleNamesCon conList) $ map thStripModuleNamesDerivClause derivClauseList
  TH.NewtypeD
    (cxt ∷ TH.Cxt) (name ∷ TH.Name) (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis]) 
    (kindMaybe ∷ HS.Maybe TH.Kind) (con ∷ TH.Con) (derivClauseList ∷ [TH.DerivClause]) →
      TH.NewtypeD 
        (map thStripModuleNamesType cxt) (thStripModuleNamesName name) (map thStripModuleNamesTyVarBndr tyVarBndrList) 
        (HS.fmap thStripModuleNamesType kindMaybe) (thStripModuleNamesCon con) $ map thStripModuleNamesDerivClause derivClauseList
  TH.TypeDataD (name ∷ TH.Name) (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis]) (kindMaybe ∷ HS.Maybe TH.Kind) (conList ∷ [TH.Con]) →
    TH.TypeDataD 
      (thStripModuleNamesName name) (map thStripModuleNamesTyVarBndr tyVarBndrList) 
      (HS.fmap thStripModuleNamesType kindMaybe) $ map thStripModuleNamesCon conList
  TH.TySynD (name ∷ TH.Name) (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis]) (type_ ∷ TH.Type) →
      TH.TySynD (thStripModuleNamesName name) (map thStripModuleNamesTyVarBndr tyVarBndrList) $ thStripModuleNamesType type_
  TH.ClassD (cxt ∷ TH.Cxt) (name ∷ TH.Name) (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis]) (funDepList ∷ [TH.FunDep]) (decList ∷ [TH.Dec]) →
    TH.ClassD 
      (map thStripModuleNamesType cxt) (thStripModuleNamesName name) (map thStripModuleNamesTyVarBndr tyVarBndrList) 
      (map thStripModuleNamesFunDep funDepList) $ map thStripModuleNamesDec decList
  TH.InstanceD (overlapMaybe ∷ HS.Maybe TH.Overlap) (cxt ∷ TH.Cxt) (type_ ∷ TH.Type) (decList ∷ [TH.Dec]) →
    TH.InstanceD overlapMaybe (map thStripModuleNamesType cxt) (thStripModuleNamesType type_) $ map thStripModuleNamesDec decList
  TH.SigD (name ∷ TH.Name) (type_ ∷ TH.Type) → TH.SigD (thStripModuleNamesName name) $ thStripModuleNamesType type_
  TH.KiSigD (name ∷ TH.Name) (kind ∷ TH.Kind) → TH.KiSigD (thStripModuleNamesName name) $ thStripModuleNamesType kind
  TH.ForeignD (foreign_ ∷ TH.Foreign) → TH.ForeignD $ thStripModuleNamesForeign foreign_
  TH.InfixD (fixity ∷ TH.Fixity) (name ∷ TH.Name) → TH.InfixD fixity $ thStripModuleNamesName name
  TH.DefaultD (tyList ∷ [TH.Type]) → TH.DefaultD $ map thStripModuleNamesType tyList
  TH.PragmaD (pragma ∷ TH.Pragma) → TH.PragmaD $ thStripModuleNamesPragma pragma
  TH.DataFamilyD (name ∷ TH.Name) (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis]) (kindMaybe ∷ HS.Maybe TH.Kind) →
      TH.DataFamilyD (thStripModuleNamesName name) (map thStripModuleNamesTyVarBndr tyVarBndrList) $ HS.fmap thStripModuleNamesType kindMaybe
  TH.DataInstD
    (cxt ∷ TH.Cxt) (tyVarBndrListMaybe ∷ HS.Maybe [TH.TyVarBndr ()]) (type_ ∷ TH.Type)
    (kindMaybe ∷ HS.Maybe TH.Kind) (conList ∷ [TH.Con]) (derivClauseList ∷ [TH.DerivClause]) →
      TH.DataInstD 
        (map thStripModuleNamesType cxt) (HS.fmap (map thStripModuleNamesTyVarBndr) tyVarBndrListMaybe) 
        (thStripModuleNamesType type_) (HS.fmap thStripModuleNamesType kindMaybe)
        (map thStripModuleNamesCon conList) $ map thStripModuleNamesDerivClause derivClauseList
  TH.NewtypeInstD
    (cxt ∷ TH.Cxt) (tyVarBndrListMaybe ∷ HS.Maybe [TH.TyVarBndr ()]) (type_ ∷ TH.Type)
    (kindMaybe ∷ HS.Maybe TH.Kind) (con ∷ TH.Con) (derivClauseList ∷ [TH.DerivClause]) →
      TH.NewtypeInstD 
        (map thStripModuleNamesType cxt) (HS.fmap (map thStripModuleNamesTyVarBndr) tyVarBndrListMaybe) 
        (thStripModuleNamesType type_) (HS.fmap thStripModuleNamesType kindMaybe) (thStripModuleNamesCon con) $
        map thStripModuleNamesDerivClause derivClauseList
  TH.TySynInstD (tySynEqn ∷ TH.TySynEqn) → TH.TySynInstD $ thStripModuleNamesTySynEqn tySynEqn
  TH.OpenTypeFamilyD (typeFamilyHead ∷ TH.TypeFamilyHead) → TH.OpenTypeFamilyD $ thStripModuleNamesTypeFamilyHead typeFamilyHead
  TH.ClosedTypeFamilyD (typeFamilyHead ∷ TH.TypeFamilyHead) (tySynEqnList ∷ [TH.TySynEqn]) →
      TH.ClosedTypeFamilyD (thStripModuleNamesTypeFamilyHead typeFamilyHead) $ map thStripModuleNamesTySynEqn tySynEqnList
  TH.RoleAnnotD (name ∷ TH.Name) (roleList ∷ [TH.Role]) → TH.RoleAnnotD (thStripModuleNamesName name) roleList
  TH.StandaloneDerivD (derivStrategyMaybe ∷ HS.Maybe TH.DerivStrategy) (cxt ∷ TH.Cxt) (type_ ∷ TH.Type) →
      TH.StandaloneDerivD 
        (HS.fmap thStripModuleNamesDerivStrategy derivStrategyMaybe) (map thStripModuleNamesType cxt) $ thStripModuleNamesType type_
  TH.DefaultSigD (name ∷ TH.Name) (type_ ∷ TH.Type) →
    TH.DefaultSigD (thStripModuleNamesName name) $ thStripModuleNamesType type_
  TH.PatSynD (name ∷ TH.Name) (patSynArgs ∷ TH.PatSynArgs) (patSynDir ∷ TH.PatSynDir) (pat ∷ TH.Pat) →
      TH.PatSynD 
        (thStripModuleNamesName name) (thStripModuleNamesPatSynArgs patSynArgs) 
        (thStripModuleNamesPatSynDir patSynDir) $ thStripModuleNamesPat pat
  TH.PatSynSigD (name ∷ TH.Name) (patSynType ∷ TH.PatSynType) → TH.PatSynSigD (thStripModuleNamesName name) $ thStripModuleNamesType patSynType
  TH.ImplicitParamBindD (string_ ∷ [HS.Char]) (exp ∷ TH.Exp) → TH.ImplicitParamBindD string_ $ thStripModuleNamesExp exp

