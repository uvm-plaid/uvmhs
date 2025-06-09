module UVMHS.Future.TH.MapName where

import UVMHS.Core

import qualified Prelude as HS
import qualified Data.List.NonEmpty as HS

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

---------------
-- MAP NAMES --
---------------

thMapNameTyVarBndr ∷ ∀ flag. (TH.Name → TH.Name) → TH.TyVarBndr flag → TH.TyVarBndr flag
thMapNameTyVarBndr f = \case
  TH.PlainTV (name ∷ TH.Name) (flag ∷ flag) →
    TH.PlainTV (f name) flag
  TH.KindedTV (name ∷ TH.Name) (flag ∷ flag) (kind ∷ TH.Kind) →
    TH.KindedTV (f name) flag $ thMapNameType f kind

thMapNameType ∷ (TH.Name → TH.Name) → TH.Type → TH.Type
thMapNameType f = \case
  TH.ForallT (tyVarBndrs ∷ [TH.TyVarBndr TH.Specificity]) (cxt ∷ TH.Cxt) (type_ ∷ TH.Type) → 
    TH.ForallT (map (thMapNameTyVarBndr f) tyVarBndrs) (map (thMapNameType f) cxt) $ thMapNameType f type_
  TH.ForallVisT (tyVarBndrs ∷ [TH.TyVarBndr ()]) (type_ ∷ TH.Type) → 
    TH.ForallVisT (map (thMapNameTyVarBndr f) tyVarBndrs) $ thMapNameType f type_
  TH.AppT (type_₁ ∷ TH.Type) (type_₂ ∷ TH.Type) → TH.AppT (thMapNameType f type_₁) $ thMapNameType f type_₂
  TH.AppKindT (type_ ∷ TH.Type) (kind ∷ TH.Kind) → TH.AppKindT (thMapNameType f type_) $ thMapNameType f kind
  TH.SigT (type_ ∷ TH.Type) (kind ∷ TH.Kind) → TH.SigT (thMapNameType f type_) $ thMapNameType f kind
  TH.VarT (name ∷ TH.Name) → TH.VarT $ f name
  TH.ConT (name ∷ TH.Name) → TH.ConT $ f name
  TH.PromotedT (name ∷ TH.Name) → TH.PromotedT $ f name
  TH.InfixT (type_₁ ∷ TH.Type) (name ∷ TH.Name) (type_₂ ∷ TH.Type) → 
    TH.InfixT (thMapNameType f type_₁) (f name) $ thMapNameType f type_₂
  TH.UInfixT (type_₁ ∷ TH.Type) (name ∷ TH.Name) (type_₂ ∷ TH.Type) → 
    TH.UInfixT (thMapNameType f type_₁) (f name) $ thMapNameType f type_₂
  TH.PromotedInfixT (type_₁ ∷ TH.Type) (name ∷ TH.Name) (type_₂ ∷ TH.Type) →
    TH.PromotedInfixT (thMapNameType f type_₁) (f name) $ thMapNameType f type_₂
  TH.PromotedUInfixT (type_₁ ∷ TH.Type) (name ∷ TH.Name) (type_₂ ∷ TH.Type) →
    TH.PromotedUInfixT (thMapNameType f type_₁) (f name) $ thMapNameType f type_₂
  TH.ParensT (type_ ∷ TH.Type) → TH.ParensT $ thMapNameType f type_
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
  TH.ImplicitParamT (s ∷ HS.String) (type_ ∷ TH.Type) → TH.ImplicitParamT s $ thMapNameType f type_

thMapNamePat ∷ (TH.Name → TH.Name) → TH.Pat → TH.Pat
thMapNamePat f = \case
  TH.LitP (lit ∷ TH.Lit) → TH.LitP lit
  TH.VarP (name ∷ TH.Name) → TH.VarP $ f name
  TH.TupP (patList ∷ [TH.Pat]) → TH.TupP $ map (thMapNamePat f) patList
  TH.UnboxedTupP (patList ∷ [TH.Pat]) → TH.UnboxedTupP $ map (thMapNamePat f) patList
  TH.UnboxedSumP (pat ∷ TH.Pat) (sumAlt ∷ TH.SumAlt) (sumArity ∷ TH.SumArity) →
    TH.UnboxedSumP (thMapNamePat f pat) sumAlt sumArity
  TH.ConP (name ∷ TH.Name) (tyList ∷ [TH.Type]) (patList ∷ [TH.Pat]) →
    TH.ConP (f name) (map (thMapNameType f) tyList) $ map (thMapNamePat f) patList
  TH.InfixP (pat₁ ∷ TH.Pat) (name ∷ TH.Name) (pat₂ ∷ TH.Pat) →
    TH.InfixP (thMapNamePat f pat₁) (f name) $ thMapNamePat f pat₂
  TH.UInfixP (pat₁ ∷ TH.Pat) (name ∷ TH.Name) (pat₂ ∷ TH.Pat) →
    TH.UInfixP (thMapNamePat f pat₁) (f name) $ thMapNamePat f pat₂
  TH.ParensP (pat ∷ TH.Pat) → TH.ParensP $ thMapNamePat f pat
  TH.TildeP (pat ∷ TH.Pat) → TH.TildeP $ thMapNamePat f pat
  TH.BangP (pat ∷ TH.Pat) → TH.BangP $ thMapNamePat f pat
  TH.AsP (name ∷ TH.Name) (pat ∷ TH.Pat) → TH.AsP (f name) $ thMapNamePat f pat
  TH.WildP → TH.WildP
  TH.RecP (name ∷ TH.Name) (fieldPatList ∷ [TH.FieldPat]) → 
    TH.RecP (f name) $ mapOn fieldPatList $ \ (nameᵢ ∷ TH.Name,pat ∷ TH.Pat) → 
      (f nameᵢ,thMapNamePat f pat)
  TH.ListP (patList ∷ [TH.Pat]) → TH.ListP $ map (thMapNamePat f) patList
  TH.SigP (pat ∷ TH.Pat) (type_ ∷ TH.Type) → TH.SigP (thMapNamePat f pat) $ thMapNameType f type_
  TH.ViewP (exp ∷ TH.Exp) (pat ∷ TH.Pat) → TH.ViewP (thMapNameExp f exp) $ thMapNamePat f pat

thMapNameStmt ∷ (TH.Name → TH.Name) → TH.Stmt → TH.Stmt
thMapNameStmt f = \case
  TH.BindS (pat ∷ TH.Pat) (exp ∷ TH.Exp) → TH.BindS (thMapNamePat f pat) $ thMapNameExp f exp
  TH.LetS (decList ∷ [TH.Dec]) → TH.LetS $ map (thMapNameDec f) decList
  TH.NoBindS (exp ∷ TH.Exp) → TH.NoBindS $ thMapNameExp f exp
  TH.ParS (stmtListList ∷ [[TH.Stmt]]) → TH.ParS $ mapp (thMapNameStmt f) stmtListList
  TH.RecS (stmtList ∷ [TH.Stmt]) → TH.RecS $ map (thMapNameStmt f) stmtList

thMapNameGuard ∷ (TH.Name → TH.Name) → TH.Guard → TH.Guard
thMapNameGuard f = \case
  TH.NormalG (exp ∷ TH.Exp) → TH.NormalG $ thMapNameExp f exp
  TH.PatG (stmtList ∷ [TH.Stmt]) → TH.PatG $ map (thMapNameStmt f) stmtList

thMapNameBody ∷ (TH.Name → TH.Name) → TH.Body → TH.Body
thMapNameBody f = \case
  TH.GuardedB (guardExpList ∷ [(TH.Guard,TH.Exp)]) →
    TH.GuardedB $ mapOn guardExpList $ \ (guard_ ∷ TH.Guard,exp ∷ TH.Exp) → 
      (thMapNameGuard f guard_,thMapNameExp f exp)
  TH.NormalB (exp ∷ TH.Exp) → TH.NormalB $ thMapNameExp f exp

thMapNameClause ∷ (TH.Name → TH.Name) → TH.Clause → TH.Clause
thMapNameClause f = \case
  TH.Clause (patList ∷ [TH.Pat]) (body ∷ TH.Body) (decList ∷ [TH.Dec]) →
    TH.Clause (map (thMapNamePat f) patList) (thMapNameBody f body) $ map (thMapNameDec f) decList

thMapNameMatch ∷ (TH.Name → TH.Name) → TH.Match → TH.Match
thMapNameMatch f = \case
  TH.Match (pat ∷ TH.Pat) (body ∷ TH.Body) (decList ∷ [TH.Dec]) →
    TH.Match (thMapNamePat f pat) (thMapNameBody f body) $ map (thMapNameDec f) decList

thMapNameRange ∷ (TH.Name → TH.Name) → TH.Range → TH.Range
thMapNameRange f = \case
  TH.FromR (exp ∷ TH.Exp) → TH.FromR $ thMapNameExp f exp
  TH.FromThenR (exp₁ ∷ TH.Exp) (exp₂ ∷ TH.Exp) → TH.FromThenR (thMapNameExp f exp₁) $ thMapNameExp f exp₂
  TH.FromToR (exp₁ ∷ TH.Exp) (exp₂ ∷ TH.Exp) → TH.FromToR (thMapNameExp f exp₁) $ thMapNameExp f exp₂
  TH.FromThenToR (exp₁ ∷ TH.Exp) (exp₂ ∷ TH.Exp) (exp₃ ∷ TH.Exp) → 
    TH.FromThenToR (thMapNameExp f exp₁) (thMapNameExp f exp₂) $ thMapNameExp f exp₃

thMapNameExp ∷ (TH.Name → TH.Name) → TH.Exp → TH.Exp
thMapNameExp f = \case
  TH.VarE (name ∷ TH.Name) → TH.VarE $ f name
  TH.ConE (name ∷ TH.Name) → TH.ConE $ f name
  TH.LitE (lit ∷ TH.Lit) → TH.LitE lit
  TH.AppE (exp₁ ∷ TH.Exp) (exp₂ ∷ TH.Exp) → TH.AppE (thMapNameExp f exp₁) $ thMapNameExp f exp₂
  TH.AppTypeE (exp ∷ TH.Exp) (type_ ∷ TH.Type) → TH.AppTypeE (thMapNameExp f exp) $ thMapNameType f type_
  TH.InfixE (expMaybe₁ ∷ HS.Maybe TH.Exp) (exp ∷ TH.Exp) (expMaybe₂ ∷ HS.Maybe TH.Exp) →
    TH.InfixE (HS.fmap (thMapNameExp f) expMaybe₁) (thMapNameExp f exp) $ HS.fmap (thMapNameExp f) expMaybe₂
  TH.UInfixE (exp₁ ∷ TH.Exp) (exp₂ ∷ TH.Exp) (exp₃ ∷ TH.Exp) →
    TH.UInfixE (thMapNameExp f exp₁) (thMapNameExp f exp₂) $ thMapNameExp f exp₃
  TH.ParensE (exp ∷ TH.Exp) → TH.ParensE $ thMapNameExp f exp
  TH.LamE (patList ∷ [TH.Pat]) (exp ∷ TH.Exp) → TH.LamE (map (thMapNamePat f) patList) $ thMapNameExp f exp
  TH.LamCaseE (matchList ∷ [TH.Match]) → TH.LamCaseE $ map (thMapNameMatch f) matchList
  TH.LamCasesE (clauseList ∷ [TH.Clause]) → TH.LamCasesE $ map (thMapNameClause f) clauseList
  TH.TupE (expMaybeList ∷ [HS.Maybe TH.Exp]) → TH.TupE $ map (HS.fmap (thMapNameExp f)) expMaybeList
  TH.UnboxedTupE (expMaybeList ∷ [HS.Maybe TH.Exp]) → TH.UnboxedTupE $ map (HS.fmap (thMapNameExp f)) expMaybeList
  TH.UnboxedSumE (exp ∷ TH.Exp) (sumAlt ∷ TH.SumAlt) (sumArity ∷ TH.SumArity) → 
    TH.UnboxedSumE (thMapNameExp f exp) sumAlt sumArity
  TH.CondE (exp₁ ∷ TH.Exp) (exp₂ ∷ TH.Exp) (exp₃ ∷ TH.Exp) →
    TH.CondE (thMapNameExp f exp₁) (thMapNameExp f exp₂) $ thMapNameExp f exp₃
  TH.MultiIfE (guardExpList ∷ [(TH.Guard,TH.Exp)]) →
    TH.MultiIfE $ mapOn guardExpList $ \ (grd ∷ TH.Guard,exp ∷ TH.Exp) → (thMapNameGuard f grd,thMapNameExp f exp)
  TH.LetE (decList ∷ [TH.Dec]) (exp ∷ TH.Exp) → TH.LetE (map (thMapNameDec f) decList) $ thMapNameExp f exp
  TH.CaseE (exp ∷ TH.Exp) (matchList ∷ [TH.Match]) → TH.CaseE (thMapNameExp f exp) $ map (thMapNameMatch f) matchList
  TH.DoE (modNameMaybe ∷ HS.Maybe TH.ModName) (stmtList ∷ [TH.Stmt]) → TH.DoE modNameMaybe $ map (thMapNameStmt f) stmtList
  TH.MDoE (modNameMaybe ∷ HS.Maybe TH.ModName) (stmtList ∷ [TH.Stmt]) → TH.MDoE modNameMaybe $ map (thMapNameStmt f) stmtList
  TH.CompE (stmtList ∷ [TH.Stmt]) → TH.CompE $ map (thMapNameStmt f) stmtList
  TH.ArithSeqE (range_ ∷ TH.Range) → TH.ArithSeqE $ thMapNameRange f range_
  TH.ListE (expList ∷ [TH.Exp]) → TH.ListE $ map (thMapNameExp f) expList
  TH.SigE (exp ∷ TH.Exp) (type_ ∷ TH.Type) → TH.SigE (thMapNameExp f exp) $ thMapNameType f type_
  TH.RecConE (name ∷ TH.Name) (fieldExpList ∷ [TH.FieldExp]) →
    TH.RecConE (f name) $ mapOn fieldExpList $ \ (nameᵢ ∷ TH.Name,exp ∷ TH.Exp) → 
      (f nameᵢ,thMapNameExp f exp)
  TH.RecUpdE (exp ∷ TH.Exp) (fieldExpList ∷ [TH.FieldExp]) →
    TH.RecUpdE (thMapNameExp f exp) $ mapOn fieldExpList $ \ (nameᵢ ∷ TH.Name,expᵢ ∷ TH.Exp) → 
      (f nameᵢ,thMapNameExp f expᵢ)
  TH.StaticE (exp ∷ TH.Exp) → TH.StaticE $ thMapNameExp f exp
  TH.UnboundVarE (name ∷ TH.Name) → TH.UnboundVarE $ f name
  TH.LabelE (string_ ∷ [HS.Char]) → TH.LabelE string_
  TH.ImplicitParamVarE (string_ ∷ [HS.Char]) → TH.ImplicitParamVarE string_
  TH.GetFieldE (exp ∷ TH.Exp) (string_ ∷ [HS.Char]) → TH.GetFieldE (thMapNameExp f exp) string_
  TH.ProjectionE (stringNonEmpty ∷ HS.NonEmpty [HS.Char]) → TH.ProjectionE stringNonEmpty
  TH.TypedBracketE (exp ∷ TH.Exp) → TH.TypedBracketE $ thMapNameExp f exp
  TH.TypedSpliceE (exp ∷ TH.Exp) → TH.TypedSpliceE $ thMapNameExp f exp

thMapNameCon ∷ (TH.Name → TH.Name) → TH.Con → TH.Con
thMapNameCon f = \case
  TH.NormalC (name ∷ TH.Name) (bangTypeList ∷ [TH.BangType]) → 
    TH.NormalC (f name) $ mapOn bangTypeList $ \ (bang ∷ TH.Bang,type_ ∷ TH.Type) → 
      (bang,thMapNameType f type_)
  TH.RecC (name ∷ TH.Name) (varBangTypeList ∷ [TH.VarBangType]) →
    TH.RecC (f name) $ mapOn varBangTypeList $ \ (nameᵢ ∷ TH.Name,bang ∷ TH.Bang,type_ ∷ TH.Type) →
      (f nameᵢ,bang,thMapNameType f type_)
  TH.InfixC (bang₁ ∷ TH.Bang,type_₁ ∷ TH.Type) (name ∷ TH.Name) (bang₂ ∷ TH.Bang,type_₂ ∷ TH.Type) → 
    TH.InfixC (bang₁,thMapNameType f type_₁) (f name) (bang₂,thMapNameType f type_₂)
  TH.ForallC (tyVarBndrList ∷ [TH.TyVarBndr TH.Specificity]) (cxt ∷ TH.Cxt) (con ∷ TH.Con) →
    TH.ForallC (map (thMapNameTyVarBndr f) tyVarBndrList) (map (thMapNameType f) cxt) $ thMapNameCon f con
  TH.GadtC (nameList ∷ [TH.Name]) (bangTypeList ∷ [TH.BangType]) (type_ ∷ TH.Type) →
    let bangTypeList' = mapOn bangTypeList $ \ (bang ∷ TH.Bang,tyᵢ ∷ TH.Type) → (bang,thMapNameType f tyᵢ) in
    TH.GadtC (map f nameList) bangTypeList' $ thMapNameType f type_
  TH.RecGadtC (nameList ∷ [TH.Name]) (varBangTypeList ∷ [TH.VarBangType]) (type_ ∷ TH.Type) →
    let varBangTypeList' = mapOn varBangTypeList $ \ (name ∷ TH.Name,bang ∷ TH.Bang,tyᵢ ∷ TH.Type) → 
          (f name,bang,thMapNameType f tyᵢ) 
    in
    TH.RecGadtC (map f nameList) varBangTypeList' $ thMapNameType f type_

thMapNameDerivClause ∷ (TH.Name → TH.Name) → TH.DerivClause → TH.DerivClause
thMapNameDerivClause f = \case
  TH.DerivClause (derivStrategyMaybe ∷ HS.Maybe TH.DerivStrategy) (cxt ∷ TH.Cxt) →
    TH.DerivClause derivStrategyMaybe $ map (thMapNameType f) cxt

thMapNameFunDep ∷ (TH.Name → TH.Name) → TH.FunDep → TH.FunDep
thMapNameFunDep f = \case
  TH.FunDep (names₁ ∷ [TH.Name]) (names₂ ∷ [TH.Name]) →
    TH.FunDep (map f names₁) $ map f names₂

thMapNameForeign ∷ (TH.Name → TH.Name) → TH.Foreign → TH.Foreign
thMapNameForeign f = \case
  TH.ImportF (callconv ∷ TH.Callconv) (safety ∷ TH.Safety) (string_ ∷ [HS.Char]) (name ∷ TH.Name) (type_ ∷ TH.Type) →
    TH.ImportF callconv safety string_ (f name) $ thMapNameType f type_
  TH.ExportF (callconv ∷ TH.Callconv) (string_ ∷ [HS.Char]) (name ∷ TH.Name) (type_ ∷ TH.Type) →
    TH.ExportF callconv string_ (f name) $ thMapNameType f type_

thMapNameRuleBndr ∷ (TH.Name → TH.Name) → TH.RuleBndr → TH.RuleBndr
thMapNameRuleBndr f = \case
  TH.RuleVar (name ∷ TH.Name) → TH.RuleVar $ f name
  TH.TypedRuleVar (name ∷ TH.Name) (type_ ∷ TH.Type) → TH.TypedRuleVar (f name) $ thMapNameType f type_

thMapNameAnnTarget ∷ (TH.Name → TH.Name) → TH.AnnTarget → TH.AnnTarget
thMapNameAnnTarget f = \case
  TH.ModuleAnnotation → TH.ModuleAnnotation
  TH.TypeAnnotation (name ∷ TH.Name) → TH.TypeAnnotation $ f name
  TH.ValueAnnotation (name ∷ TH.Name) → TH.ValueAnnotation $ f name

thMapNamePragma ∷ (TH.Name → TH.Name) → TH.Pragma → TH.Pragma
thMapNamePragma f = \case
  TH.InlineP (name ∷ TH.Name) (inline ∷ TH.Inline) (ruleMatch ∷ TH.RuleMatch) (phases ∷ TH.Phases) →
    TH.InlineP (f name) inline ruleMatch phases
  TH.OpaqueP (name ∷ TH.Name) → TH.OpaqueP $ f name
  TH.SpecialiseP (name ∷ TH.Name) (type_ ∷ TH.Type) (inlineMaybe ∷ HS.Maybe TH.Inline) (phases ∷ TH.Phases) →
    TH.SpecialiseP (f name) (thMapNameType f type_) inlineMaybe phases
  TH.SpecialiseInstP (type_ ∷ TH.Type) → TH.SpecialiseInstP $ thMapNameType f type_
  TH.RuleP 
    (string_ ∷ [HS.Char]) (tyVarBndrListMaybe ∷ HS.Maybe [TH.TyVarBndr ()]) (ruleBndrList ∷ [TH.RuleBndr]) 
    (exp₁ ∷ TH.Exp) (exp₂ ∷ TH.Exp) (phases ∷ TH.Phases) →
      TH.RuleP 
        string_ (HS.fmap (map (thMapNameTyVarBndr f)) tyVarBndrListMaybe) (map (thMapNameRuleBndr f) ruleBndrList) 
        (thMapNameExp f exp₁) (thMapNameExp f exp₂) phases
  TH.AnnP (annTarget ∷ TH.AnnTarget) (exp ∷ TH.Exp) → TH.AnnP (thMapNameAnnTarget f annTarget) $ thMapNameExp f exp
  TH.LineP (i ∷ HS.Int) (string_ ∷ [HS.Char]) → TH.LineP i string_
  TH.CompleteP (nameList ∷ [TH.Name]) (nameMaybe ∷ HS.Maybe TH.Name) →
    TH.CompleteP (map f nameList) $ HS.fmap f  nameMaybe

thMapNameTySynEqn ∷ (TH.Name → TH.Name) → TH.TySynEqn → TH.TySynEqn
thMapNameTySynEqn f = \case
  TH.TySynEqn (tyVarBndrListMaybe ∷ HS.Maybe [TH.TyVarBndr ()]) (type_₁ ∷ TH.Type) (type_₂ ∷ TH.Type) →
    TH.TySynEqn (HS.fmap (map (thMapNameTyVarBndr f)) tyVarBndrListMaybe) (thMapNameType f type_₁) $ thMapNameType f type_₂

thMapNameFamilyResultSig ∷ (TH.Name → TH.Name) → TH.FamilyResultSig → TH.FamilyResultSig
thMapNameFamilyResultSig f = \case
  TH.NoSig →
    TH.NoSig
  TH.KindSig (kind ∷ TH.Kind) →
    TH.KindSig $ thMapNameType f kind
  TH.TyVarSig (tyVarBndr ∷ TH.TyVarBndr ()) →
    TH.TyVarSig $ thMapNameTyVarBndr f tyVarBndr

thMapNameInjectivityAnn ∷ (TH.Name → TH.Name) → TH.InjectivityAnn → TH.InjectivityAnn
thMapNameInjectivityAnn f = \case
  TH.InjectivityAnn (name ∷ TH.Name) (nameList ∷ [TH.Name]) →
    TH.InjectivityAnn (f name) $ map f nameList

thMapNameTypeFamilyHead ∷ (TH.Name → TH.Name) → TH.TypeFamilyHead → TH.TypeFamilyHead
thMapNameTypeFamilyHead f = \case
  TH.TypeFamilyHead 
    (name ∷ TH.Name) 
    (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis]) 
    (familyResultSig ∷ TH.FamilyResultSig)
    (injectivityAnnMaybe ∷ HS.Maybe TH.InjectivityAnn) →
      TH.TypeFamilyHead 
        (f name) 
        (map (thMapNameTyVarBndr f) tyVarBndrList)
        (thMapNameFamilyResultSig f familyResultSig) $
        HS.fmap (thMapNameInjectivityAnn f) injectivityAnnMaybe

thMapNameDerivStrategy ∷ (TH.Name → TH.Name) → TH.DerivStrategy → TH.DerivStrategy
thMapNameDerivStrategy f = \case
  TH.StockStrategy → TH.StockStrategy
  TH.AnyclassStrategy → TH.AnyclassStrategy
  TH.NewtypeStrategy → TH.NewtypeStrategy
  TH.ViaStrategy (type_ ∷ TH.Type) → TH.ViaStrategy $ thMapNameType f type_

thMapNamePatSynArgs ∷ (TH.Name → TH.Name) → TH.PatSynArgs → TH.PatSynArgs
thMapNamePatSynArgs f = \case
  TH.PrefixPatSyn (nameList ∷ [TH.Name]) →
    TH.PrefixPatSyn $ map f nameList
  TH.InfixPatSyn (name₁ ∷ TH.Name) (name₂ ∷ TH.Name) →
    TH.InfixPatSyn (f name₁) $ f name₂
  TH.RecordPatSyn (nameList ∷ [TH.Name]) →
    TH.RecordPatSyn $ map f nameList

thMapNamePatSynDir ∷ (TH.Name → TH.Name) → TH.PatSynDir → TH.PatSynDir
thMapNamePatSynDir f = \case
  TH.Unidir →
    TH.Unidir
  TH.ImplBidir →
    TH.ImplBidir
  TH.ExplBidir (clauseList ∷ [TH.Clause]) →
    TH.ExplBidir $ map (thMapNameClause f) clauseList

thMapNameDec ∷ (TH.Name → TH.Name) → TH.Dec → TH.Dec
thMapNameDec f = \case
  TH.FunD (name ∷ TH.Name) (clauseList ∷ [TH.Clause]) → 
    TH.FunD (f name) $ map (thMapNameClause f) clauseList
  TH.ValD (pat ∷ TH.Pat) (body ∷ TH.Body) (decList ∷ [TH.Dec]) →
    TH.ValD (thMapNamePat f pat) (thMapNameBody f body) $ map (thMapNameDec f) decList
  TH.DataD 
    (cxt ∷ TH.Cxt) (name ∷ TH.Name) (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis]) 
    (kindMaybe ∷ HS.Maybe TH.Kind) (conList ∷ [TH.Con]) (derivClauseList ∷ [TH.DerivClause]) →
      TH.DataD 
        (map (thMapNameType f) cxt) (f name) (map (thMapNameTyVarBndr f) tyVarBndrList) 
        (HS.fmap (thMapNameType f) kindMaybe) (map (thMapNameCon f) conList) $ map (thMapNameDerivClause f) derivClauseList
  TH.NewtypeD
    (cxt ∷ TH.Cxt) (name ∷ TH.Name) (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis]) 
    (kindMaybe ∷ HS.Maybe TH.Kind) (con ∷ TH.Con) (derivClauseList ∷ [TH.DerivClause]) →
      TH.NewtypeD 
        (map (thMapNameType f) cxt) (f name) (map (thMapNameTyVarBndr f) tyVarBndrList) 
        (HS.fmap (thMapNameType f) kindMaybe) (thMapNameCon f con) $ map (thMapNameDerivClause f) derivClauseList
  TH.TypeDataD (name ∷ TH.Name) (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis]) (kindMaybe ∷ HS.Maybe TH.Kind) (conList ∷ [TH.Con]) →
    TH.TypeDataD 
      (f name) (map (thMapNameTyVarBndr f) tyVarBndrList) 
      (HS.fmap (thMapNameType f) kindMaybe) $ map (thMapNameCon f) conList
  TH.TySynD (name ∷ TH.Name) (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis]) (type_ ∷ TH.Type) →
      TH.TySynD (f name) (map (thMapNameTyVarBndr f) tyVarBndrList) $ thMapNameType f type_
  TH.ClassD (cxt ∷ TH.Cxt) (name ∷ TH.Name) (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis]) (funDepList ∷ [TH.FunDep]) (decList ∷ [TH.Dec]) →
    TH.ClassD 
      (map (thMapNameType f) cxt) (f name) (map (thMapNameTyVarBndr f) tyVarBndrList) 
      (map (thMapNameFunDep f) funDepList) $ map (thMapNameDec f) decList
  TH.InstanceD (overlapMaybe ∷ HS.Maybe TH.Overlap) (cxt ∷ TH.Cxt) (type_ ∷ TH.Type) (decList ∷ [TH.Dec]) →
    TH.InstanceD overlapMaybe (map (thMapNameType f) cxt) (thMapNameType f type_) $ map (thMapNameDec f) decList
  TH.SigD (name ∷ TH.Name) (type_ ∷ TH.Type) → TH.SigD (f name) $ thMapNameType f type_
  TH.KiSigD (name ∷ TH.Name) (kind ∷ TH.Kind) → TH.KiSigD (f name) $ thMapNameType f kind
  TH.ForeignD (foreign_ ∷ TH.Foreign) → TH.ForeignD $ thMapNameForeign f foreign_
  TH.InfixD (fixity ∷ TH.Fixity) (name ∷ TH.Name) → TH.InfixD fixity $ f name
  TH.DefaultD (tyList ∷ [TH.Type]) → TH.DefaultD $ map (thMapNameType f) tyList
  TH.PragmaD (pragma ∷ TH.Pragma) → TH.PragmaD $ thMapNamePragma f pragma
  TH.DataFamilyD (name ∷ TH.Name) (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis]) (kindMaybe ∷ HS.Maybe TH.Kind) →
      TH.DataFamilyD (f name) (map (thMapNameTyVarBndr f) tyVarBndrList) $ HS.fmap (thMapNameType f) kindMaybe
  TH.DataInstD
    (cxt ∷ TH.Cxt) (tyVarBndrListMaybe ∷ HS.Maybe [TH.TyVarBndr ()]) (type_ ∷ TH.Type)
    (kindMaybe ∷ HS.Maybe TH.Kind) (conList ∷ [TH.Con]) (derivClauseList ∷ [TH.DerivClause]) →
      TH.DataInstD 
        (map (thMapNameType f) cxt) (HS.fmap (map (thMapNameTyVarBndr f)) tyVarBndrListMaybe) 
        (thMapNameType f type_) (HS.fmap (thMapNameType f) kindMaybe)
        (map (thMapNameCon f) conList) $ map (thMapNameDerivClause f) derivClauseList
  TH.NewtypeInstD
    (cxt ∷ TH.Cxt) (tyVarBndrListMaybe ∷ HS.Maybe [TH.TyVarBndr ()]) (type_ ∷ TH.Type)
    (kindMaybe ∷ HS.Maybe TH.Kind) (con ∷ TH.Con) (derivClauseList ∷ [TH.DerivClause]) →
      TH.NewtypeInstD 
        (map (thMapNameType f) cxt) (HS.fmap (map (thMapNameTyVarBndr f)) tyVarBndrListMaybe) 
        (thMapNameType f type_) (HS.fmap (thMapNameType f) kindMaybe) (thMapNameCon f con) $
        map (thMapNameDerivClause f) derivClauseList
  TH.TySynInstD (tySynEqn ∷ TH.TySynEqn) → TH.TySynInstD $ thMapNameTySynEqn f tySynEqn
  TH.OpenTypeFamilyD (typeFamilyHead ∷ TH.TypeFamilyHead) → TH.OpenTypeFamilyD $ thMapNameTypeFamilyHead f typeFamilyHead
  TH.ClosedTypeFamilyD (typeFamilyHead ∷ TH.TypeFamilyHead) (tySynEqnList ∷ [TH.TySynEqn]) →
      TH.ClosedTypeFamilyD (thMapNameTypeFamilyHead f typeFamilyHead) $ map (thMapNameTySynEqn f) tySynEqnList
  TH.RoleAnnotD (name ∷ TH.Name) (roleList ∷ [TH.Role]) → TH.RoleAnnotD (f name) roleList
  TH.StandaloneDerivD (derivStrategyMaybe ∷ HS.Maybe TH.DerivStrategy) (cxt ∷ TH.Cxt) (type_ ∷ TH.Type) →
      TH.StandaloneDerivD 
        (HS.fmap (thMapNameDerivStrategy f) derivStrategyMaybe) (map (thMapNameType f) cxt) $ thMapNameType f type_
  TH.DefaultSigD (name ∷ TH.Name) (type_ ∷ TH.Type) →
    TH.DefaultSigD (f name) $ thMapNameType f type_
  TH.PatSynD (name ∷ TH.Name) (patSynArgs ∷ TH.PatSynArgs) (patSynDir ∷ TH.PatSynDir) (pat ∷ TH.Pat) →
      TH.PatSynD 
        (f name) (thMapNamePatSynArgs f patSynArgs) 
        (thMapNamePatSynDir f patSynDir) $ thMapNamePat f pat
  TH.PatSynSigD (name ∷ TH.Name) (patSynType ∷ TH.PatSynType) → TH.PatSynSigD (f name) $ thMapNameType f patSynType
  TH.ImplicitParamBindD (string_ ∷ [HS.Char]) (exp ∷ TH.Exp) → TH.ImplicitParamBindD string_ $ thMapNameExp f exp

