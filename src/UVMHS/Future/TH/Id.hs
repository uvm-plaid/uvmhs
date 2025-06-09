module UVMHS.Future.TH.Id where

import qualified Prelude as HS
import qualified Data.List.NonEmpty as HS

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

------------------------
-- IDENTITY FUNCTIONS --
------------------------

thIdOccName ∷ TH.OccName → TH.OccName
thIdOccName = \case
  TH.OccName (string_ ∷ [HS.Char]) →
    TH.OccName string_

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
  TH.ForallT (tyVarBndrs ∷ [TH.TyVarBndr TH.Specificity]) (cxt ∷ TH.Cxt) (type_ ∷ TH.Type) → 
    TH.ForallT tyVarBndrs cxt type_
  TH.ForallVisT (tyVarBndrs ∷ [TH.TyVarBndr ()]) (type_ ∷ TH.Type) → 
    TH.ForallVisT tyVarBndrs type_
  TH.AppT (type_₁ ∷ TH.Type) (type_₂ ∷ TH.Type) → 
    TH.AppT type_₁ type_₂
  TH.AppKindT (type_ ∷ TH.Type) (kind ∷ TH.Kind) → 
    TH.AppKindT type_ kind
  TH.SigT (type_ ∷ TH.Type) (kind ∷ TH.Kind) → 
    TH.SigT type_ kind
  TH.VarT (name ∷ TH.Name) →
    TH.VarT name
  TH.ConT (name ∷ TH.Name) →
    TH.ConT name
  TH.PromotedT (name ∷ TH.Name) →
    TH.PromotedT name
  TH.InfixT (type_₁ ∷ TH.Type) (name ∷ TH.Name) (type_₂ ∷ TH.Type) → 
    TH.InfixT type_₁ name type_₂
  TH.UInfixT (type_₁ ∷ TH.Type) (name ∷ TH.Name) (type_₂ ∷ TH.Type) → 
    TH.UInfixT type_₁ name type_₂
  TH.PromotedInfixT (type_₁ ∷ TH.Type) (name ∷ TH.Name) (type_₂ ∷ TH.Type) →
    TH.PromotedInfixT type_₁ name type_₂
  TH.PromotedUInfixT (type_₁ ∷ TH.Type) (name ∷ TH.Name) (type_₂ ∷ TH.Type) →
    TH.PromotedUInfixT type_₁ name type_₂
  TH.ParensT (type_ ∷ TH.Type) →
    TH.ParensT type_
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
  TH.ImplicitParamT (s ∷ HS.String) (type_ ∷ TH.Type) → 
    TH.ImplicitParamT s type_

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
  TH.SigP (pat ∷ TH.Pat) (type_ ∷ TH.Type) →
    TH.SigP pat type_
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
  TH.AppTypeE (exp ∷ TH.Exp) (type_ ∷ TH.Type) →
    TH.AppTypeE exp type_
  TH.InfixE (expMaybe₁ ∷ HS.Maybe TH.Exp) (exp ∷ TH.Exp) (expMaybe₂ ∷ HS.Maybe TH.Exp) →
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
  TH.TupE (expMaybeList ∷ [HS.Maybe TH.Exp]) →
    TH.TupE expMaybeList
  TH.UnboxedTupE (expMaybeList ∷ [HS.Maybe TH.Exp]) →
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
  TH.DoE (modNameMaybe ∷ HS.Maybe TH.ModName) (stmtList ∷ [TH.Stmt]) →
    TH.DoE modNameMaybe stmtList
  TH.MDoE (modNameMaybe ∷ HS.Maybe TH.ModName) (stmtList ∷ [TH.Stmt]) →
    TH.MDoE modNameMaybe stmtList
  TH.CompE (stmtList ∷ [TH.Stmt]) →
    TH.CompE stmtList
  TH.ArithSeqE (range_ ∷ TH.Range) →
    TH.ArithSeqE range_
  TH.ListE (expList ∷ [TH.Exp]) →
    TH.ListE expList
  TH.SigE (exp ∷ TH.Exp) (type_ ∷ TH.Type) →
    TH.SigE exp type_
  TH.RecConE (name ∷ TH.Name) (fieldExpList ∷ [TH.FieldExp]) →
    TH.RecConE name fieldExpList
  TH.RecUpdE (exp ∷ TH.Exp) (fieldExpList ∷ [TH.FieldExp]) →
    TH.RecUpdE exp fieldExpList
  TH.StaticE (exp ∷ TH.Exp) →
    TH.StaticE exp
  TH.UnboundVarE (name ∷ TH.Name) →
    TH.UnboundVarE name
  TH.LabelE (string_ ∷ [HS.Char]) →
    TH.LabelE string_
  TH.ImplicitParamVarE (string_ ∷ [HS.Char]) →
    TH.ImplicitParamVarE string_
  TH.GetFieldE (exp ∷ TH.Exp) (string_ ∷ [HS.Char]) →
    TH.GetFieldE exp string_
  TH.ProjectionE (stringNonEmpty ∷ HS.NonEmpty [HS.Char]) →
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
  TH.GadtC (nameList ∷ [TH.Name]) (bangTypeList ∷ [TH.BangType]) (type_ ∷ TH.Type) →
    TH.GadtC nameList bangTypeList type_
  TH.RecGadtC (nameList ∷ [TH.Name]) (varBangTypeList ∷ [TH.VarBangType]) (type_ ∷ TH.Type) →
    TH.RecGadtC nameList varBangTypeList type_

thIdDerivClause ∷ TH.DerivClause → TH.DerivClause
thIdDerivClause = \case
  TH.DerivClause (derivStrategyMaybe ∷ HS.Maybe TH.DerivStrategy) (cxt ∷ TH.Cxt) →
    TH.DerivClause derivStrategyMaybe cxt

thIdFunDep ∷ TH.FunDep → TH.FunDep
thIdFunDep = \case
  TH.FunDep (names₁ ∷ [TH.Name]) (names₂ ∷ [TH.Name]) →
    TH.FunDep names₁ names₂

thIdForeign ∷ TH.Foreign → TH.Foreign
thIdForeign = \case
  TH.ImportF (callconv ∷ TH.Callconv) (safety ∷ TH.Safety) (string_ ∷ [HS.Char]) (name ∷ TH.Name) (type_ ∷ TH.Type) →
    TH.ImportF callconv safety string_ name type_
  TH.ExportF (callconv ∷ TH.Callconv) (string_ ∷ [HS.Char]) (name ∷ TH.Name) (type_ ∷ TH.Type) →
    TH.ExportF callconv string_ name type_

thIdRuleBndr ∷ TH.RuleBndr → TH.RuleBndr
thIdRuleBndr = \case
  TH.RuleVar (name ∷ TH.Name) →
    TH.RuleVar name
  TH.TypedRuleVar (name ∷ TH.Name) (type_ ∷ TH.Type) →
    TH.TypedRuleVar name type_

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
  TH.SpecialiseP (name ∷ TH.Name) (type_ ∷ TH.Type) (inlineMaybe ∷ HS.Maybe TH.Inline) (phases ∷ TH.Phases) →
    TH.SpecialiseP name type_ inlineMaybe phases
  TH.SpecialiseInstP (type_ ∷ TH.Type) →
    TH.SpecialiseInstP type_
  TH.RuleP 
    (string_ ∷ [HS.Char]) 
    (tyVarBndrListMaybe ∷ HS.Maybe [TH.TyVarBndr ()]) 
    (ruleBndrList ∷ [TH.RuleBndr]) 
    (exp₁ ∷ TH.Exp) 
    (exp₂ ∷ TH.Exp)
    (phases ∷ TH.Phases) →
      TH.RuleP string_ tyVarBndrListMaybe ruleBndrList exp₁ exp₂ phases
  TH.AnnP (annTarget ∷ TH.AnnTarget) (exp ∷ TH.Exp) →
    TH.AnnP annTarget exp
  TH.LineP (i ∷ HS.Int) (string_ ∷ [HS.Char]) →
    TH.LineP i string_
  TH.CompleteP (nameList ∷ [TH.Name]) (nameMaybe ∷ HS.Maybe TH.Name) →
    TH.CompleteP nameList nameMaybe

thIdTySynEqn ∷ TH.TySynEqn → TH.TySynEqn
thIdTySynEqn = \case
  TH.TySynEqn (tyVarBndrListMaybe ∷ HS.Maybe [TH.TyVarBndr ()]) (type_₁ ∷ TH.Type) (type_₂ ∷ TH.Type) →
    TH.TySynEqn tyVarBndrListMaybe type_₁ type_₂

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
    (injectivityAnnMaybe ∷ HS.Maybe TH.InjectivityAnn) →
      TH.TypeFamilyHead name tyVarBndrList familyResultSig injectivityAnnMaybe

thIdDerivStrategy ∷ TH.DerivStrategy → TH.DerivStrategy
thIdDerivStrategy = \case
  TH.StockStrategy → 
    TH.StockStrategy
  TH.AnyclassStrategy → 
    TH.AnyclassStrategy
  TH.NewtypeStrategy → 
    TH.NewtypeStrategy
  TH.ViaStrategy (type_ ∷ TH.Type) → 
    TH.ViaStrategy type_

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
    (kindMaybe ∷ HS.Maybe TH.Kind) 
    (conList ∷ [TH.Con]) 
    (derivClauseList ∷ [TH.DerivClause]) →
      TH.DataD cxt name tyVarBndrList kindMaybe conList derivClauseList
  TH.NewtypeD
    (cxt ∷ TH.Cxt) 
    (name ∷ TH.Name) 
    (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis]) 
    (kindMaybe ∷ HS.Maybe TH.Kind) 
    (con ∷ TH.Con) 
    (derivClauseList ∷ [TH.DerivClause]) →
      TH.NewtypeD cxt name tyVarBndrList kindMaybe con derivClauseList
  TH.TypeDataD 
    (name ∷ TH.Name) 
    (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis]) 
    (kindMaybe ∷ HS.Maybe TH.Kind) 
    (conList ∷ [TH.Con]) →
      TH.TypeDataD name tyVarBndrList kindMaybe conList
  TH.TySynD 
    (name ∷ TH.Name)
    (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis])
    (type_ ∷ TH.Type) →
      TH.TySynD name tyVarBndrList type_
  TH.ClassD 
    (cxt ∷ TH.Cxt)
    (name ∷ TH.Name)
    (tyVarBndrList ∷ [TH.TyVarBndr TH.BndrVis])
    (funDepList ∷ [TH.FunDep])
    (decList ∷ [TH.Dec]) →
      TH.ClassD cxt name tyVarBndrList funDepList decList
  TH.InstanceD 
    (overlapMaybe ∷ HS.Maybe TH.Overlap)
    (cxt ∷ TH.Cxt)
    (type_ ∷ TH.Type)
    (decList ∷ [TH.Dec]) →
      TH.InstanceD overlapMaybe cxt type_ decList
  TH.SigD
    (name ∷ TH.Name)
    (type_ ∷ TH.Type) →
      TH.SigD name type_
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
    (kindMaybe ∷ HS.Maybe TH.Kind) →
      TH.DataFamilyD name tyVarBndrList kindMaybe
  TH.DataInstD
    (cxt ∷ TH.Cxt)
    (tyVarBndrListMaybe ∷ HS.Maybe [TH.TyVarBndr ()])
    (type_ ∷ TH.Type)
    (kindMaybe ∷ HS.Maybe TH.Kind)
    (conList ∷ [TH.Con]) 
    (derivClauseList ∷ [TH.DerivClause]) →
      TH.DataInstD cxt tyVarBndrListMaybe type_ kindMaybe conList derivClauseList
  TH.NewtypeInstD
    (cxt ∷ TH.Cxt)
    (tyVarBndrListMaybe ∷ HS.Maybe [TH.TyVarBndr ()])
    (type_ ∷ TH.Type)
    (kindMaybe ∷ HS.Maybe TH.Kind)
    (con ∷ TH.Con) 
    (derivClauseList ∷ [TH.DerivClause]) →
      TH.NewtypeInstD cxt tyVarBndrListMaybe type_ kindMaybe con derivClauseList
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
    (derivStrategyMaybe ∷ HS.Maybe TH.DerivStrategy)
    (cxt ∷ TH.Cxt)
    (type_ ∷ TH.Type) →
      TH.StandaloneDerivD derivStrategyMaybe cxt type_
  TH.DefaultSigD
    (name ∷ TH.Name)
    (type_ ∷ TH.Type) →
      TH.DefaultSigD name type_
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
    (string_ ∷ [HS.Char])
    (exp ∷ TH.Exp) →
      TH.ImplicitParamBindD string_ exp


