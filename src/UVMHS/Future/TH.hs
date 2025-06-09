module UVMHS.Future.TH where

import UVMHS.Core hiding (thTyVarBndrName)

import UVMHS.Future.TH.MapName

-- ================ --
-- TEMPLATE HASKELL --
-- ================ --

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import qualified Prelude as HS

thGensymS ∷ 𝕊 → QIO TH.Name
thGensymS s = TH.newName $ tohsChars s

thGensymSN ∷ 𝕊 → ℕ64 → QIO TH.Name
thGensymSN s n = thGensymS $ s ⧺ show𝕊 n

thGensymN ∷ ℕ64 → QIO TH.Name
thGensymN n = thGensymSN "x" n

thGensym ∷ QIO TH.Name
thGensym = thGensymS "x"

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

thStripModuleNamesDec ∷ TH.Dec → TH.Dec
thStripModuleNamesDec = thMapNameDec $ \case
  TH.Name (occName ∷ TH.OccName) (nameFlavor ∷ TH.NameFlavour) →
    TH.Name occName $ case nameFlavor of
      TH.NameS → TH.NameS
      TH.NameQ (modName ∷ TH.ModName) → TH.NameQ modName
      TH.NameU (uniq ∷ TH.Uniq) → TH.NameU uniq
      TH.NameL (uniq ∷ TH.Uniq) → TH.NameL uniq
      -- THE PRIMARY OPERATION
      TH.NameG (_nameSpace ∷ TH.NameSpace) (_pkgName ∷ TH.PkgName) (_modName ∷ TH.ModName) →
        TH.NameS

thShowDecs ∷ TH.DecsQ → TH.ExpQ
thShowDecs decsQ = do
  decs ← decsQ
  let s = TH.pprint decs
  [| s |]

thAllTermNamesFilteredBy ∷ (TH.Name → 𝔹) → TH.Type → 𝑃 TH.Name
thAllTermNamesFilteredBy f = 
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
thAnyNameOccursInType names τ  = thAllTermNamesFilteredBy (∈ names) τ ≢ null

----------------
-- ADTConInfo --
----------------

data ADTConInfo = ADTConInfo
  { adtConInfoName     ∷ TH.Name
  , adtConInfoArgTypes ∷ [TH.Type]
  } deriving (Eq,Ord,Show)

adtConInfo ∷ 𝕊 → TH.Con → QIO ADTConInfo
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

adtInfo ∷ 𝕊 → TH.Name → QIO ADTInfo
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
      (kindO ∷ HS.Maybe (TH.Kind))
      (cons ∷ [TH.Con])
      (_derivClauses ∷ [TH.DerivClause]) → do
        when (not $ kindO ≡ HS.Nothing ⩔ kindO ≡ HS.Just TH.StarT) $ \ () → 
          fail𝕊 $ err_adtInfo_BAD_KIND kindO
        ADTInfo cxt nameCon tyVarBndrs ^$ mapM (adtConInfo fname) cons
    TH.NewtypeD 
      (cxt ∷ TH.Cxt) 
      (nameCon ∷ TH.Name) 
      (tyVarBndrs ∷ [TH.TyVarBndr TH.BndrVis])
      (kindO ∷ HS.Maybe (TH.Kind))
      (con ∷ TH.Con)
      (_derivClauses ∷ [TH.DerivClause]) → do
        when (not $ kindO ≡ HS.Nothing ⩔ kindO ≡ HS.Just TH.StarT) $ \ () → 
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
    err_adtInfo_BAD_KIND ∷ HS.Maybe TH.Kind → 𝕊
    err_adtInfo_BAD_KIND kindO = concat $ inbetween " "
      [ "<"
      , fname
      , ">"
      , "[["
      , string $ TH.pprint nameADT
      , "∷"
      , case kindO of 
          HS.Nothing → "<no-kind-annotation>" 
          HS.Just kind → string $ TH.pprint kind
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

adtInfoConssQ ∷ (Monad m,MonadQIO m) ⇒ ADTInfo → (TH.ExpQ → [TH.TypeQ] → m a) → m [a]
adtInfoConssQ 𝒾 f = mapMOn (adtInfoCons 𝒾) $ \ 𝒾C → f (qio $ TH.conE $ adtConInfoName 𝒾C) $ map return $ adtConInfoArgTypes 𝒾C

-----------------
-- ADTProdInfo --
-----------------

data ADTProdInfo = ADTProdInfo
  { adtProdInfoCxt      ∷ TH.Cxt
  , adtProdInfoName     ∷ TH.Name
  , adtProdInfoTypeArgs ∷ [TH.TyVarBndr TH.BndrVis]
  , adtProdInfoCon      ∷ ADTConInfo
  } deriving (Eq,Ord,Show)

adtProdInfo ∷ 𝕊 → TH.Name → QIO ADTProdInfo
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
