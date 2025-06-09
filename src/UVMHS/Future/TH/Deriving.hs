module UVMHS.Future.TH.Deriving where

import UVMHS.Core
import UVMHS.Lib.Fuzzy
import UVMHS.Lib.Pretty
import UVMHS.Future.TH

import qualified Language.Haskell.TH as TH

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

createFuzzyInstance ∷ [𝕊] → [TH.Name] → TH.Name → TH.DecsQ
createFuzzyInstance recVarNamesS recTyNamesS name = do
  when (count @ℕ64 (pow𝑃 recVarNamesS) ≢ count recVarNamesS) $ \ () →
    fail𝕊 $ err_MSG_DUPLICATE_REC_NAMES ()
  𝒾 ← adtInfo "createFuzzyInstance" name
  let nameMap ∷ 𝕊 ⇰ TH.Name
      nameMap = adtInfoTypeArgsNameMap 𝒾
  recNames ∷ 𝑃 TH.Name
            ← pow ^$ mapMOn recVarNamesS $ \ nameᵢ → do
    case nameMap ⋕? nameᵢ of
      None → fail𝕊 $ err_MSG_INVALID_REC_NAME nameMap
      Some nameᵣ → return nameᵣ
  let recNames' ∷ 𝑃 TH.Name
      recNames' = recNames ∪ single name ∪ pow recTyNamesS
  [d| instance 
        $(thTupsTQ $ mapOn (adtInfoAllConArgsQ 𝒾) $ \ τ → [t| Fuzzy $τ |])
        ⇒
        Fuzzy $(adtInfoFullTypeQ 𝒾) 
        where
          fuzzy = $(do
            d ← thGensymS "d"
            anyConAnyRecFields :* anyConAllNonRecFields :* es ← unWriterT $ adtInfoConssQ 𝒾 $ \ mk τs → 
              id @(WriterT (𝔹 ∧ 𝔹) QIO (TH.Exp)) $
              UVMHS.Core.do
                conQ :* (anyRecField :* stmts) ← evalRWST () mk $ retStateOut $ eachOn (withIndex τs) $ uncurry $ \ n τ → 
                  id @(RWST () (𝔹 ∧ 𝐼 TH.StmtQ) (TH.ExpQ) (WriterT (𝔹 ∧ 𝔹) QIO) ()) $
                  UVMHS.Core.do 
                    x' ← qio $ thGensymN n
                    modify $ \ eQ → [| $eQ $(TH.varE x') |]
                    isRec ← qio $ thAnyNameOccursInType recNames' ^$ τ
                    tellL fstL isRec
                    tellL sndL $ single $ TH.bindS (TH.varP x') $ if not isRec then [| fuzzy @($τ) |] else [| fuzzyRec @($τ) |]
                tellL fstL anyRecField
                tellL sndL $ not anyRecField
                let weight = if anyRecField then [| $(TH.varE d) |] else [| one |]
                qio [| (:*) $(weight) $ \ () → $(TH.doE $ lazyList $ concat [stmts,single $ TH.noBindS [| return $conQ |]]) |]
            when (anyConAnyRecFields ⩓ not anyConAllNonRecFields) $ \ () → 
              fail𝕊 $ concat $ inbetween " " $ 
                [ "createFuzzyInstance:"
                , "if there are any constructors that have any recursive fields"
                , "then there must be at least one constructor"
                , "that has all non-recursive fields"
                ]
            if anyConAnyRecFields
            then
              [| do $(TH.varP d) ← fuzzyDepth
                    wrchoose $(return $ TH.ListE $ lazyList es)
              |]
            else
              [| wrchoose $(return $ TH.ListE $ lazyList es)
              |])





   |]
  where
    err_MSG_DUPLICATE_REC_NAMES ∷ () → 𝕊
    err_MSG_DUPLICATE_REC_NAMES () = concat $ inbetween " "
      [ "createFuzzyInstance:"
      , "[["
      , string $ TH.pprint name
      , ","
      , ppRenderNoFmt $ pretty recVarNamesS
      , "]]"
      , "the list of provided names"
      , "should not include any duplicates"
      , "(purely as a sanity check)."
      ]
    err_MSG_INVALID_REC_NAME ∷ 𝕊 ⇰ TH.Name → 𝕊
    err_MSG_INVALID_REC_NAME nameMap = concat $ inbetween " " $
      [ "createFuzzyInstance:"
      , "[["
      , string $ TH.pprint name
      , ","
      , ppRenderNoFmt $ pretty recVarNamesS
      , "]]"
      , "each provided name must must match"
      , "the name of a type variable argument"
      , "for the declared data type."
      , "valid options are:"
      , show𝕊 $ dkeys nameMap
      ]


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
