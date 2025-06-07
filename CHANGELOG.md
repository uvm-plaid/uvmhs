# Future

- changes to Core.Data.Function:
  - added:
  
        wrapAB ∷ (c → d) → (a → b) → (b → c) → a → d
        wrapAB h f g = h ∘ g ∘ f
        
        wrapBA ∷ (a → b) → (c → d) → (b → c) → a → d
        wrapBA f h g = h ∘ g ∘ f

- changes to Core.Data.Iter:
  - added:

        zipAllWith ∷ (ToIter a t₁,ToIter b t₂) ⇒ (a → c) → (b → c) → (a → b → c) → t₁ → t₂ → 𝐼 c
        zipAllWith f₁ f₂ f₃ xs ys = iter $ zipAllWith𝑆 f₁ f₂ f₃ (stream xs) $ stream ys
        
        zipAll ∷ (ToIter a t₁,ToIter b t₂) ⇒ t₁ → t₂ → 𝐼 ((a ∨ b) ∨ a ∧ b)
        zipAll = zipAllWith (Inl ∘ Inl) (Inl ∘ Inr) $ Inr ∘∘ (:*)

        apply ∷ (a → a → a) → a → [a] → a
        apply f x xs = foldOnFrom xs x $ \ xᵢ xₐ → f xₐ xᵢ

- changes to Core.Data.Stream:
  - added:

        zipAllWith𝑆 ∷ (a → c) → (b → c) → (a → b → c) → 𝑆 a → 𝑆 b → 𝑆 c
        zipAllWith𝑆 f₁ f₂ f₃ = loop
          where
            loop xs ys = 𝑆 $ \ () → case (un𝑆 xs (),un𝑆 ys ()) of
              (Some (x :* xs'),None           ) → Some (f₁ x   :* map f₁ xs'  )
              (None           ,Some (y :* ys')) → Some (f₂ y   :* map f₂ ys'  )
              (Some (x :* xs'),Some (y :* ys')) → Some (f₃ x y :* loop xs' ys')
              (None           ,None           ) → None

- changes to Core.Data.Set:
  - added:

        extend𝑃 ∷ (Ord b) ⇒ (a → 𝑃 b) → 𝑃 a → 𝑃 b
        extend𝑃 f = pow ∘ extend (iter ∘ f) ∘ iter

- changes to Lib.Parser.Core:
  - renamed:
    - `Parser` to `RawParser`
    - `runParser` to `runRawParser`
    - `pNewExpressionContext` to `rpNewExpressionContext`
    - `pGetContext` to `rpGetContext`
    - `pGetContextRendered` to `rpGetContextRendered`
    - `pWithContext` to `rpWithContext`
    - `pFail` to `rpFail`
    - `pErr` to `rpErr`
    - `pNewErrContext` to `rpNewErrContext`
    - `pNewContext ` to `rpNewContext`
    - `pWithContextRendered` to `rpWithContextRendered`
    - `pRender` to `rpRender`
    - `pAdvance` to `rpAdvance`
    - `pPluck` to `rpPluck`
    - `pRecord` to `rpRecord`
    - `pEnd` to `rpEnd`
    - `pFinal` to `rpFinal`
    - `pAny` to `rpAny`
    - `pShaped` to `rpShaped`
    - `pDie` to `rpDie`
    - `pGuard` to `rpGuard`
    - `pFailEff` to `rpFailEff`
    - `pSatisfies` to `rpSatisfies`
    - `pToken` to `rpToken`
    - `pOptional` to `rpOptional`
    - `pMany` to `rpMany`
    - `pOneOrMore` to `rpOneOrMore`
    - `pManySepBy` to `rpManySepBy`
    - `pOneOrMoreSepBy` to `rpOneOrMoreSepBy`
    - `pWord` to `rpWord`

- changes to Lib.Parser.Regex:
  - renamed:
    - `blockifyTokensTL` to `blockifyTokensTLAnchored`
  - added:
    
        blockifyTokensWSBasicUnanchored ∷ 𝕍 (PreParserToken TokenWSBasic) → 𝕍 (PreParserToken TokenWSBasic)
        blockifyTokensWSBasicUnanchored = blockifyTokensTLAnchored (shape newlineTWSBasicL) (shape blockTWSBasicL) mkIndentTokenWSBasic

        tokenizeWSAnchored ∷
          ∀ c t o u. (Show u,Ord c,Ord t,Pretty t,Classified c t,Eq o,Eq u,Plus u)
          ⇒ Lexer c t o u TokenWSBasic → 𝕊 → 𝕍 (ParserToken t) → Doc ∨ 𝕍 (ParserToken TokenWSBasic)
        tokenizeWSAnchored l so ts = do
          ts₁ ← tokenize l so ts
          let ts₂ = blockifyTokensWSBasicAnchored ts₁
              ts₃ = finalizeTokens ts₂
          return ts₃
        
        tokenizeWSAnchoredIO ∷
          ∀ c t o u. (Show u,Ord c,Ord t,Pretty t,Classified c t,Eq o,Eq u,Plus u)
          ⇒ Lexer c t o u TokenWSBasic → 𝕊 → 𝕍 (ParserToken t) → IO (𝕍 (ParserToken TokenWSBasic))
        tokenizeWSAnchoredIO l so ts = elimChoice (\ msg → do pprint msg ; abortIO) return $ tokenizeWSAnchored l so ts
        
        tokenizeWSAnchoredIOMain ∷
          ∀ c t o u. (Show u,Ord c,Ord t,Pretty t,Classified c t,Eq o,Eq u,Plus u)
          ⇒ Lexer c t o u TokenWSBasic → 𝕊 → 𝕍 (ParserToken t) → IO ()
        tokenizeWSAnchoredIOMain l so ts = do
          xs ← tokenizeWSAnchoredIO l so ts
          pprint $ ppVertical
            [ ppHeader "Success"
            , pretty $ mapOn xs $ \ x → parserTokenValue x :* parserContextLocRange (parserTokenContext x)
            ]
          pprint $ concat $ map (concat ∘ iter ∘ parserContextDisplayL ∘ parserTokenContext) xs
        
        tokenizeWSUnanchored ∷
          ∀ c t o u. (Show u,Ord c,Ord t,Pretty t,Classified c t,Eq o,Eq u,Plus u)
          ⇒ Lexer c t o u TokenWSBasic → 𝕊 → 𝕍 (ParserToken t) → Doc ∨ 𝕍 (ParserToken TokenWSBasic)
        tokenizeWSUnanchored l so ts = do
          ts₁ ← tokenize l so ts
          let ts₂ = blockifyTokensWSBasicUnanchored ts₁
              ts₃ = finalizeTokens ts₂
          return ts₃
        
        tokenizeWSUnanchoredIO ∷
          ∀ c t o u. (Show u,Ord c,Ord t,Pretty t,Classified c t,Eq o,Eq u,Plus u)
          ⇒ Lexer c t o u TokenWSBasic → 𝕊 → 𝕍 (ParserToken t) → IO (𝕍 (ParserToken TokenWSBasic))
        tokenizeWSUnanchoredIO l so ts = elimChoice (\ msg → do pprint msg ; abortIO) return $ tokenizeWSUnanchored l so ts
        
        tokenizeWSUnAnchoredIOMain ∷
          ∀ c t o u. (Show u,Ord c,Ord t,Pretty t,Classified c t,Eq o,Eq u,Plus u)
          ⇒ Lexer c t o u TokenWSBasic → 𝕊 → 𝕍 (ParserToken t) → IO ()
        tokenizeWSUnAnchoredIOMain l so ts = do
          xs ← tokenizeWSUnanchoredIO l so ts
          pprint $ ppVertical
            [ ppHeader "Success"
            , pretty $ mapOn xs $ \ x → parserTokenValue x :* parserContextLocRange (parserTokenContext x)
            ]
          pprint $ concat $ map (concat ∘ iter ∘ parserContextDisplayL ∘ parserTokenContext) xs

- changes to Lib.Substitution.hs:
  - remove: `pNameWS`
  - change: all parsers are now `TokenWSBasic` parsers
      
- changes to Lib.Parser.CParser.hs:
  - rename file `CParser.hs` to `Parser.hs`
  - rename:
    - `cp<X>` to `p<X>`
    - `p<X>[no-WS suffix]` to `p<X>_DEP`
    - `p<X>WS` to `p<X>`
  - change:
    - `pTok`, `pTokSyntax` and friends now return unit
    - `pTokRet` has the old behavior of returning the raw token
  - added:

        pTokAny ∷ (Ord t,ToIter t ts) ⇒ ts → Parser t ()
        pTokAny ts = concat $ mapOn (iter ts) pTok

        pTokSyntaxAny ∷ (ToIter 𝕊 t) ⇒ t → Parser TokenWSBasic ()
        pTokSyntaxAny = pTokAny ∘ map SyntaxTWSBasic ∘ iter

        pTokName_DEP ∷ CParser TokenBasic 𝕊
        pTokName_DEP = cpShaped $ view nameTBasicL
        
        pTokName ∷ CParser TokenWSBasic 𝕊
        pTokName = cpShaped $ view nameTWSBasicL

        pTokNat64N ∷ CParser TokenWSBasic ℕ64
        pTokNat64N = failEff ∘ natO64 *$ cpNatNWS

        pTokInt64 ∷ CParser TokenWSBasic ℤ64
        pTokInt64 = failEff ∘ intO64 *$ cpIntWS

        pTokNat ∷ CParser TokenWSBasic ℕ
        pTokNat = failEff ∘ natO *$ cpIntWS

        pTokNat64 ∷ CParser TokenWSBasic ℕ64
        pTokNat64 = failEff ∘ natO64 *$ cpIntWS

        pTokChar ∷ CParser TokenWSBasic ℂ
        pTokChar = cpShaped $ view charTWSBasicL

        tokenizeAndParse ∷ (Eq u,Show u,Plus u,Eq o,Ord w,Pretty a) ⇒ 𝕊 → Lexer CharClass ℂ o u w → CParser w a → 𝕊 → (Doc ∨ Doc) ∨ a
        tokenizeAndParse so lex xM s = do
          case tokenize lex so $ tokens s of
            Inl d → Inl $ Inl d
            Inr ts → case parse xM so $ finalizeTokens ts of
              Inl d → Inl $ Inr d
              Inr x → Inr x

- changes to Lib.Annotated:
  - added:
    
        class HasRaw r a | a → r where
          toRaw ∷ a → r
          frRaw ∷ r → a

        instance (Null e) ⇒ HasRaw a (𝐴 e a) where
          toRaw = aval
          frRaw = 𝐴 null

- new module Lib.Virtual:

      class Virtual c r v | v→r,v→c where
        virtualize ∷ r → v
        realize ∷ (c) ⇒ v → r
      
      ground ∷ ∀ c r v. (Virtual c r v,c) ⇒  v → v
      ground = virtualize ∘ realize
      
      instance Virtual (Ord a) (𝑃 a) (𝐼 a) where
        virtualize = iter
        realize = pow

- new module Lib.StreamM:


      newtype 𝑆M m a = 𝑆M { un𝑆M :: () → m (Step (Result a ∧ 𝑆M m a)) }
      newtype 𝑆MI m a = 𝑆MI { un𝑆MI ∷ 𝑆M m a }

  The idea is that `𝑆M` is the usual monadic stream type, and `𝑆MI` does fair
  interleaving between streams on `(⧺)` and `(≫=)` operations (a la miniKanren
  and LogicT)
