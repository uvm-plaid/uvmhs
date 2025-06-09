# Future

- changes to Core.Init:
  - added:

        type QIO = TH.Q
    
- changes to Core.Effects:
  - renamed:
    - `MonadQ` to `MonadQIO`
    - `???` to `qio`
  - added:

        listen ∷ (Monad m,MonadWriter o m) ⇒ m a → m (o ∧ a)
        listen xM = do
          o :* x ← hijack xM
          tell o
          return $ o :* x
        
        listenL ∷ (Monad m,MonadWriter o₁ m) ⇒ (o₁ ⟢ o₂) → m a → m (o₂ ∧ a)
        listenL ℓ xM = do
          o₁ :* x ← hijack xM
          tell o₁
          return $ access ℓ o₁ :* x

- changes to Core.Monads:
  - created `LiftQIO` and `MonadQIO` instances for all monad transformers

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

        instance Null (𝑆 a) where null = null𝑆
        instance Append (𝑆 a) where (⧺) = append𝑆
        instance Monoid (𝑆 a)
        
        instance Single a (𝑆 a) where single = single𝑆
        
        null𝑆 ∷ 𝑆 a
        null𝑆 = 𝑆 $ \ () → None
        
        single𝑆 ∷ a → 𝑆 a
        single𝑆 x = 𝑆 $ \ () → Some $ x :* null𝑆
        
        append𝑆 ∷ 𝑆 a → 𝑆 a → 𝑆 a
        append𝑆 xs ys = 𝑆 $ \ () →
          case un𝑆 xs () of
            None → un𝑆 ys ()
            Some (x :* xs') → Some $ x :* append𝑆 xs' ys

- changes to Core.Data.Set:
  - added:

        extend𝑃 ∷ (Ord b) ⇒ (a → 𝑃 b) → 𝑃 a → 𝑃 b
        extend𝑃 f = pow ∘ extend (iter ∘ f) ∘ iter

- changes to Core.Data.Arithmetic:
  - added:

        round ∷ 𝔻 → ℤ
        round = HS.round

- changes to Core.IO:
  - API is now:
    
        humanReadableTime ∷ 𝔻 → 𝕊 ∧ 𝔻
        humanReadableTime t = if 
          | t ≥ 1000000000 → "s"  :* (t / 1000000000)
          | t ≥ 1000000    → "ms" :* (t / 1000000)
          | t ≥ 1000       → "μs" :* (t / 1000)
          | otherwise      → "ns" :* t
        
        humanReadableBytes ∷ 𝔻 → 𝕊 ∧ 𝔻
        humanReadableBytes b = if 
          | b ≥ 1000000000 → "GB" :* (b / 1000000000)
          | b ≥ 1000000    → "MB" :* (b / 1000000)
          | b ≥ 1000       → "KB" :* (b / 1000)
          | otherwise       → "B"  :* b
        
        nowCPU ∷ IO ℤ64
        nowCPU = do
          gc
          s ← Stat.getRTSStats
          return $ Stat.cpu_ns s
        
        timeIO ∷ (() → IO a) → IO (a ∧ ℕ64)
        timeIO f = do
          t₁ ← nowCPU
          x ← f ()
          t₂ ← nowCPU
          return $ x :* natΩ64 (t₂ - t₁)
        
        timeIOLog ∷ 𝕊 → (() → IO a) → IO a
        timeIOLog s f = do
          out $ "TIMING: " ⧺ s 
          oflush
          x :* t ← timeIO f
          let u :* t' = humanReadableTime $ dbl t
          out $ "CPU TIME ELAPSED: " ⧺ show𝕊 t' ⧺ " " ⧺ u
          oflush
          return x
        
        profileIO ∷ (() → IO a) → IO (a ∧ ℕ64 ∧ 𝔻)
        profileIO xM = do
          gc
          s₁ ← Stat.getRTSStats
          x ← xM ()
          gc
          s₂ ← Stat.getRTSStats
          let n₁ = Stat.major_gcs s₁
              u₁ = Stat.cumulative_live_bytes s₁
              t₁ = Stat.cpu_ns s₁
              n₂ = Stat.major_gcs s₂
              u₂ = Stat.cumulative_live_bytes s₂
              t₂ = Stat.cpu_ns s₂
              t = natΩ64 $ t₂ - t₁
              m  = dbl (u₂ - u₁) / dbl (n₂ - n₁)
          return $ x :* t :* m
        
        profileIOLog ∷ 𝕊 → (() → IO a) → IO a
        profileIOLog s xM = do
          out $ "TIMING AND MEMORY: " ⧺ s 
          oflush
          x :* t :* m ← profileIO xM
          let ut :* t' = humanReadableTime $ dbl t
              um :* m' = humanReadableBytes m
          out $ "CPU TIME ELAPSED: " ⧺ show𝕊 t' ⧺ " " ⧺ ut
          out $ "AVERAGE MEMORY USED: " ⧺ show𝕊 m' ⧺ " " ⧺ um
          oflush
          return x

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

- new IterTypes benchmark
