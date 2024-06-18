module FP.Parser.Core where

import FP.Prelude
import FP.Pretty
import FP.Parser.Common
import FP.Parser.Effects

------------------
-- Parser Monad --
------------------

newtype Parser t a = Parser { runParser ∷ ReaderT (ParserEnv t) (StateT (ParserState t) (NondetAppendT (Writer (ParserOut t)))) a
  } deriving
  ( Functor,Monad
  , MonadReader (ParserEnv t)
  , MonadWriter (ParserOut t)
  , MonadState (ParserState t)
  , MonadMonoid
  )

runParserWith ∷ ParserEnv t → ParserState t → Parser t a → ([(a,ParserState t)],ParserOut t)
runParserWith r s = runWriter ∘ runNondetAppendT ∘ runStateTWith s ∘ runReaderTWith r ∘ runParser

makeParser ∷ (ParserEnv t → ParserState t → ([(a,ParserState t)],ParserOut t)) → Parser t a
makeParser f = Parser $ ReaderT $ \ r → StateT $ \ s → NondetAppendT $ writer $ f r s

---------------------------
-- Primitive Combinators --
---------------------------

pFail ∷ Parser t a
pFail = do
  pi ← getL parserStateInputL
  ek ← askL parserEnvErrorStackL
  pc ← getL parserStateErrorContextL
  tell $ ParserOut bot $ SourceError𝒪 $ errorSourceLocalContext pi ek pc
  mnull

pPluck ∷ Parser t t
pPluck = do
  SourceInput ts nextLoc ← getL parserStateInputL
  case unconsStream ts of
    Nothing → pAppendError "more input" pFail
    Just (x,ts') → do
      let nextNextLoc = case unconsStream ts' of
            Nothing → bumpCol nextLoc
            Just (x',_) → locRangeBegin $ sourceTokenRange x'
      putL parserStateInputL $ SourceInput ts' nextNextLoc
      fmt ← askL parserEnvRenderFormatL
      modifyL parserStateErrorContextL $ \ pc → pc ⧺ sourceLocalContextFromToken fmt x
      modifyL parserStateCaptureContextL $ \ pc → pc ⧺ sourceLocalContextFromToken fmt x
      return $ sourceTokenValue x

pAppendError ∷ 𝕊 → Parser t a → Parser t a
pAppendError msg xM = do
  (stack,msg') ← askL parserEnvErrorStackL
  local (update parserEnvErrorStackL (msg':stack,msg)) xM

pNewContext ∷ ParserState t ⟢ SourceContextPrefix t → Parser t a → Parser t (a,SourceContextPrefix t)
pNewContext 𝓁 xM = do
  pc ← getL 𝓁
  putL 𝓁 $ pushSourceLocalContext pc
  x ← xM
  pc' ← getL 𝓁
  putL 𝓁 $ pc ⧺ pc'
  return (x,pc')

pCapture ∷ Parser t a → Parser t (a,SourceContextPrefix t)
pCapture = pNewContext parserStateCaptureContextL

pRender ∷ Format → Parser t s → Parser t s
pRender fmt = local $ alter parserEnvRenderFormatL $ (⧺) [fmt]

pEnd ∷ Parser t ()
pEnd = do
  ts ← getL (sourceInputStreamL ⌾ parserStateInputL)
  when (shape justL $ unconsStream ts) $ pAppendError "end of stream" pFail

pCatch ∷ Parser t a → Parser t a → Parser t a
pCatch cM xM = do
  (xM',o) ← hijack $ mconcat
    [ do
        x ← xM
        tell $ ParserOut True null
        return $ Just x
    , return Nothing
    ]
  tell $ ParserOut bot $ parserOutError o
  if parserOutSuccess o
    then return𝒪 mnull xM'
    else cM

---------------------------
-- Mid-level Combinators --
---------------------------

pError ∷ 𝕊 → Parser t a → Parser t a
pError msg = compose
  [ fst ^∘ pNewContext parserStateErrorContextL
  , local (update parserEnvErrorStackL ([],msg))
  ]

pCaptureFull ∷ Parser t a → Parser t (a,SourceContext t)
pCaptureFull xM = do
  (x,c) ← pCapture xM
  pi ← getL parserStateInputL
  return (x,SourceContext c pi)

pFinal ∷ Parser t a → Parser t a
pFinal aM = do
  a ← aM
  pEnd
  return a

pShaped ∷ 𝕊 → (t → 𝒪 a) → Parser t a
pShaped msg sh = do
  s ← get
  t ← pPluck
  case sh t of
    Nothing → do
      put s
      pAppendError msg pFail
    Just x → return x

pSatisfies ∷ 𝕊 → (t → 𝔹) → Parser t t
pSatisfies msg p = pShaped msg $ \ x → if p x then Just x else Nothing

pLit ∷ (Eq t,Pretty t) ⇒ t → Parser t t
pLit l = pSatisfies (ppString l) ((==) l)

pWord ∷ ∀ s t. (Pretty s,Eq t,Pretty t,Isomorphic s [t]) ⇒ s → Parser t s
pWord s = pAppendError (ppString s) $ isoFrom isomorphic ^$ mapM pLit (isoTo isomorphic s ∷ [t])

pOptional ∷ Parser t a → Parser t (𝒪 a)
pOptional p = mconcat [map Just p,return Nothing]

pTries ∷ [Parser t a] → Parser t a
pTries = foldr (\ p₁ p₂ → pCatch p₂ p₁) mnull

pOptionalGreedy ∷ Parser t a → Parser t (𝒪 a)
pOptionalGreedy xM = pTries
  [ Just ^$ xM
  , return Nothing
  ]

pManyGreedy ∷ Parser t a → Parser t [a]
pManyGreedy xM = pTries
  [ do
      x ← xM
      xs ← pManyGreedy xM
      return $ x:xs
  , return []
  ]

pOneOrMoreGreedy ∷ Parser t a → Parser t [a]
pOneOrMoreGreedy xM = do
  x ← xM
  xs ← pManyGreedy xM
  return $ x:xs

pManySepByGreedy ∷ Parser t () → Parser t a → Parser t [a]
pManySepByGreedy sepM xM = pTries
  [ do
      x ← xM
      xs ← map snd ^$ pManyGreedy $ sepM <×> xM
      return $ x:xs
  , return []
  ]

------------------------
-- High-level Helpers --
------------------------

pLParen ∷ Parser ℂ ()
pLParen = void $ pLit '('

pRParen ∷ Parser ℂ ()
pRParen = void $ pLit ')'

pDigit ∷ Parser ℂ ℂ
pDigit = pSatisfies "digit [0-9]" isDigit

pNatural ∷ Parser ℂ ℕ
pNatural = 𝕤read ∘ 𝕤 ^$ pOneOrMoreGreedy pDigit

pInteger ∷ Parser ℂ ℤ
pInteger = do
  sign ← elim𝒪 "" 𝕤 ^$ pOptionalGreedy $ pLit '-'
  digits ← 𝕤 ^$ pOneOrMoreGreedy pDigit
  return $ 𝕤read $ sign ⧺ digits

pDouble ∷ Parser ℂ 𝔻
pDouble = do
  sign ← elim𝒪 "" 𝕤 ^$ pOptionalGreedy $ pLit '-'
  digits ← 𝕤 ^$ pOneOrMoreGreedy pDigit
  decimal ← elim𝒪 "" 𝕤 ^$ pOptionalGreedy $ do
    dot ← 𝕤 ^$ pLit '.'
    digits' ← 𝕤 ^$ pOneOrMoreGreedy pDigit
    return $ dot ⧺ digits'
  return $ 𝕤read $ sign ⧺ digits ⧺ decimal

pNumber ∷ Parser ℂ (ℤ ⊎ 𝔻)
pNumber = do
  sign ← elim𝒪 "" 𝕤 ^$ pOptionalGreedy $ pLit '-'
  digits ← 𝕤 ^$ pOneOrMoreGreedy pDigit
  decimalM ← pOptionalGreedy $ do
    dot ← 𝕤 ^$ pLit '.'
    digits' ← 𝕤 ^$ pOneOrMoreGreedy pDigit
    return $ dot ⧺ digits'
  case decimalM of
    Nothing → return $ Left $ 𝕤read $ sign ⧺ digits
    Just decimal → return $ Right $ 𝕤read $ sign ⧺ digits ⧺ decimal

pLetter ∷ Parser ℂ ℂ
pLetter = pSatisfies "letter [a-zA-Z]" isLetter

pWhitespaceGreedy ∷ Parser ℂ 𝕊
pWhitespaceGreedy = 𝕤 ^$ pOneOrMoreGreedy $ pSatisfies "whitespace [ \\t\\n]" isSpace

pOptionalWhitespaceGreedy ∷ Parser ℂ ()
pOptionalWhitespaceGreedy = void $ pOptionalGreedy $ pWhitespaceGreedy

pSurroundedBy ∷ Parser t () → Parser t () → Parser t a → Parser t a
pSurroundedBy luM ruM xM = do
  luM
  x ← xM
  ruM
  return x

pSurrounded ∷ Parser t () → Parser t a → Parser t a
pSurrounded uM = pSurroundedBy uM uM

---------------------
-- Running Parsers --
---------------------

runParser₀ ∷ (ToStream (SourceToken t) ts) ⇒ Parser t a → ts → ([(a,ParserState t)],ParserOut t)
runParser₀ p ts = runParserWith parserEnv₀ (parserState₀ $ stream ts) p

parse ∷ (ToStream (SourceToken t) ts,Pretty a) ⇒ Parser t a → ts → Doc ⊎ a
parse p ss = case runParserWith parserEnv₀ (parserState₀ $ stream ss) (pFinal p) of
  ([],ParserOut _ pe) → Left $ displaySourceError𝒪 pe
  ([(x,_)],_) → Right x
  (x:xs,_) → Left $ ppVertical $ concat
    [ return $ ppHeader "Ambiguous Parse"
    , intersperse (ppHeader "OR") $ map (pretty ∘ fst) (x:xs)
    ]

parseIO ∷ (ToStream (SourceToken t) ts,Pretty a) ⇒ Parser t a → ts → IO a
parseIO p ss = case parse p ss of
  Left d → pprint d ≫ abortIO
  Right a → return a

parseIOMain ∷ (ToStream (SourceToken t) ts,Pretty a) ⇒ Parser t a → ts → IO ()
parseIOMain p ss = do
  x ← parseIO p ss
  pprint $ ppVertical
    [ ppHeader "Success"
    , pretty x
    ]

------------------------
-- Running Tokenizers --
------------------------

tokenize ∷ ∀ ts t a. (ToStream (SourceToken t) ts) ⇒ Parser t a → ts → Doc ⊎ [SourceToken a]
tokenize p ss = loop (parserState₀ $ stream ss) null
  where
    loop ∷ ParserState t → ParserOut t → Doc ⊎ [SourceToken a]
    loop s pe
      | isEmpty $ sourceInputStream $ parserStateInput s = return null
      | otherwise =
          let (xss₀,ParserOut sd pe') = runParserWith parserEnv₀ s (tell pe ≫ pCapture p)
              ord = flip compare `on` (locPos ∘ sourceInputNextLoc ∘ parserStateInput ∘ snd)
              xss = head $ sortBy ord xss₀
          in case xss of
            Nothing → Left $ displaySourceError𝒪 pe'
            Just ((x,cc),s') → do
              xs ← loop s' $ ParserOut sd pe'
              let locRange = case sourceContextPrefixRange cc of
                    Bot →
                      let loc = sourceInputNextLoc $ parserStateInput s
                      in LocRange loc loc
                    AddBot r → r
              return $ SourceToken x locRange (sourceContextPrefixDisplay cc) (sourceContextPrefixDisplayError cc):xs

tokenizeIO ∷ (ToStream (SourceToken t) ts) ⇒ Parser t a → ts → IO [SourceToken a]
tokenizeIO p ss = case tokenize p ss of
  Left d → pprint d ≫ abortIO
  Right a → return a

tokenizeIOMain ∷ (ToStream (SourceToken t) ts,Pretty a) ⇒ Parser t a → ts → IO ()
tokenizeIOMain p ss = do
  x ← tokenizeIO p ss
  pprint $ ppVertical
    [ ppHeader "Success"
    , pretty x
    ]

----------------
-- Annotating --
----------------

data FullContextAnnotated t a = FullContextAnnotated
  { fullContextAnnotatedContext ∷ SourceContext t
  , fullContextAnnotatedValue ∷ a
  }

instance Functor (FullContextAnnotated t) where
  map f (FullContextAnnotated pc x) = FullContextAnnotated pc $ f x
instance Comonad (FullContextAnnotated t) where
  extract = fullContextAnnotatedValue
  FullContextAnnotated pc x =≫ f = FullContextAnnotated pc $
    f $ FullContextAnnotated pc x

-----------
-- Tests --
-----------

testParsingMultipleFailure ∷ IO ()
testParsingMultipleFailure = parseIOMain parser input
  where
    parser ∷ Parser ℂ 𝕊
    parser = mconcat
      [ pError "XXX*" $ mconcat
          [ pRender (FG pink) $ pWord "xxxy"
          , pRender (FG pink) $ pWord "xxxz"
          ]
      , pError "XXXZ" $ do
          x ← pError "XX" $ pRender (FG blue) $ pWord "xx"
          y ← pError "XZ" $ pRender (FG green) $ pWord "xz"
          return $ x ⧺ y
      , pError "XXZZ" $ pWord "xxzz"
      , pError "XXXAorB" $ pRender (FG teal) $ do
          x ← pWord "xxx"
          y ← single ^$ mconcat
            [ pLit 'a'
            , pLit 'b'
            ]
          return $ x ⧺ y
      ]
    input ∷ Stream (SourceToken ℂ)
    input = tokens "xxxx"

testParsingBlinders ∷ IO ()
testParsingBlinders = parseIOMain parser input
  where
    parser ∷ Parser ℂ [𝕊]
    parser = oneOrMore $ pError "Item" $ mconcat
      [ pWord "xxxx"
      , single ^$ pLit '\n'
      , pWord "xxxxxxxx\nxxxxxxxx"
      ]
    input ∷ Stream (SourceToken ℂ)
    input = tokens "xxxx\nxxxxxxxx\nxxxxxxxy\nxxxxxxxx\nxxxxxxxx"

testParsingAmbiguity ∷ IO ()
testParsingAmbiguity = parseIOMain parser input
  where
    parser = concat ^$ oneOrMore $ mconcat
      [ ppFG green ∘ ppText ∘ single ^$ pLit 'x'
      , ppFG blue ∘ ppText ^$ pWord "xx"
      ]
    input = tokens "xxx"

testParsingSuccess ∷ IO ()
testParsingSuccess = parseIOMain parser input
  where
    parser = concat ^$ oneOrMore $ mconcat [pRender (FG green) $ pWord "xx",pRender (FG blue) $ pWord "yy"]
    input = tokens "xxxxyyxxyy"

testParsingErrorNewline ∷ IO ()
testParsingErrorNewline = parseIOMain (𝕤 ^$ many $ pLit 'x') $ tokens "xxx\nx"
