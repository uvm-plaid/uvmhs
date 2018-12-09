module UVMHS.Lib.Parser.ParserContext where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Parser.Loc
import UVMHS.Lib.Parser.Sep

-- # ParserContextDoc

data ParserContextMode = ParserContextDisplay | ParserContextError
  deriving (Eq,Ord)

newtype  ParserContextDoc = ParserContextDoc { runParserContextDoc ∷ RWS ParserContextMode Doc () () }
  deriving (Null,Append,Monoid)
instance Pretty ParserContextDoc where pretty = execParserContextDoc

onParserContextDoc ∷ (RWS ParserContextMode Doc () () → RWS ParserContextMode Doc () ()) → ParserContextDoc → ParserContextDoc
onParserContextDoc f = ParserContextDoc ∘ f ∘ runParserContextDoc

execParserContextDoc ∷ ParserContextDoc → Doc
execParserContextDoc = evalRWS ParserContextDisplay () ∘ retOut ∘ runParserContextDoc

parserContextError ∷ ParserContextDoc → ParserContextDoc
parserContextError = onParserContextDoc $ local ParserContextError

data ParserContextDocCached = ParserContextDocCached
  { parserContextDocCachedDoc ∷ ParserContextDoc
  , parserContextDocCachedBytes ∷ 𝑄 OutputElemNF
  }
instance Eq ParserContextDocCached where (==) = (≡) `on` parserContextDocCachedBytes
instance Ord ParserContextDocCached where compare = (⋚) `on` parserContextDocCachedBytes
instance Null ParserContextDocCached where null = mkParserContextDocCached null
instance Append ParserContextDocCached where 
  ParserContextDocCached d₁ b₁ ⧺ ParserContextDocCached d₂ b₂ = ParserContextDocCached (d₁ ⧺ d₂) (b₁ ⧺ b₂)
instance Monoid ParserContextDocCached
instance Pretty ParserContextDocCached where pretty = pretty ∘ parserContextDocCachedDoc

mkParserContextDocCached ∷ ParserContextDoc → ParserContextDocCached
mkParserContextDocCached doc = ParserContextDocCached doc (prettyNFDoc $ execParserContextDoc doc)

onParserContextDocCached ∷ (RWS ParserContextMode Doc () () → RWS ParserContextMode Doc () ()) → ParserContextDocCached → ParserContextDocCached
onParserContextDocCached f = mkParserContextDocCached ∘ onParserContextDoc f ∘ parserContextDocCachedDoc

-- # ParserContextLines

data ParserContextChunk = ParserContextChunk
  { parserContextChunkLocRange ∷ AddBot LocRange
  , parserContextChunkNewlines ∷ ℕ
  , parserContextChunkDoc ∷ ParserContextDocCached
  }
  -- deriving (Eq,Ord)
  deriving (Eq,Ord)
makeLenses ''ParserContextChunk
makePrettySum ''ParserContextChunk

instance Null ParserContextChunk where null = ParserContextChunk bot zero null
instance Append ParserContextChunk where 
  ParserContextChunk r₁ n₁ d₁ ⧺ ParserContextChunk r₂ n₂ d₂ = ParserContextChunk (r₁ ⊔ r₂) (n₁ + n₂) (d₁ ⧺ d₂) 
instance Monoid ParserContextChunk

type ContextLines = SepR ParserContextChunk ParserContextChunk

-- # ParserContext

data ParserContext = ParserContext
  { parserContextLocRange ∷ AddBot LocRange
  , parserContextNewlines ∷ ℕ
  , parserContextLines ∷ ContextLines
  }
  -- deriving (Eq,Ord)
  deriving (Eq,Ord)
makeLenses ''ParserContext
makePrettySum ''ParserContext

instance Null ParserContext where null = parserContextFromLines null
instance Append ParserContext where ParserContext l₁ n₁ s₁ ⧺ ParserContext l₂ n₂ s₂ = ParserContext (l₁ ⊔ l₂) (n₁ + n₂) (s₁ ⧺ s₂)
instance Monoid ParserContext

onParserContext ∷ (RWS ParserContextMode Doc () () → RWS ParserContextMode Doc () ()) → ParserContext → ParserContext
onParserContext = alter parserContextLinesL ∘ map ∘ alter parserContextChunkDocL ∘ onParserContextDocCached

execParserContext ∷ ParserContext → ParserContextDocCached
execParserContext = concat ∘ map parserContextChunkDoc ∘ iter ∘ parserContextLines

parserContextFromLines ∷ ContextLines → ParserContext
parserContextFromLines pcl₀ = let (lr,n) = parserContextLinesMeta pcl₀ in ParserContext lr n pcl₀
  where
    parserContextLinesMeta pcl = 
      ( joins $ map parserContextChunkLocRange $ iter pcl
      , sum $ map parserContextChunkNewlines $ iter pcl
      )

truncateParserContext ∷ ℕ → ParserContext → ParserContext
truncateParserContext n (ParserContext _lr _n l) = parserContextFromLines $ lastNSepR n l

newtype InputContext = InputContext { runInputContext ∷ ParserContext }
  --deriving ({-Eq,Ord,-}Null,Append,Monoid)
  deriving (Eq,Ord,Null,Append,Monoid)
makePrettySum ''InputContext
newtype ExpressionContext = ExpressionContext { runExpressionContext ∷ ParserContext }
  --deriving ({-Eq,Ord,-}Null,Append,Monoid)
  deriving (Eq,Ord,Null,Append,Monoid)
makePrettySum ''ExpressionContext
