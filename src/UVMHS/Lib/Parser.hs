module UVMHS.Lib.Parser
  -- * UTILITY TYPES
  ( Loc(..),LocRange(..)
  , ParserContext(..)
  , SrcCxt(..),srcCxt₀
  , ParserErrorInfo(..)
  , ParserError(..)
  , PreParserToken(..)
  , ParserToken(..),preTokens,finalizeTokens,tokens,renderParserTokens
  -- * LEXER API
  -- ** REGEX
  , CharClass(..)
  , Regex(..)
  , nullRegex
  , resRegex,epsRegex,retRegex,outRegex,lepsRegex,fepsRegex,oepsRegex,uepsRegex
  , atomRegex,tokRegex,ntokRegex,classRegex
  , sumRegex,seqRegex,starRegex
  , DFA(..),compileRegex
  -- ** LEXERS
  , Lexer(..)
  , tokenize,tokenizeIO,tokenizeIOMain
  , tokenizeWSAnchored,tokenizeWSAnchoredIO,tokenizeWSAnchoredIOMain
  , tokenizeWSUnanchored,tokenizeWSUnanchoredIO,tokenizeWSUnanchoredIOMain
  -- ** DEFAULT LANGUAGE ABSTRACTION
  , TokenClassWSBasic(..)
  , TokenWSBasic(..)
  , LexerWSBasicSyntax(..),lexerWSBasic
  , lexerWSBasicSyntaxPunsMk,lexerWSBasicSyntaxKeysMk,lexerWSBasicSyntaxPrmsMk
  , lexerWSBasicSyntaxOprsMk,lexerWSBasicSyntaxBlocksMk
  -- ** BLOCKIFY
  , blockifyTokensWSBasicAnchored,blockifyTokensWSBasicUnanchored
  -- * PARSER API
  , Parser(..)
  , pRender,pErr,pFinal,pTok,pTokAny,pTokRet,pTokShaped,pTokSat,pAny,pWord,pWordRet
  , pOptional,pMany,pOneOrMore,pManySepBy,pOneOrMoreSepBy
  , pDie,pGuard,pFailEff
  , pTokName,pTokSyntax,pTokSyntaxAny,pTokNatN,pTokNatN64,pTokInt,pTokInt64,pTokNat,pTokNat64
  , pTokDouble,pTokString,pTokChar,pTokBlock,pTokOpen,pTokClose,pTokDelim
  , pNewExpressionContext,pNewErrContext,pNewContext,pWithContextRendered,pNewWithContextRendered,pGetContextRendered
  , pManyContext,pOneOrMoreContext,pManySepByContext,pOneOrMoreSepByContext
  , runParser₀,parse,parseIO,parseIOMain
  , lexAndParse,lexAndParseIO,lexAndParseIOMain
  , lexAndParseAnchored,lexAndParseAnchoredIO,lexAndParseAnchoredIOMain
  , lexAndParseUnanchored,lexAndParseUnanchoredIO,lexAndParseUnanchoredIOMain
  -- * MIXFIX API
  , MixfixF(..)
  , onlyTerminalsF
  , fmixPrefix,fmixPostfix,fmixInfix,fmixInfixL,fmixInfixR,fmixTerminal
  , fmixfix,fmixfixWithContext,fmixfixWithContextSet
  ) where

import UVMHS.Lib.Parser.Loc
import UVMHS.Lib.Parser.Mixfix
import UVMHS.Lib.Parser.Parser
import UVMHS.Lib.Parser.ParserContext
import UVMHS.Lib.Parser.ParserError
import UVMHS.Lib.Parser.ParserInput
import UVMHS.Lib.Parser.Regex
