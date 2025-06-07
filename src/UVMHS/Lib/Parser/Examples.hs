module UVMHS.Lib.Parser.Examples where

import UVMHS.Core
import UVMHS.Lib.Parser.Parser
import UVMHS.Lib.Parser.ParserInput
import UVMHS.Lib.Parser.RawParser
import UVMHS.Lib.Parser.Regex
import UVMHS.Lib.Pretty

testParsingSmall ∷ IO ()
testParsingSmall = parseIOMain parser "<small example>" input
  where
    parser = pWord $ 𝕤 "xyzxyz"
    input = tokens "xyzxycxyz"

testParsingMultiline ∷ IO ()
testParsingMultiline = parseIOMain parser "<multiline example>" input
  where
    parser = exec $ inbetween (pWord $ 𝕤 "\n") $ list $ replicateI (𝕟 7) $ \ n → pNewContext "line" $ pWord ("xyz" ⧺ show𝕊 n)
    input = tokens "xyz0\nxyz1\nxyz2\nxyc3\nxyz4\nxyz5\nxyz6\n"

testParsingBranching ∷ IO ()
testParsingBranching = parseIOMain parser "<branching example>" input
  where
    parser ∷ Parser ℂ 𝕊
    parser = tries
      [ pNewContext "XXX*" $ tries
          [ pRender (formats [FG pink]) $ pWordRet "xxxy"
          , pRender (formats [FG pink]) $ pWordRet "xxxz"
          ]
      , pNewContext "XXXZ" $ do
          x ← pErr "XX" $ pRender (formats [FG blue]) $ pWordRet "xx"
          y ← pErr "XZ" $ pRender (formats [FG green]) $ pWordRet "xz"
          return $ x ⧺ y
      , pNewContext "XXZZ" $ pWordRet "xxzz"
      , pNewContext "XXXAorB" $ pRender (formats [FG teal]) $ do
          x ← pWordRet "xxx"
          y ← single ^$ tries
            [ pTokRet 'a'
            , pTokRet 'b'
            ]
          return $ x ⧺ y
      ]
    input ∷ 𝕍 (ParserToken ℂ)
    input = tokens "xxxx"

-- testParsingAmbiguity ∷ IO ()
-- testParsingAmbiguity = parseIOMain parser input
--   where
--     parser = concat ^$ pOneOrMore $ tries
--       [ ppFG yellow ∘ ppString ∘ single ^$ pTok 'y'
--       , ppFG green ∘ ppString ∘ single ^$ pTok 'x'
--       , ppFG blue ∘ ppString ^$ pWord "xx"
--       ]
--     input = tokens "xxx"

testParsingGreedy ∷ IO ()
testParsingGreedy = parseIOMain parser "<greedy example>" input
  where
    parser = concat ^$ pOneOrMore $ tries
      [ ppFG yellow ∘ ppString ∘ single ^$ pRender (formats [FG yellow]) $ rawToParser $ rpToken 'y'
      , ppFG green ∘ ppString ∘ single ^$ pRender (formats [FG green]) $ rawToParser $ rpToken 'x'
      , ppFG blue ∘ ppString ^$ pRender (formats [FG yellow]) $ pWordRet "xx"
      ]
    input = tokens "xxx"

testParsingGreedyAmbiguity ∷ IO ()
testParsingGreedyAmbiguity = parseIOMain parser "<greedy ambiguity example>" input
  where
    parser = concat ^$ pOneOrMore $ tries
      [ ppFG yellow ∘ ppString ∘ single ^$ pRender (formats [FG yellow]) $ rawToParser $ rpToken 'y'
      , tries
          [ ppFG blue ∘ ppString ^$ pRender (formats [FG blue]) $ pWordRet "x"
          , ppFG pink ∘ ppString ^$ pRender (formats [FG pink]) $ pWordRet "xx"
          ]
      , ppFG green ∘ ppString ∘ single ^$ pRender (formats [FG green]) $ rawToParser $ rpToken 'x'
      ]
    input = tokens "xxx"

testParsingSuccess ∷ IO ()
testParsingSuccess = parseIOMain parser "<success example>" input
  where
    parser = concat ^$ pOneOrMore $ tries
      [ pRender (formats [FG green]) $ pWord $ 𝕤 "xx"
      , pRender (formats [FG blue]) $ pWord $ 𝕤 "yy"
      ]
    input = tokens "xxxxyyxxyy"

testParsingErrorNewline ∷ IO ()
testParsingErrorNewline = parseIOMain (string ^$ pMany $ rawToParser $ rpToken 'x') "<error newline example>" $ tokens "xxx\nx"

testParsingErrorEof ∷ IO ()
testParsingErrorEof = parseIOMain (exec $ replicate (𝕟 3) $ pTok 'x') "<error eof example>" $ tokens "xx"

testTokenizeSimple ∷ IO ()
testTokenizeSimple =
  let rgx = lWord "x" ▷ oepsRegex ()
      dfa = compileRegex rgx
  in tokenizeIOMain (Lexer (const dfa) (const ∘ ((:*) False) ∘ string) ()) "<tokenize simple example>" $ tokens "xxx"

testTokenize ∷ IO ()
testTokenize =
  let rgx = concat [lWord "x",lWord "xy",lWord "y"] ▷ oepsRegex ()
      dfa = compileRegex rgx
  in tokenizeIOMain (Lexer (const dfa) (const ∘ ((:*) False) ∘ string) ()) "<tokenize example>" $ tokens "xxyxyxyxyxxyy"

testTokenizeFailure1 ∷ IO ()
testTokenizeFailure1 =
  let rgx = concat
        [ lWord "x" ▷ fepsRegex (formats [FG green]) ▷ lepsRegex (𝕟64 2)
        , lWord "x" ▷ fepsRegex (formats [FG yellow]) ▷ lepsRegex (𝕟64 1)
        , lWord "xx" ▷ fepsRegex (formats [FG blue])
        , lWord "xy" ▷ fepsRegex (formats [FG teal])
        , lWord "xz" ▷ fepsRegex (formats [FG pink])
        ] ▷ oepsRegex ()
      dfa = compileRegex rgx
  in tokenizeIOMain (Lexer (const dfa) (const ∘ ((:*) False) ∘ string) ()) "<tokenize failure1 example>" $ tokens "xxxxy"

testTokenizeFailure2 ∷ IO ()
testTokenizeFailure2 =
  let rgx = concat
        [ lWord "x" ▷ fepsRegex (formats [FG green]) ▷ lepsRegex (𝕟64 2)
        , lWord "x" ▷ fepsRegex (formats [FG yellow]) ▷ lepsRegex (𝕟64 1)
        , lWord "xx" ▷ fepsRegex (formats [FG blue])
        , lWord "xy" ▷ fepsRegex (formats [FG teal])
        , lWord "xz" ▷ fepsRegex (formats [FG pink])
        ] ▷ oepsRegex ()
      dfa = compileRegex rgx
  in tokenizeIOMain (Lexer (const dfa) (const ∘ ((:*) False) ∘ string) ()) "<tokenize failiure2 example>" $ tokens "xxxyxxxzxc"
