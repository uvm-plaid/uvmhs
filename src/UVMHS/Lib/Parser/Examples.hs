module UVMHS.Lib.Parser.Examples where

import UVMHS.Core
import UVMHS.Lib.Pretty
import UVMHS.Lib.Parser.Core
import UVMHS.Lib.Parser.ParserInput
import UVMHS.Lib.Parser.Regex
import UVMHS.Lib.Parser.CParser

testParsingSmall ∷ IO ()
testParsingSmall = parseIOMain parser "<small example>" input
  where
    parser = cpWord $ 𝕤 "xyzxyz"
    input = tokens "xyzxycxyz"

testParsingMultiline ∷ IO ()
testParsingMultiline = parseIOMain parser "<multiline example>" input
  where
    parser = exec $ inbetween (void $ cpWord $ 𝕤 "\n") $ list $ replicateI (𝕟 7) $ \ n → cpNewContext "line" $ void $ cpWord ("xyz" ⧺ show𝕊 n)
    input = tokens "xyz0\nxyz1\nxyz2\nxyc3\nxyz4\nxyz5\nxyz6\n"

testParsingBranching ∷ IO ()
testParsingBranching = parseIOMain parser "<branching example>" input
  where
    parser ∷ CParser ℂ 𝕊
    parser = tries
      [ cpNewContext "XXX*" $ tries
          [ cpRender (formats [FG pink]) $ cpWord "xxxy"
          , cpRender (formats [FG pink]) $ cpWord "xxxz"
          ]
      , cpNewContext "XXXZ" $ do
          x ← cpErr "XX" $ cpRender (formats [FG blue]) $ cpWord "xx"
          y ← cpErr "XZ" $ cpRender (formats [FG green]) $ cpWord "xz"
          return $ x ⧺ y
      , cpNewContext "XXZZ" $ cpWord "xxzz"
      , cpNewContext "XXXAorB" $ cpRender (formats [FG teal]) $ do
          x ← cpWord "xxx"
          y ← single ^$ tries
            [ cpToken 'a'
            , cpToken 'b'
            ]
          return $ x ⧺ y
      ]
    input ∷ 𝕍 (ParserToken ℂ)
    input = tokens "xxxx"

-- testParsingAmbiguity ∷ IO ()
-- testParsingAmbiguity = parseIOMain parser input
--   where
--     parser = concat ^$ pOneOrMore $ tries
--       [ ppFG yellow ∘ ppString ∘ single ^$ pToken 'y'
--       , ppFG green ∘ ppString ∘ single ^$ pToken 'x'
--       , ppFG blue ∘ ppString ^$ pWord "xx"
--       ]
--     input = tokens "xxx"

testParsingGreedy ∷ IO ()
testParsingGreedy = parseIOMain parser "<greedy example>" input
  where
    parser = concat ^$ cpOneOrMore $ tries
      [ ppFG yellow ∘ ppString ∘ single ^$ cpRender (formats [FG yellow]) $ toCParser $ rpToken 'y'
      , ppFG green ∘ ppString ∘ single ^$ cpRender (formats [FG green]) $ toCParser $ rpToken 'x'
      , ppFG blue ∘ ppString ^$ cpRender (formats [FG yellow]) $ cpWord "xx"
      ]
    input = tokens "xxx"

testParsingGreedyAmbiguity ∷ IO ()
testParsingGreedyAmbiguity = parseIOMain parser "<greedy ambiguity example>" input
  where
    parser = concat ^$ cpOneOrMore $ tries
      [ ppFG yellow ∘ ppString ∘ single ^$ cpRender (formats [FG yellow]) $ toCParser $ rpToken 'y'
      , tries
          [ ppFG blue ∘ ppString ^$ cpRender (formats [FG blue]) $ cpWord "x"
          , ppFG pink ∘ ppString ^$ cpRender (formats [FG pink]) $ cpWord "xx"
          ]
      , ppFG green ∘ ppString ∘ single ^$ cpRender (formats [FG green]) $ toCParser $ rpToken 'x'
      ]
    input = tokens "xxx"

testParsingSuccess ∷ IO ()
testParsingSuccess = parseIOMain parser "<success example>" input
  where
    parser = concat ^$ cpOneOrMore $ tries
      [ cpRender (formats [FG green]) $ cpWord $ 𝕤 "xx"
      , cpRender (formats [FG blue]) $ cpWord $ 𝕤 "yy"
      ]
    input = tokens "xxxxyyxxyy"

testParsingErrorNewline ∷ IO ()
testParsingErrorNewline = parseIOMain (string ^$ cpMany $ toCParser $ rpToken 'x') "<error newline example>" $ tokens "xxx\nx"

testParsingErrorEof ∷ IO ()
testParsingErrorEof = parseIOMain (exec $ replicate (𝕟 3) $ void $ cpToken 'x') "<error eof example>" $ tokens "xx"

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
