module X86ParserTest where

import Prelude
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec hiding (Label)
import Text.Megaparsec.Char

import X86.AST
import X86.Parser

testx86Parser :: IO ()
testx86Parser = hspec $ do
    describe "X86 Parser" $ do
        -- SIZE SUFFIX
        it "returns byte (8b) size" $ do
            parse parseOSize "" "b" `shouldParse` (B :: OSize)
        it "returns single (32b float) size" $ do
            parse parseOSize "" "s" `shouldParse` (S :: OSize)
        it "returns word (16b) size" $ do
            parse parseOSize "" "w" `shouldParse` (W :: OSize)
        it "returns long (32b integer or 64 float) size" $ do
            parse parseOSize "" "l" `shouldParse` (L :: OSize)
        it "returns quad (64b) size" $ do
            parse parseOSize "" "q" `shouldParse` (Q :: OSize)
        it "returns ten bytes (80b float) size" $ do
            parse parseOSize "" "t" `shouldParse` (T :: OSize)
        it "should fail on something else" $ do
            parse parseOSize "" `shouldFailOn` "r"
        -- DIRECTIVES
        it "should parse no arg directive" $ do
            parse parseOpCode "" ".section" `shouldParse` (Directive "section" [])
        it "should parse no arg directive (with spaces 1)" $ do
            parse parseOpCode "" "       .section" `shouldParse` (Directive "section" [])
        it "should parse no arg directive (with spaces 2)" $ do
            parse parseOpCode "" "       .section    \t\n" `shouldParse` (Directive "section" [])
        it "should parse no arg directive (with spaces 3)" $ do
            parse parseOpCode "" "   \t\t\t    .section    \t\n" `shouldParse` (Directive "section" [])
        it "should parse directive with 1 arg" $ do
            parse parseOpCode "" "  .globl __main" `shouldParse` (Directive "globl" ["__main"])
        it "should parse directive with 1 arg (wih spaces 1)" $ do
            parse parseOpCode "" "  .globl \t   __main   \t" `shouldParse` (Directive "globl" ["__main"])
        it "should parse directive with 1 arg (wih spaces 2)" $ do
            parse parseOpCode "" ".globl \t\t\t   __main  \n \t" `shouldParse` (Directive "globl" ["__main"])
        it "should parse directive with many args" $ do
            parse parseOpCode "" ".section __TEXT,__cstring,cstring_literals" `shouldParse` (Directive "section" ["__TEXT","__cstring","cstring_literals"])
        it "should parse directive with many args (with spaces 1)" $ do
            parse parseOpCode "" "     \t\t .section __TEXT,   __cstring,  cstring_literals" `shouldParse` (Directive "section" ["__TEXT","__cstring","cstring_literals"])
        it "should parse directive with many args (with spaces 1)" $ do
            parse parseOpCode "" "     \t\t .section __TEXT  , __cstring ,  cstring_literals" `shouldParse` (Directive "section" ["__TEXT","__cstring","cstring_literals"])
        it "should not parse something as directive if it doesn't start with a dot" $ do
            parse parseDirective "" `shouldFailOn` "   section"
        -- LABELS
        it "should parse label" $ do
            parse parseOpCode "" "someLabel:" `shouldParse` (Label "someLabel")
        it "should parse label (with spaces)" $ do
            parse parseOpCode "" "    someLabel:   " `shouldParse` (Label "someLabel")
        it "should parse label 2" $ do
            parse parseOpCode "" "L_.str:" `shouldParse` (Label "L_.str")
        -- COMMENTS (TODO: comments after other)
        it "should parse comment" $ do
            parse parseOpCode "" "# some comment" `shouldParse` (Comment "some comment")
        it "should parse comment (with spaces)" $ do
            parse parseOpCode "" "       # some comment" `shouldParse` (Comment "some comment")
