module X86ParserTest where

import Prelude
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

import X86.AST
import X86.Parser

testx86Parser :: IO ()
testx86Parser = hspec $ do
    describe "X86 Parser" $ do
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
        it "should fail on anything else" $ do
            parse parseOSize "" `shouldFailOn` "r"
