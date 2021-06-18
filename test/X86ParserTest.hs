module X86ParserTest where

import Protolude
import Data.Text hiding (index)
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
        -- COMMENTS (TODO: comments after other opcodes)
        it "should parse comment" $ do
            parse parseOpCode "" "# some comment" `shouldParse` (Comment "some comment")
        it "should parse comment (with spaces)" $ do
            parse parseOpCode "" "       # some comment" `shouldParse` (Comment "some comment")
        -- Registers
        for_ rEGS $ \reg ->
            it ("should parse " <> unpack reg) $ do
                parse parseRegister "" ("%"<>reg) `shouldParse` (read $ unpack $ Data.Text.toUpper reg)
        for_ rEGS $ \reg ->
            it ("should not parse register " <> unpack reg <> " without % prefix") $ do
                parse parseRegister "" `shouldFailOn` reg
        -- Memory addressing modes
        it "should parse memory addressing mode (everything present)" $ do
            parse parseMemoryOperand "" "%cs:-8(%ebp, %edx, 4)" `shouldParse` (MemOp (Just CS) (Just (-8)) (Just EBP) (Just EDX) (Just 4))
        it "should parse memory addressing mode (no segment)" $ do
            parse parseMemoryOperand "" "-2(%ebp, %edx, 8)" `shouldParse` (MemOp Nothing (Just (-2)) (Just EBP) (Just EDX) (Just 8))
        it "should parse memory addressing mode (segment + offset only)" $ do
            parse parseMemoryOperand "" "%ds:6" `shouldParse` (MemOp (Just DS) (Just 6) Nothing Nothing Nothing)
        it "should parse memory addressing mode (no segment and offset)" $ do
            parse parseMemoryOperand "" "(%esp, %ecx, 8)" `shouldParse` (MemOp Nothing Nothing (Just ESP) (Just ECX) (Just 8))
        it "should parse memory addressing mode (base register only)" $ do
            parse parseMemoryOperand "" "(%rcx)" `shouldParse` (MemOp Nothing Nothing (Just RCX) Nothing Nothing)
        it "should parse memory addressing mode (base register and offset)" $ do
            parse parseMemoryOperand "" "-13(%rcx)" `shouldParse` (MemOp Nothing (Just (-13)) (Just RCX) Nothing Nothing)
        it "should parse memory addressing mode (base and index regs)" $ do
            parse parseMemoryOperand "" "(%rcx, %rdx)" `shouldParse` (MemOp Nothing Nothing (Just RCX) (Just RDX) Nothing)
        it "should parse memory addressing mode (base, index and scale)" $ do
            parse parseMemoryOperand "" "(%rcx, %rdx, 2)" `shouldParse` (MemOp Nothing Nothing (Just RCX) (Just RDX) (Just 2))
        it "should parse memory addressing mode (base, index and offset)" $ do
            parse parseMemoryOperand "" "-5(%rcx, %rdx,)" `shouldParse` (MemOp Nothing (Just (-5)) (Just RCX) (Just RDX) Nothing)
        it "should parse memory addressing mode (index, offset and scale) - arithmetic" $ do
            parse parseMemoryOperand "" "9(,%eax,4)" `shouldParse` (MemOp Nothing (Just 9) Nothing (Just EAX) (Just 4))
        it "should parse memory addressing mode (index and scale) - arithmetic" $ do
            parse parseMemoryOperand "" "(,%eax,4)" `shouldParse` (MemOp Nothing Nothing Nothing (Just EAX) (Just 4))
        it "should parse memory addressing mode (base and index) - arithmetic" $ do
            parse parseMemoryOperand "" "(%ebx,%eax,)" `shouldParse` (MemOp Nothing Nothing (Just EBX) (Just EAX) Nothing)
        it "should parse memory addressing mode (offset and scale) - arithmetic" $ do
            parse parseMemoryOperand "" "9(,,4)" `shouldParse` (MemOp Nothing (Just 9) Nothing Nothing (Just 4))
        -- Literals
        it "should parse literal integer" $ do
            parse parseLiteral "" "$3" `shouldParse` (I 3)
        it "should parse literal integer negative" $ do
            parse parseLiteral "" "$-12" `shouldParse` (I (-12))
        it "should parse literal double" $ do
            parse parseLiteral "" "$4.5" `shouldParse` (D 4.5)
        it "should parse literal double negative" $ do
            parse parseLiteral "" "$-8.9" `shouldParse` (D (-8.9))
        it "should parse literal label" $ do
            parse parseLiteral "" "$labelll" `shouldParse` (Lbl "labelll")
        -- Instructons
        it "should parse 2 operand instruction #1" $ do
            parse parseInstruction "" "movl    $0, -4(%rbp)" `shouldParse`
                (Mov L (Immediate (I 0)) (Memory
                    (MemOp { segment = Nothing
                           , offset = Just (-4)
                           , base = Just RBP
                           , index = Nothing
                           , scale = Nothing})))
