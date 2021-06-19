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
            parse parseMemoryOperand "" "%cs:-8(%ebp, %edx, 4)" `shouldParse` (MemOp (Just CS) (Just (I (-8))) (Just EBP) (Just EDX) (Just 4))
        it "should parse memory addressing mode (no segment)" $ do
            parse parseMemoryOperand "" "-2(%ebp, %edx, 8)" `shouldParse` (MemOp Nothing (Just (I (-2))) (Just EBP) (Just EDX) (Just 8))
        it "should parse memory addressing mode (segment + offset only)" $ do
            parse parseMemoryOperand "" "%ds:6" `shouldParse` (MemOp (Just DS) (Just (I 6)) Nothing Nothing Nothing)
        it "should parse memory addressing mode (segment + offset only)" $ do
            parse parseMemoryOperand "" "%ds:6" `shouldParse` (MemOp (Just DS) (Just (I 6)) Nothing Nothing Nothing)
        it "should parse memory addressing mode (no segment and offset)" $ do
            parse parseMemoryOperand "" "(%esp, %ecx, 8)" `shouldParse` (MemOp Nothing Nothing (Just ESP) (Just ECX) (Just 8))
        it "should parse memory addressing mode (base register only)" $ do
            parse parseMemoryOperand "" "(%rcx)" `shouldParse` (MemOp Nothing Nothing (Just RCX) Nothing Nothing)
        it "should parse memory addressing mode (base register and offset)" $ do
            parse parseMemoryOperand "" "-13(%rcx)" `shouldParse` (MemOp Nothing (Just (I (-13))) (Just RCX) Nothing Nothing)
        it "should parse memory addressing mode (base and index regs)" $ do
            parse parseMemoryOperand "" "(%rcx, %rdx)" `shouldParse` (MemOp Nothing Nothing (Just RCX) (Just RDX) Nothing)
        it "should parse memory addressing mode (base, index and scale)" $ do
            parse parseMemoryOperand "" "(%rcx, %rdx, 2)" `shouldParse` (MemOp Nothing Nothing (Just RCX) (Just RDX) (Just 2))
        it "should parse memory addressing mode (base, index and offset)" $ do
            parse parseMemoryOperand "" "-5(%rcx, %rdx,)" `shouldParse` (MemOp Nothing (Just (I (-5))) (Just RCX) (Just RDX) Nothing)
        it "should parse memory addressing mode (index, offset and scale) - arithmetic" $ do
            parse parseMemoryOperand "" "9(,%eax,4)" `shouldParse` (MemOp Nothing (Just (I 9)) Nothing (Just EAX) (Just 4))
        it "should parse memory addressing mode (index and scale) - arithmetic" $ do
            parse parseMemoryOperand "" "(,%eax,4)" `shouldParse` (MemOp Nothing Nothing Nothing (Just EAX) (Just 4))
        it "should parse memory addressing mode (base and index) - arithmetic" $ do
            parse parseMemoryOperand "" "(%ebx,%eax,)" `shouldParse` (MemOp Nothing Nothing (Just EBX) (Just EAX) Nothing)
        it "should parse memory addressing mode (offset and scale) - arithmetic" $ do
            parse parseMemoryOperand "" "9(,,4)" `shouldParse` (MemOp Nothing (Just (I 9)) Nothing Nothing (Just 4))
        it "should parse memory addressing mode (offset and scale) - arithmetic - special case" $ do
            parse parseMemoryOperand "" "9(,4)" `shouldParse` (MemOp Nothing (Just (I 9)) Nothing Nothing (Just 4))
        it "should parse instruction pointer relative addressing mode with number offset" $ do
            parse parseMemoryOperand "" "1234(%rip)" `shouldParse` (MemOp Nothing (Just (I 1234)) (Just RIP) Nothing Nothing)
        it "should parse instruction pointer relative addressing mode with symbol" $ do
            parse parseMemoryOperand "" "var(%rip)" `shouldParse` (MemOp Nothing (Just (Lbl "var")) (Just RIP) Nothing Nothing)
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
        it "should parse 0 operand instruction #1" $ do
            parse parseInstruction "" "    retq" `shouldParse` (Ret Q)
        it "should parse 1 operand instruction #1" $ do
            parse parseInstruction "" " popq %rbp" `shouldParse` (Pop Q (Reg RBP))
        it "should parse 1 operand instruction #2" $ do
            parse parseInstruction "" "   pushq %rbp" `shouldParse` (Push Q (Reg RBP))
        it "should parse 2 operand instruction #1" $ do
            parse parseInstruction "" "movl    $0, -4(%rbp)" `shouldParse`
                (Mov L (Immediate (I 0)) (Memory
                    (MemOp { segment = Nothing
                           , disp = Just (I (-4))
                           , base = Just RBP
                           , index = Nothing
                           , scale = Nothing})))
        it "should parse 2 operand instruction #2" $ do
            parse parseInstruction "" "movq  %rsp, %rbp" `shouldParse` (Mov Q (Reg RSP) (Reg RBP))
        it "should parse 2 operand instruction #3" $ do
            parse parseInstruction "" " movb    $3, %al" `shouldParse` (Mov B (Immediate (I 3)) (Reg AL))
        it "should parse 2 operand instruction with rip relative addressing" $ do
            parse parseInstruction "" "  leaq  L_.str(%rip), %rdi" `shouldParse`
                (Lea Q (Memory
                        $ MemOp { segment = Nothing
                                , disp    = Just (Lbl "L_.str")
                                , base    = Just RIP
                                , index   = Nothing
                                , scale   = Nothing})
                       (Reg RDI))
        it "should parse 2 operand instruction with two registers" $ do
            parse parseInstruction "" " xorl   %eax, %eax" `shouldParse` (Xor L (Reg EAX) (Reg EAX))
        it "should parse 2 operand instruction with literal register operands" $ do
            parse parseInstruction "" "  addq    $16, %rsp" `shouldParse` (Add Q (Immediate (I 16)) (Reg RSP))
