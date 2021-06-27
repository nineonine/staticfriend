module X86.AST where

import Protolude
{-

https://sourceware.org/binutils/docs/as/
https://www.felixcloutier.com/x86/
https://www.eecg.utoronto.ca/~amza/www.mindsec.com/files/x86regs.html
https://wiki.cdot.senecacollege.ca/wiki/X86_64_Register_and_Instruction_Quick_Start


-}

type SymbolId = Text

type X86Program = [Instruction]

data Instruction
    = Op Operation
    | Directive Text [Text]
    | Label SymbolId
    | Comment Text
    | NotImplementedOpcode Text
    deriving (Show, Eq)

data OSize = T | Q | L | W | S | B deriving (Show, Eq)

readOSize :: Char -> OSize
readOSize 't' = T
readOSize 'q' = Q
readOSize 'l' = L
readOSize 'w' = W
readOSize 'b' = B
readOSize 's' = S
readOSize _   = panic "readOSize"

data Operation
    = Add OSize Operand Operand
    | Sub OSize Operand Operand
    | Call OSize Operand
    | Lea OSize Operand Operand
    | Mov OSize Operand Operand
    | Push OSize Operand
    | Pop OSize Operand
    | Ret OSize
    | Xor OSize Operand Operand
    | NotImplementedMnemonic Text
    deriving (Show, Eq)

numOfOperands :: Text -> Int
numOfOperands mnem
    | elem mnem [ "ret" ]
    = 0
    | elem mnem [ "push", "pop", "call" ]
    = 1
    | elem mnem [ "add", "sub", "lea", "mov", "xor" ]
    = 2
    | otherwise
    = panic "numOfOperands: not implemented"

mkOp_os0 :: Text -> (OSize -> Operation)
mkOp_os0 = \case
    "ret" -> Ret
    _else -> panic ("mkOp_os0: " <> _else)

mkOp_os1 :: Text -> (OSize -> Operand -> Operation)
mkOp_os1 = \case
    "push" -> Push
    "pop"  -> Pop
    "call" -> Call
    _else -> panic ("mkOp_os1: " <> _else)

mkOp_os2 :: Text -> (OSize -> Operand -> Operand -> Operation)
mkOp_os2 = \case
    "add" -> Add
    "sub" -> Sub
    "lea" -> Lea
    "mov" -> Mov
    "xor" -> Xor
    _else -> panic ("mkOp_os2: " <> _else)

data Operand = Reg Register
             | Memory MemoryOperand
             | Immediate Literal
             deriving (Show, Eq)

data MemoryOperand = MemOp {
    segment :: Maybe Register
  , disp    :: Maybe Literal
  , base    :: Maybe Register
  , index   :: Maybe Register
  , scale   :: Maybe Integer
} deriving (Show, Eq)

data Literal = I Int | D Double | Lbl SymbolId deriving (Show, Eq)

data Register =
            -- | General Purpose
                RAX | RBX | RCX | RDX -- 64
              | EAX | EBX | ECX | EDX -- 32
              | AX  | BX  | CX  | DX  -- 16
              | AH  | BH  | CH  | DH  -- 8
              | AL  | BL  | CL  | DL  -- 8
            -- instruction pointer
              | RIP | IP
            -- | Segment
              | CS | DS | ES | FS | GS | SS
            -- Indexes and pointers
              | RBP | RSP | RSI | RDI
              | EBP | ESP | ESI | EDI
              | BP  | SP  | SI  | DI
            -- EFLAGS
              | EFLAGSReg EFLAGS
            -- TODO: R8-r15, Floating-Point and SIMD
              | NotImplementedReg Text
            deriving (Show, Eq, Read)

data EFLAGS = EFLAGS
            { _CF :: Bool
            , _PF :: Bool
            , _AF :: Bool
            , _ZF :: Bool
            , _SF :: Bool
            , _TF :: Bool
            , _IF :: Bool
            , _DF :: Bool
            , _OF :: Bool
            , _IOPL :: Bool
            , _NT :: Bool
            , _RF :: Bool
            , _VM :: Bool
            , _AC :: Bool
            , _VIF :: Bool
            , _VIP :: Bool
            , _ID :: Bool
            } deriving (Show, Eq, Read)
