module X86.AST where

import Data.Text
{-

https://sourceware.org/binutils/docs/as/
https://www.felixcloutier.com/x86/
https://www.eecg.utoronto.ca/~amza/www.mindsec.com/files/x86regs.html
https://wiki.cdot.senecacollege.ca/wiki/X86_64_Register_and_Instruction_Quick_Start


-}

type LabelId = Text

type X86Program = [Opcode]

data Opcode = Instr Instruction
            | Directive Text
            | Label LabelId
            | Comment Text
            | NotImplementedOpcode Text
            deriving Show

data OSize = T | Q | L | W | S | B deriving Show

data Instruction
    = Add OSize Operand Operand
    | Sub OSize Operand Operand
    | Call Text
    | Lea OSize Operand Operand
    | Mov OSize Operand Operand
    | Push OSize Operand
    | Pop OSize Operand
    | Ret OSize
    | NotImplementedMnemonic Text
    deriving Show

data Operand = Reg Register
             | Memory MemoryOperand
             | Immediate Literal
             deriving Show

data MemoryOperand = MemOp {
    segment :: Maybe Register
  , offset  :: Maybe Integer
  , base    :: Maybe Register
  , index   :: Maybe Register
  , scale   :: Maybe Integer
} deriving Show

data Literal = I Int | D Double | Str Text deriving Show

data Register =
            -- | General Purpose
                RAX | RBX | RCX | RDX -- 64
              | EAX | EBX | ECX | EDX -- 32
              | AX  | BX  | CX  | DX  -- 16
              | AH  | BH  | CH  | DH  -- 8
              | AL  | BL  | CL  | DL  -- 8
            -- | Segment
              | CS | DS | ES | FS | GS | SS
            -- Indexes and pointers
              | RBP | RSP | RSI | RDI
              | EPB | ESP | ESI | EDI
              | BP  | SP  | SI  | DI
            -- EFLAGS
              | EFLAGSReg EFLAGS
            -- TODO: R8-r15, Floating-Point and SIMD
              | NotImplementedReg Text
            deriving Show

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
            } deriving Show
