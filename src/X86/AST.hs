module X86.AST where

import Protolude
import Data.Text
{-

https://sourceware.org/binutils/docs/as/
https://www.felixcloutier.com/x86/
https://www.eecg.utoronto.ca/~amza/www.mindsec.com/files/x86regs.html
https://wiki.cdot.senecacollege.ca/wiki/X86_64_Register_and_Instruction_Quick_Start


-}

type LabelId = Text
type Function = Text

type X86Program = [Opcode]

data Opcode = Instr Instruction
            | Directive Text Text
            | Label LabelId
            | Comment Text
            | NotImplementedOpcode Text
            deriving Show

data OSize = T | Q | L | W | S | B deriving Show

readOSize :: Char -> OSize
readOSize 't' = T
readOSize 'q' = Q
readOSize 'l' = L
readOSize 'W' = W
readOSize 'B' = B

data Instruction
    = Add OSize Operand Operand
    | Sub OSize Operand Operand
    | Call Function
    | Lea OSize Operand Operand
    | Mov OSize Operand Operand
    | Push OSize Operand
    | Pop OSize Operand
    | Ret OSize
    | NotImplementedMnemonic Text
    deriving Show

numOfOperands :: Text -> Int
numOfOperands mnem
    | elem mnem [ "ret" ]
    = 0
    | elem mnem [ "push", "pop", "call" ]
    = 1
    | elem mnem [ "add", "sub", "lea", "mov" ]
    = 2
    | otherwise
    = panic "numOfOperands: not implemented"

mkCallInstr :: Function -> Instruction
mkCallInstr = Call

mkINstr_os0 :: Text -> (OSize -> Instruction)
mkINstr_os0 = \case
    "ret" -> Ret
    _else -> panic ("mkINstr_os0: " <> _else)

mkINstr_os1 :: Text -> (OSize -> Operand -> Instruction)
mkINstr_os1 = \case
    "push" -> Push
    "pop"  -> Pop
    _else -> panic ("mkINstr_os1: " <> _else)

mkINstr_os2 :: Text -> (OSize -> Operand -> Operand -> Instruction)
mkINstr_os2 = \case
    "add" -> Add
    "sub" -> Sub
    "lea" -> Lea
    "mov" -> Mov
    _else -> panic ("mkINstr_os2: " <> _else)

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
