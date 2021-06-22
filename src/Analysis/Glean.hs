module Analysis.Glean where

import Protolude
import Data.List (union)
import qualified Data.IntMap.Strict as IntMap

import Analysis.InfoItem
import X86.AST

gleanInfoItems :: X86Program -> IntMap.IntMap [InfoItemLabel]
gleanInfoItems prog = go IntMap.empty (zip [1..] prog)
    where
    go acc [] = acc
    go acc ((i,opcode):rest)
        | items <- gleanFromOpcode opcode
        = go (IntMap.insert i items acc) rest

gleanFromOpcode :: Opcode -> [InfoItemLabel]
gleanFromOpcode (Instr i) = [GeneralInfo] <> gleanInstr i
gleanFromOpcode (Directive _ _) = [Directives]
gleanFromOpcode (Label _) = [Labels]
gleanFromOpcode (Comment _) = [Comments]
gleanFromOpcode (NotImplementedOpcode _) = []

registerInfoItems :: [InfoItemLabel]
registerInfoItems = [OperationSuffixes, OperandPrefixes]

gleanInstr :: Instruction -> [InfoItemLabel]
gleanInstr (Add os op1 op2)
    = registerInfoItems <> union (gleanOperand op1) (gleanOperand op2)
gleanInstr (Sub os op1 op2)
    = registerInfoItems <> union (gleanOperand op1) (gleanOperand op2)
gleanInstr (Call os op1)
    = registerInfoItems <> gleanOperand op1
gleanInstr (Lea os op1 op2)
    = registerInfoItems <> union (gleanOperand op1) (gleanOperand op2)
gleanInstr (Mov os op1 op2)
    = registerInfoItems <> union (gleanOperand op1) (gleanOperand op2)
gleanInstr (Push os op1)
    = registerInfoItems <> gleanOperand op1
gleanInstr (Pop os op1)
    = registerInfoItems <> gleanOperand op1
gleanInstr (Xor os op1 op2)
    = registerInfoItems <> union (gleanOperand op1) (gleanOperand op2)
gleanInstr (Ret os) = [OperationSuffixes]
gleanInstr (NotImplementedMnemonic _) = []

gleanOperand :: Operand -> [InfoItemLabel]
gleanOperand (Reg r) = [Registers]
gleanOperand (Memory memOp) = [AddressOperandSyntax]
gleanOperand (Immediate lit) = []
