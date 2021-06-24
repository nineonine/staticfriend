module Analysis.Glean where

import Protolude
import Data.List (union)
import qualified Data.Map.Strict as M

import Analysis.InfoItem
import X86.AST

gleanInfoItems :: X86Program -> InfoItemLookup -> [[InfoItem]]
gleanInfoItems prog infoItemLookup = map glean prog where
    glean opcode =
        map (\lbl ->
                InfoItem lbl (fromMaybe (panic "infoItemLookup")
                                $ M.lookup lbl infoItemLookup)
            ) (gleanFromOpcode opcode)

gleanFromOpcode :: Opcode -> [InfoItemLabel]
gleanFromOpcode (Instr i) = [GeneralInfo] <> gleanInstr i
gleanFromOpcode (Directive _ _) = [Directives]
gleanFromOpcode (Label _) = [Labels]
gleanFromOpcode (Comment _) = [Comments]
gleanFromOpcode (NotImplementedOpcode _) = []

instrInfoItems :: [InfoItemLabel]
instrInfoItems = [OperationSuffixes, OperandPrefixes]

gleanInstr :: Instruction -> [InfoItemLabel]
gleanInstr (Add os op1 op2)
    = instrInfoItems <> union (gleanOperand op1) (gleanOperand op2)
gleanInstr (Sub os op1 op2)
    = instrInfoItems <> union (gleanOperand op1) (gleanOperand op2)
gleanInstr (Call os op1)
    = instrInfoItems <> gleanOperand op1
gleanInstr (Lea os op1 op2)
    = instrInfoItems <> union (gleanOperand op1) (gleanOperand op2)
gleanInstr (Mov os op1 op2)
    = instrInfoItems <> union (gleanOperand op1) (gleanOperand op2)
gleanInstr (Push os op1)
    = instrInfoItems <> gleanOperand op1
gleanInstr (Pop os op1)
    = instrInfoItems <> gleanOperand op1
gleanInstr (Xor os op1 op2)
    = instrInfoItems <> union (gleanOperand op1) (gleanOperand op2)
gleanInstr (Ret os) = [OperationSuffixes]
gleanInstr (NotImplementedMnemonic _) = []

gleanOperand :: Operand -> [InfoItemLabel]
gleanOperand (Reg r) = [Registers]
gleanOperand (Memory memOp) = [AddressOperandSyntax]
gleanOperand (Immediate lit) = []

gleanRegister :: Register -> [InfoItemLabel]
gleanRegister _ = []

gleanLiteral :: Literal -> [InfoItemLabel]
gleanLiteral _ = []
