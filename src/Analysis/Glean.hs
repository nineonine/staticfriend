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
                InfoItem lbl (fromMaybe emptyInfoItemBody (M.lookup lbl infoItemLookup))
            ) (gleanFromInstr opcode)

gleanFromInstr :: Instruction -> [InfoItemLabel]
gleanFromInstr (Op o) = [GeneralInfo] <> gleanOp o
gleanFromInstr (Directive d _) = [Directives, DirectiveInfo d]
gleanFromInstr (Label _) = [Labels]
gleanFromInstr (Comment _) = [Comments]
gleanFromInstr (NotImplementedOpcode _) = []

instrInfoItems :: [InfoItemLabel]
instrInfoItems = [OperationSuffixes, OperandPrefixes]

gleanOp :: Operation -> [InfoItemLabel]
gleanOp (Add os op1 op2)
    = instrInfoItems <> union (gleanOperand op1) (gleanOperand op2)
gleanOp (Sub os op1 op2)
    = instrInfoItems <> union (gleanOperand op1) (gleanOperand op2)
gleanOp (Call os op1)
    = instrInfoItems <> gleanOperand op1
gleanOp (Lea os op1 op2)
    = instrInfoItems <> union (gleanOperand op1) (gleanOperand op2)
gleanOp (Mov os op1 op2)
    = instrInfoItems <> union (gleanOperand op1) (gleanOperand op2)
gleanOp (Push os op1)
    = instrInfoItems <> gleanOperand op1
gleanOp (Pop os op1)
    = instrInfoItems <> gleanOperand op1
gleanOp (Xor os op1 op2)
    = instrInfoItems <> union (gleanOperand op1) (gleanOperand op2)
gleanOp (Ret os) = [OperationSuffixes]
gleanOp (NotImplementedMnemonic _) = []

gleanOperand :: Operand -> [InfoItemLabel]
gleanOperand (Reg r) = [Registers]
gleanOperand (Memory memOp) = [AddressOperandSyntax]
gleanOperand (Immediate lit) = []

gleanRegister :: Register -> [InfoItemLabel]
gleanRegister _ = []

gleanLiteral :: Literal -> [InfoItemLabel]
gleanLiteral _ = []
