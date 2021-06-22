module Analysis.Glean where

import Protolude
import Data.Aeson
import qualified Data.IntMap.Strict as IntMap

import Analysis.InfoItem
import X86.AST

gleanInfoItems :: X86Program -> IntMap.IntMap [InfoItem]
gleanInfoItems prog = go IntMap.empty (zip [1..] prog)
    where
    go acc [] = acc
    go acc ((i,opcode):rest)
        | items <- gleanFromOpcode opcode
        = go (IntMap.insert i items acc) rest

gleanFromOpcode :: Opcode -> [InfoItem]
gleanFromOpcode (Instr _) = []
gleanFromOpcode (Directive _ _ ) = []
gleanFromOpcode (Label _) = []
gleanFromOpcode (Comment _) = []
gleanFromOpcode (NotImplementedOpcode _) = []
