module Analysis.InfoItem where

import Protolude

type InfoItem = (InfoItemLabel, InfoItemBody)
type InfoItemLookup = Map InfoItemLabel InfoItemBody

data InfoItemLabel
    = GeneralInfo
    | OperationSuffixes
    | OperandPrefixes
    | AddressOperandSyntax
    | Comments
    | Directives Text
    | Registers
    | Operands
    | InstrPtrRelAdd
    deriving (Show, Eq)

data InfoItemBody = InfoItemBody Text deriving (Show, Eq)
