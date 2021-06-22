module Analysis.InfoItem where

import Protolude hiding (toList)
import Data.Aeson
import Data.Yaml

type InfoItem = (InfoItemLabel, InfoItemBody)
type InfoItemLookup = Map InfoItemLabel InfoItemBody

data InfoItemLabel
    = GeneralInfo
    | OperationSuffixes
    | OperandPrefixes
    | AddressOperandSyntax
    | Comments
    | Directives
    | Registers
    | Operands
    | InstrPtrRelAddr
    | Labels
    deriving (Show, Eq, Ord, Generic, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

newtype InfoItemBody = InfoItemBody Text deriving (Show, Eq, Generic, ToJSON, FromJSON)

_x86infoitems_fp :: FilePath
_x86infoitems_fp = "./src/Analysis/x86_info_items.yaml"

getInfoItemLookup :: IO InfoItemLookup
getInfoItemLookup = do
    res <- decodeFileEither _x86infoitems_fp
    case res of
        Left e -> panic ("getInfoItemLookup: " <> show e)
        Right r -> return r
