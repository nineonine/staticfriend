module Analysis.InfoItem where

import Protolude hiding (toList)
import Protolude.Partial (read)
import Data.Aeson
import Data.HashMap.Strict hiding (mapMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
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
    | InstrPtrRelAdd
    deriving (Show, Read, Eq, Ord, Generic, ToJSON, ToJSONKey)

newtype InfoItemBody = InfoItemBody Text deriving (Show, Eq, Generic, ToJSON)

_x86infoitems_fp :: FilePath
_x86infoitems_fp = "./src/Analysis/x86_info_items.yaml"

getInfoItemLookup :: IO InfoItemLookup
getInfoItemLookup = do
    res <- decodeFileEither _x86infoitems_fp
    case res of
        Left e -> panic ("getInfoItemLookup: " <> show e)
        Right r -> case r of
            Object o -> return (go M.empty (toList o))
                where go acc [] = acc
                      go acc ((k,v):rest)
                           | String t <- v
                           , lbl <- read (T.unpack k)
                           = go (M.insert lbl (InfoItemBody t) acc) rest
                           | otherwise
                           = panic "getInfoItemLookup: unexpected"
