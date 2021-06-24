module Analysis.InfoItem where

import Protolude hiding (toList)
import Data.Aeson
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Yaml

data InfoItem = InfoItem {
    ii_label :: !InfoItemLabel
  , ii_body  :: !InfoItemBody
  } deriving (Show, Eq, Generic, ToJSON)

type InfoItemLookup = Map InfoItemLabel InfoItemBody
type Source = Text

newtype InfoItemAnalysis = InfoItemAnalysis (Vector (Source, [InfoItem]))
    deriving (Show, Eq)
instance ToJSON InfoItemAnalysis where
    toJSON (InfoItemAnalysis m) = Array (V.map toJSONValue m)
        where toJSONValue (src, infoItems ) = object [
                  "source"     .= src
                , "info_items" .= infoItems]

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
