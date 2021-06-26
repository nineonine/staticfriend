module Analysis.InfoItem where

import Protolude hiding (toList)
import Protolude.Partial (read)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Yaml

_DIRECTIVE_PREFIX :: Text = "Directive."

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
    -- General
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
    -- Special
    | DirectiveInfo !Text -- ^ Directive name
    deriving (Show, Read, Eq, Ord, Generic, FromJSON)

instance ToJSONKey InfoItemLabel where
    toJSONKey = toJSONKeyText (\case
        DirectiveInfo dirname -> _DIRECTIVE_PREFIX <> dirname
        _else -> T.pack (show _else))

instance ToJSON InfoItemLabel where
    toJSON (DirectiveInfo dir) = String (_DIRECTIVE_PREFIX <> dir)
    toJSON _else = String (T.pack $ show _else)

instance FromJSONKey InfoItemLabel where
    fromJSONKey = FromJSONKeyText (\v ->
        if | _DIRECTIVE_PREFIX `T.isPrefixOf` v
           , dirname <- T.drop (T.length _DIRECTIVE_PREFIX) v
           -> DirectiveInfo dirname
           | otherwise
           -> read $ T.unpack v)

newtype InfoItemBody = InfoItemBody Text deriving (Show, Eq, Generic, ToJSON, FromJSON)

_x86infoitems_fp :: FilePath
_x86infoitems_fp = "./src/Analysis/x86_info_items.yaml"

getInfoItemLookup :: IO InfoItemLookup
getInfoItemLookup = do
    res <- decodeFileEither _x86infoitems_fp
    case res of
        Left e -> panic ("getInfoItemLookup: " <> show e)
        Right r -> return r
