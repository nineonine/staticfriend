module Session.Response where

import Protolude

import Data.Aeson (ToJSON)

import Analysis.InfoItem

-- | Session Execution Response object
data SessionState = SessionState
    { source   :: Text
    , target   :: Text
    , analysis :: InfoItemAnalysis
    , info_item_lookup :: InfoItemLookup
    } deriving (Show, Eq, Generic, ToJSON)

mkSessionState :: Text -> Text -> InfoItemAnalysis -> InfoItemLookup
               -> SessionState
mkSessionState = SessionState
