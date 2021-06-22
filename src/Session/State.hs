module Session.State where

import Protolude
import Data.Map.Strict

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

import Analysis.InfoItem

-- | Session Execution Response object
data SessionState = SessionState
    { source  :: Text
    , target  :: Text
    , target_with_meta :: Map Integer Text
    , info_item_lookup :: InfoItemLookup
    } deriving (Show,Eq, Generic, ToJSON)

mkSessionState :: Text -> Text -> Map Integer Text -> InfoItemLookup
               -> SessionState
mkSessionState = SessionState
