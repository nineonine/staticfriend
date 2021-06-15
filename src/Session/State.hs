module Session.State where

import Protolude
import Data.Map.Strict

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

-- | Session Execution Response object
data SessionState = SessionState
    { source  :: Text
    , target  :: Text
    , target_with_meta :: Map Integer Text
    } deriving (Show,Eq, Generic)

mkSessionState :: Text -> Text -> Map Integer Text -> SessionState
mkSessionState = SessionState

instance ToJSON SessionState
instance FromJSON SessionState
