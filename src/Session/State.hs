module Session.State where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

-- | Session Execution Response object
data SessionState = SessionState
    { source  :: String
    , target  :: String
    } deriving (Show,Eq, Generic)

mkSessionState :: String -> String -> SessionState
mkSessionState = SessionState

instance ToJSON SessionState
instance FromJSON SessionState
