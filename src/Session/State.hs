module Session.State where

import Data.Map.Strict

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

-- | Session Execution Response object
data SessionState = SessionState
    { source  :: String
    , target  :: String
    , target_with_meta :: Map Integer String
    } deriving (Show,Eq, Generic)

mkSessionState :: String -> String -> Map Integer String -> SessionState
mkSessionState = SessionState

instance ToJSON SessionState
instance FromJSON SessionState
