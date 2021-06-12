module Session.Request where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

data SessionRequest = SessionRequest
    { source_in          :: String
    , source_out         :: String
    , optimization_level :: String
    } deriving Generic

instance ToJSON SessionRequest
instance FromJSON SessionRequest
