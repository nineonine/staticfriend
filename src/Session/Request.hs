module Session.Request where

import Data.Aeson (FromJSON)
import GHC.Generics

data SessionRequest = SessionRequest
    { session_source       :: String
    , session_target       :: String
    , session_optimization :: String
    , session_program      :: String
    } deriving Generic

instance FromJSON SessionRequest
