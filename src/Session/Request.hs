module Session.Request where

import Protolude
import Data.Aeson (FromJSON)
import GHC.Generics

data SessionRequest = SessionRequest
    { session_source       :: Text
    , session_target       :: Text
    , session_optimization :: Text
    , session_program      :: Text
    } deriving Generic

instance FromJSON SessionRequest
